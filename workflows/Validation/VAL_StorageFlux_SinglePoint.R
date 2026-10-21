# VAL_StorageFlux_SinglePoint.R
#
# Single-point storage flux approximation for the three validation towers
# (SE-Sto, SE-Svb, US-Uaf), used in place of the multi-level column-
# integration method in flow.validation.storage.R for all three sites (not
# just the two where it fails -- see below).
#
# Run this after flow.validation.storage.R (it reads that script's
# SITE_storage_flux.csv output for dry_air_concentration and
# column_height_m).
#
# Why a single-point method instead of the existing multi-level one:
# flow.validation.storage.R's compute_eddy_storage_day() requires >=3 valid
# CH4 concentration levels per half-hour to integrate a column profile.
# Checking n_levels_used across the full record shows this is structurally
# impossible at two of the three towers, not a fixable QC threshold:
#   - SE-Sto (5 nominal heights, 0.43-3 m tower): n_levels_used never
#     exceeds 1 in 70,128 records -- CH4 is effectively measured at one
#     usable height.
#   - SE-Svb (16 nominal heights, 85 m mast): n_levels_used is 0 for ~99%
#     of 12,192 records -- CH4 concentration exists at only two heights
#     (85 m and 35 m, the "13_8" AA pair from 02_VAL_SensorHeightPairs.R
#     in lterwg-flux-gradient-methane).
#   - US-Uaf (4 heights, 1-8 m) does clear the 3-level bar often enough to
#     gapfill to full coverage under the existing method, but for one
#     consistent method across all three validation sites this script
#     applies the same single-point approximation everywhere:
#
#   S_CH4 = column_height_m * d(dry_air_concentration)/dt
#
# using the per-half-hour dry_air_concentration (nmol m-3) already reported
# in each SITE_storage_flux.csv, differenced against the previous half-hour.
# Gaps larger than 3600 s are not differenced (set to NA rather than
# extrapolated across a data gap). Sentinel/outlier concentration values are
# dropped using 1st/99th percentile bounds per site before differencing.
#
# SE-Sto note: an earlier run of this script found dry_air_concentration for
# SE-Sto ~100x smaller than SE-Svb/US-Uaf's (~800 vs ~80,000 nmol/m3,
# expected from ~1900-1950 ppb x ~42-43 mol/m3 dry air). That was traced to
# two raw-unit bugs in flow.SE-Sto.data.format.conc.diffs.R (P_kPa actually
# hPa; CH4 mixing ratio actually ppm) and is now corrected upstream in
# flow.validation.storage.R's apply_site_unit_corrections(). No correction
# is applied in this script -- it assumes correctly-scaled
# dry_air_concentration input from an up-to-date SITE_storage_flux.csv.
#
# Output: SITE_storage_flux_1pt.csv per site (time.rounded, storage_flux_1pt
# in nmol CH4 m-2 s-1) + one compiled Validation_storage_singlepoint.RData
# (list Validation_storage_singlepoint, by site), alongside the existing
# Storage_Flux/ output.

library(tidyverse)

site.list <- c('SE-Sto', 'SE-Svb', 'US-Uaf')

dir.create(fs::path(output_dir, "SinglePoint"), recursive = TRUE, showWarnings = FALSE)

Validation_storage_singlepoint <- list()

for (site in site.list) {

  message("Single-point storage: ", site)

  conc <- read.csv(fs::path(output_dir, paste0(site, "_storage_flux.csv"))) %>%
    filter(gas == "CH4") %>%
    mutate(time.rounded = as.POSIXct(time_halfhour_storage_end, tz = "UTC")) %>%
    filter(!is.na(dry_air_concentration)) %>%
    arrange(time.rounded)

  # Drop sentinel/outlier concentration values (1st/99th percentile bounds)
  bounds <- quantile(conc$dry_air_concentration, c(0.01, 0.99), na.rm = TRUE)
  conc <- conc %>%
    filter(dry_air_concentration >= bounds[1], dry_air_concentration <= bounds[2])

  conc <- conc %>%
    mutate(
      dt_s = as.numeric(difftime(time.rounded, lag(time.rounded), units = "secs")),
      dC   = dry_air_concentration - lag(dry_air_concentration),
      storage_flux_1pt = if_else(!is.na(dt_s) & dt_s > 0 & dt_s <= 3600,
                                  column_height_m * dC / dt_s, NA_real_)
    ) %>%
    filter(!is.na(storage_flux_1pt)) %>%
    select(time.rounded, storage_flux_1pt)

  Validation_storage_singlepoint[[site]] <- conc

  write.csv(conc,
            fs::path(output_dir, "SinglePoint", paste0(site, "_storage_flux_1pt.csv")),
            row.names = FALSE)

  message(" -> ", nrow(conc), " half-hours with a valid single-point storage estimate")
}

save(Validation_storage_singlepoint,
     file = fs::path(output_dir, "SinglePoint", "Validation_storage_singlepoint.RData"))

message("Done. Saved `Validation_storage_singlepoint` to ",
        fs::path(output_dir, "SinglePoint", "Validation_storage_singlepoint.RData"))
