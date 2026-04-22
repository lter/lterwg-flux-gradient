## --------------------------------------------- ##
#             NEON Storage Flux -----
## --------------------------------------------- ##
# Purpose:
# Estimate storage flux from NEON tower concentration profiles for CO2, H2O,
# and CH4 across all locally available NEON tower sites.
#
# This script expects the extracted site files created by
# `workflows/flow.neon.data.extract.R`:
#   data/SITE/SITE_1min.Rdata
#   data/SITE/SITE_9min.Rdata
#   data/SITE/SITE_attr.Rdata
#
# Storage is estimated as the vertically integrated rate of change in gas
# concentration from the ground to the top measurement height using
# layer-centered thicknesses:
#   F_storage = sum( n_air * dchi/dt * dz )
#
# where:
#   n_air = P / (R * T)  [mol m-3]
#   chi   = dry-air mixing ratio for each gas
#   dz    = layer thickness represented by each concentration level
#
# Units of `storage_flux` follow the native gas mixing-ratio scale:
#   CO2 = umol m-2 s-1
#   H2O = mmol m-2 s-1
#   CH4 = nmol m-2 s-1
#
# Output:
#   data/NEON_storage.RData
#   object `NEON_storage` in the workspace

rm(list = ls())

library(dplyr)
library(fs)
library(lubridate)
library(purrr)
library(tibble)

source(file.path("functions", "interp.flux.R"))


parse_neon_time <- function(x) {
  as.POSIXct(strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
}


safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}


safe_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}


native_units <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "umol m-2 s-1",
    gas == "H2O" ~ "mmol m-2 s-1",
    gas == "CH4" ~ "nmol m-2 s-1",
    TRUE ~ NA_character_
  )
}


get_site_list <- function(data_dir) {
  site_dirs <- fs::dir_ls(data_dir, type = "directory", recurse = FALSE)
  site_names <- basename(site_dirs)

  has_files <- file.exists(file.path(data_dir, site_names, paste0(site_names, "_1min.Rdata"))) &
    file.exists(file.path(data_dir, site_names, paste0(site_names, "_9min.Rdata"))) &
    file.exists(file.path(data_dir, site_names, paste0(site_names, "_attr.Rdata")))

  site_names[has_files]
}


prepare_pressure_series <- function(min1.list) {
  press <- min1.list$Press

  tibble(
    timeBgn = parse_neon_time(press$timeBgn),
    timeEnd = parse_neon_time(press$timeEnd),
    pressure_kPa = as.numeric(press$mean),
    qf = as.numeric(press$qfFinl)
  ) %>%
    mutate(pressure_kPa = ifelse(qf == 1, NA_real_, pressure_kPa)) %>%
    select(timeBgn, timeEnd, pressure_kPa) %>%
    arrange(timeBgn)
}


prepare_top_tair_series <- function(min1.list, attr.df) {
  tair <- min1.list$TAir
  top_level <- max(attr.df$TowerPosition, na.rm = TRUE)

  tibble(
    timeBgn = parse_neon_time(tair$timeBgn),
    timeEnd = parse_neon_time(tair$timeEnd),
    TowerPosition = as.numeric(tair$TowerPosition),
    tair_C = as.numeric(tair$mean),
    qf = as.numeric(tair$qfFinl)
  ) %>%
    filter(TowerPosition == top_level) %>%
    mutate(tair_C = ifelse(qf == 1, NA_real_, tair_C)) %>%
    select(timeBgn, timeEnd, tair_C) %>%
    arrange(timeBgn)
}


interpolate_molar_density <- function(time_pred, pressure_df, tair_df) {
  pressure_kPa <- interp.flux(
    timeBgn = pressure_df$timeBgn,
    timeEnd = pressure_df$timeEnd,
    flux = pressure_df$pressure_kPa,
    timePred = time_pred
  )

  tair_C <- interp.flux(
    timeBgn = tair_df$timeBgn,
    timeEnd = tair_df$timeEnd,
    flux = tair_df$tair_C,
    timePred = time_pred
  )

  tibble(
    timeMid = time_pred,
    pressure_kPa = pressure_kPa,
    tair_C = tair_C,
    molar_density_molm3 = (pressure_kPa * 1000) / (8.314462618 * (tair_C + 273.15))
  )
}


prepare_gas_profile <- function(gas_df, attr.df) {
  heights_df <- attr.df %>%
    transmute(
      TowerPosition = as.numeric(TowerPosition),
      TowerHeight_m = as.numeric(DistZaxsLvlMeasTow)
    )

  tibble(
    timeBgn = parse_neon_time(gas_df$timeBgn),
    timeEnd = parse_neon_time(gas_df$timeEnd),
    TowerPosition = as.numeric(gas_df$TowerPosition),
    mixing_ratio = as.numeric(gas_df$mean),
    qf = as.numeric(gas_df$qfFinl)
  ) %>%
    mutate(
      mixing_ratio = ifelse(qf == 1, NA_real_, mixing_ratio),
      timeMid = timeBgn + (timeEnd - timeBgn) / 2
    ) %>%
    left_join(heights_df, by = "TowerPosition") %>%
    filter(!is.na(TowerHeight_m)) %>%
    group_by(timeMid, timeBgn, timeEnd, TowerPosition, TowerHeight_m) %>%
    summarise(mixing_ratio = safe_mean(mixing_ratio), .groups = "drop") %>%
    arrange(TowerHeight_m, timeMid)
}


compute_layer_thickness <- function(heights_m) {
  heights_m <- sort(unique(heights_m))

  if (length(heights_m) == 0) {
    return(tibble(TowerHeight_m = numeric(), layer_thickness_m = numeric()))
  }

  if (length(heights_m) == 1) {
    return(tibble(
      TowerHeight_m = heights_m,
      layer_thickness_m = heights_m
    ))
  }

  boundaries_m <- c(
    0,
    (heights_m[-length(heights_m)] + heights_m[-1]) / 2,
    max(heights_m)
  )

  tibble(
    TowerHeight_m = heights_m,
    layer_thickness_m = diff(boundaries_m)
  )
}


compute_storage_flux_gas <- function(site, gas, min9.list, min1.list, attr.df) {
  gas_profile <- prepare_gas_profile(min9.list[[gas]], attr.df)

  if (nrow(gas_profile) == 0) {
    return(tibble())
  }

  thickness_df <- compute_layer_thickness(gas_profile$TowerHeight_m)

  gas_profile <- gas_profile %>%
    left_join(thickness_df, by = "TowerHeight_m") %>%
    group_by(TowerHeight_m) %>%
    arrange(timeMid, .by_group = TRUE) %>%
    mutate(
      dt_s = as.numeric(difftime(timeMid, lag(timeMid), units = "secs")),
      dchi_dt = (mixing_ratio - lag(mixing_ratio)) / dt_s
    ) %>%
    ungroup()

  pressure_df <- prepare_pressure_series(min1.list)
  tair_df <- prepare_top_tair_series(min1.list, attr.df)
  met_df <- interpolate_molar_density(sort(unique(gas_profile$timeMid)), pressure_df, tair_df)

  gas_profile <- gas_profile %>%
    left_join(met_df, by = "timeMid") %>%
    mutate(layer_storage_flux = molar_density_molm3 * dchi_dt * layer_thickness_m)

  gas_profile %>%
    group_by(timeMid) %>%
    summarise(
      site = first(site),
      gas = first(gas),
      timeBgn = min(timeBgn, na.rm = TRUE),
      timeEnd = max(timeEnd, na.rm = TRUE),
      time_halfhour = lubridate::round_date(first(timeMid), unit = "30 minutes"),
      pressure_kPa = safe_mean(pressure_kPa),
      tair_C = safe_mean(tair_C),
      top_height_m = max(TowerHeight_m, na.rm = TRUE),
      n_levels_total = n_distinct(TowerHeight_m),
      n_levels_used = sum(!is.na(layer_storage_flux)),
      storage_flux = safe_sum(layer_storage_flux),
      units = first(native_units(gas)),
      .groups = "drop"
    ) %>%
    mutate(
      storage_method = "profile dchi_dt integrated over layer-centered tower heights",
      molar_density_method = "P_over_RT"
    )
}


data_dir <- "data"
site.list <- get_site_list(data_dir)

if (length(site.list) == 0) {
  stop("No NEON site folders with _1min, _9min, and _attr files were found in `data/`.")
}

message("Estimating storage flux for ", length(site.list), " NEON tower sites...")

storage_list <- vector("list", length(site.list))
names(storage_list) <- site.list

for (site in site.list) {
  message("Working on ", site, "...")

  load(file.path(data_dir, site, paste0(site, "_1min.Rdata")))
  load(file.path(data_dir, site, paste0(site, "_9min.Rdata")))
  load(file.path(data_dir, site, paste0(site, "_attr.Rdata")))

  site_storage <- bind_rows(
    compute_storage_flux_gas(site, "CO2", min9.list, min1.list, attr.df),
    compute_storage_flux_gas(site, "H2O", min9.list, min1.list, attr.df),
    compute_storage_flux_gas(site, "CH4", min9.list, min1.list, attr.df)
  )

  storage_list[[site]] <- site_storage

  rm(min1.list, min9.list, attr.df)
}

NEON_storage <- bind_rows(storage_list) %>%
  arrange(site, gas, timeMid) %>%
  as_tibble()

save(NEON_storage, file = file.path(data_dir, "NEON_storage.RData"))

message("Done. Saved `NEON_storage` to ", file.path(data_dir, "NEON_storage.RData"))
