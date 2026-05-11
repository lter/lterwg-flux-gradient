## --------------------------------------------- ##
#             NEON Storage Flux -----
## --------------------------------------------- ##
# Purpose:
# Estimate storage flux from NEON aligned tower concentration files for CO2,
# H2O, and CH4 across all locally available NEON tower sites.
#
# This script expects the aligned concentration files created by
# `workflows/flow.neon.data.format.conc.diffs.R`:
#   DATA_DIR/SITE/SITE_aligned_conc_flux_9min.RData
#
# Storage is estimated with the eddy4R.stor storage workflow pattern
# documented for DP1.00099.001:
#   time synchronization -> dry-air concentration -> def.itpl.time() ->
#   def.time.rate.diff() -> def.itpl.spce() -> def.flux.stor()
#
#   F_storage = integral_z( d[n_air * chi(z)]/dt )
#
# where:
#   n_air = (P - e) / (R * T)  [mol dry air m-3]
#   e     = water vapor pressure from RH and saturation vapor pressure
#   chi   = dry-air mixing ratio for each gas
#
# Units of `storage_flux` follow the native gas mixing-ratio scale:
#   CO2 = umol m-2 s-1
#   H2O = mmol m-2 s-1
#   CH4 = nmol m-2 s-1
#
# Output:
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/SITE_storage_flux.csv
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/NEON_storage.RData
#   object `NEON_storage` in the workspace
#
# Optional command-line use:
#   Rscript workflows/flow.neon.storage.R [data_dir] [output_dir] [attr_data_dir]

library(dplyr)
library(fs)
library(lubridate)
library(tibble)


data_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Aligned_Concentrations"
output_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux"
attr_data_dir <- "/Volumes/MaloneLab/Research/FluxGradient/Attributes/data"
allow_eddy4r_stor_fallback <- identical(Sys.getenv("NEON_ALLOW_EDDY4R_STOR_FALLBACK"), "TRUE")
min_valid_levels <- 3L

args <- commandArgs(trailingOnly = TRUE)
script_file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_file_arg) > 0) {
  normalizePath(sub("^--file=", "", script_file_arg[[1]]), mustWork = FALSE)
} else {
  NA_character_
}
repo_dir <- if (!is.na(script_path)) {
  dirname(dirname(script_path))
} else {
  getwd()
}
local_eddy4r_stor_path <- file.path(repo_dir, "functions", "eddy4R.stor.local.R")

if (length(args) >= 1) {
  data_dir <- args[[1]]
}
if (length(args) >= 2) {
  output_dir <- args[[2]]
}
if (length(args) >= 3) {
  attr_data_dir <- args[[3]]
}


storage_framework <- function() {
  required_exports <- c(
    "def.itpl.time",
    "def.time.rate.diff",
    "def.itpl.spce",
    "def.flux.stor"
  )

  if (file.exists(local_eddy4r_stor_path)) {
    source(local_eddy4r_stor_path)
    missing_functions <- required_exports[!vapply(
      required_exports,
      exists,
      logical(1),
      mode = "function"
    )]

    if (length(missing_functions) > 0) {
      stop(
        "Local eddy4R.stor compatibility file is missing expected functions: ",
        paste(missing_functions, collapse = ", "),
        call. = FALSE
      )
    }

    if (requireNamespace("eddy4R.stor", quietly = TRUE)) {
      return(list(
        package_available = TRUE,
        label = paste0(
          "eddy4R.stor ",
          utils::packageVersion("eddy4R.stor"),
          " sequence via local compatibility wrappers"
        ),
        method = paste(
          "eddy4R.stor dp02-dp04 sequence:",
          "def.itpl.time, def.time.rate.diff, def.itpl.spce, def.flux.stor"
        )
      ))
    }

    return(list(
      package_available = FALSE,
      label = "local eddy4R.stor 0.0.1 compatibility functions",
      method = paste(
        "local eddy4R.stor-compatible dp02-dp04 sequence:",
        "def.itpl.time, def.time.rate.diff, def.itpl.spce, def.flux.stor"
      )
    ))
  }

  if (!requireNamespace("eddy4R.stor", quietly = TRUE)) {
    msg <- paste(
      "The eddy4R.stor package is required for production storage-flux runs.",
      "Install/load the NEON eddy4R.stor framework, add",
      "`functions/eddy4R.stor.local.R`, or set NEON_ALLOW_EDDY4R_STOR_FALLBACK=TRUE",
      "for a local smoke test that uses the same column-burden algorithm but",
      "cannot call eddy4R.stor directly."
    )

    if (!allow_eddy4r_stor_fallback) {
      stop(msg, call. = FALSE)
    }

    warning(msg, call. = FALSE)
    return(list(
      package_available = FALSE,
      label = "eddy4R.stor workflow fallback; package unavailable",
      method = paste(
        "eddy4R.stor dp02-dp04 sequence requested, but fallback",
        "column-burden algorithm used because no framework was available"
      )
    ))
  }

  missing_exports <- setdiff(required_exports, getNamespaceExports("eddy4R.stor"))
  if (length(missing_exports) > 0) {
    stop(
      "Installed eddy4R.stor is missing expected storage functions: ",
      paste(missing_exports, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    package_available = TRUE,
    label = paste0("eddy4R.stor ", utils::packageVersion("eddy4R.stor")),
    method = paste(
      "eddy4R.stor dp02-dp04 pattern: def.itpl.time,",
      "def.time.rate.diff, def.itpl.spce, def.flux.stor"
    )
  )
}


coerce_neon_time <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

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


safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  stats::sd(x)
}


safe_sum_int <- function(x) {
  if (all(is.na(x))) {
    return(NA_integer_)
  }
  as.integer(sum(x, na.rm = TRUE))
}


standard_error_from_variance <- function(variance, num_samp) {
  variance <- as.numeric(variance)
  num_samp <- as.numeric(num_samp)
  se <- sqrt(variance / num_samp)
  se[is.na(variance) | is.na(num_samp) | num_samp <= 0 | variance < 0] <- NA_real_
  se
}


combine_standard_errors <- function(se) {
  se <- as.numeric(se)
  se <- se[!is.na(se)]
  if (length(se) == 0) {
    return(NA_real_)
  }
  sqrt(sum(se^2)) / length(se)
}


storage_uncertainty_units <- function(gas) {
  native_units(gas)
}


safe_min_time <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.POSIXct(NA, tz = "GMT"))
  }
  min(x)
}


safe_max_time <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.POSIXct(NA, tz = "GMT"))
  }
  max(x)
}


safe_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}


time_sync_offset_s <- function(gas) {
  dplyr::case_when(
    gas == "CH4" ~ 90,
    gas %in% c("CO2", "H2O") ~ 0,
    TRUE ~ 0
  )
}


level_tair_C <- function(gas_df, tower_position) {
  fallback <- if ("Tair_K" %in% names(gas_df)) {
    as.numeric(gas_df$Tair_K) - 273.15
  } else {
    rep(NA_real_, nrow(gas_df))
  }

  tair_C <- fallback
  valid_pos <- !is.na(tower_position)

  for (pos in unique(tower_position[valid_pos])) {
    col_name <- paste0("Tair", as.integer(pos))
    idx <- valid_pos & tower_position == pos

    if (col_name %in% names(gas_df)) {
      tair_C[idx] <- as.numeric(gas_df[[col_name]])[idx]
    }
  }

  tair_C
}


sat_vapor_pressure_kPa <- function(tair_C) {
  0.6112 * exp(17.67 * tair_C / (tair_C + 243.5))
}


dry_air_molar_density <- function(pressure_kPa, tair_K, rh_pct) {
  tair_C <- tair_K - 273.15
  e_kPa <- (rh_pct / 100) * sat_vapor_pressure_kPa(tair_C)
  density <- ((pressure_kPa - e_kPa) * 1000) / (8.314462618 * tair_K)
  density[pressure_kPa <= e_kPa] <- NA_real_
  density
}


native_units <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "umol m-2 s-1",
    gas == "H2O" ~ "W m-2",
    gas == "CH4" ~ "nmol m-2 s-1",
    TRUE ~ NA_character_
  )
}


computed_native_units <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "umol m-2 s-1",
    gas == "H2O" ~ "mmol m-2 s-1",
    gas == "CH4" ~ "nmol m-2 s-1",
    TRUE ~ NA_character_
  )
}


burden_units <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "umol m-2",
    gas == "H2O" ~ NA_character_,
    gas == "CH4" ~ "nmol m-2",
    TRUE ~ NA_character_
  )
}


concentration_units <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "umol m-3",
    gas == "H2O" ~ "mmol m-3",
    gas == "CH4" ~ "nmol m-3",
    TRUE ~ NA_character_
  )
}


existing_storage_column <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "FC_stor_interp",
    gas == "H2O" ~ "LE_stor_interp",
    TRUE ~ NA_character_
  )
}


existing_storage_description <- function(gas) {
  dplyr::case_when(
    gas == "CO2" ~ "NEON aligned FC_stor_interp",
    gas == "H2O" ~ "NEON aligned LE_stor_interp",
    TRUE ~ "computed from tower profile concentrations"
  )
}


aligned_input_path <- function(data_dir, site) {
  paths <- file.path(
    data_dir,
    site,
    paste0(site, "_aligned_conc_flux_9min", c(".RData", ".Rdata"))
  )

  paths[file.exists(paths)][[1]]
}


get_site_list <- function(data_dir) {
  site_dirs <- fs::dir_ls(data_dir, type = "directory", recurse = FALSE)
  site_names <- basename(site_dirs)

  has_files <- vapply(
    site_names,
    function(site) {
      any(file.exists(file.path(
        data_dir,
        site,
        paste0(site, "_aligned_conc_flux_9min", c(".RData", ".Rdata"))
      )))
    },
    logical(1)
  )

  site_names[has_files]
}


load_aligned_site <- function(data_dir, site) {
  env <- new.env(parent = emptyenv())
  load(aligned_input_path(data_dir, site), envir = env)

  if (!exists("min9Diff.list", envir = env)) {
    stop("No `min9Diff.list` object found for ", site, ".", call. = FALSE)
  }

  env$min9Diff.list
}


find_site_attr_file <- function(data_dir, site) {
  candidates <- c(
    file.path(attr_data_dir, site, paste0(site, "_attr.Rdata")),
    file.path(attr_data_dir, site, paste0(site, "_attr.RData")),
    file.path(data_dir, site, paste0(site, "_attr.Rdata")),
    file.path(data_dir, site, paste0(site, "_attr.RData")),
    file.path(dirname(data_dir), "data", site, paste0(site, "_attr.Rdata")),
    file.path(dirname(data_dir), "data", site, paste0(site, "_attr.RData")),
    file.path(repo_dir, "data", site, paste0(site, "_attr.Rdata")),
    file.path(repo_dir, "data", site, paste0(site, "_attr.RData"))
  )

  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    return(NA_character_)
  }
  existing[[1]]
}


metadata_from_attr_file <- function(attr_file) {
  env <- new.env(parent = emptyenv())
  load(attr_file, envir = env)

  if (!exists("attr.df", envir = env)) {
    return(NULL)
  }

  attr_df <- env$attr.df
  needed <- c("TowerPosition", "DistZaxsLvlMeasTow")

  if (!all(needed %in% names(attr_df))) {
    return(NULL)
  }

  tibble(
    TowerPosition = as.numeric(attr_df$TowerPosition),
    TowerHeight_m = as.numeric(attr_df$DistZaxsLvlMeasTow),
    TimeTube_s = if ("TimeTube" %in% names(attr_df)) {
      as.numeric(attr_df$TimeTube)
    } else {
      NA_real_
    },
    metadata_source = attr_file
  ) %>%
    filter(!is.na(TowerPosition), !is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


metadata_from_aligned <- function(min9Diff.list) {
  bind_rows(lapply(names(min9Diff.list), function(gas) {
    gas_df <- min9Diff.list[[gas]]

    if (is.null(gas_df) || nrow(gas_df) == 0) {
      return(tibble())
    }

    bind_rows(
      tibble(
        TowerPosition = as.numeric(gas_df$TowerPosition_A),
        TowerHeight_m = as.numeric(gas_df$TowerHeight_A)
      ),
      tibble(
        TowerPosition = as.numeric(gas_df$TowerPosition_B),
        TowerHeight_m = as.numeric(gas_df$TowerHeight_B)
      )
    )
  })) %>%
    filter(!is.na(TowerPosition), !is.na(TowerHeight_m)) %>%
    group_by(TowerPosition) %>%
    summarise(
      TowerHeight_m = median(TowerHeight_m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      TimeTube_s = NA_real_,
      metadata_source = "aligned_concentration_heights"
    ) %>%
    arrange(TowerHeight_m)
}


build_site_metadata <- function(site, data_dir, min9Diff.list) {
  attr_file <- find_site_attr_file(data_dir, site)
  meta <- NULL

  if (!is.na(attr_file)) {
    meta <- metadata_from_attr_file(attr_file)
  }

  if (is.null(meta) || nrow(meta) == 0) {
    meta <- metadata_from_aligned(min9Diff.list)
  }

  if (nrow(meta) == 0) {
    stop("Could not determine tower metadata for ", site, ".", call. = FALSE)
  }

  meta
}


extract_profile_side <- function(gas_df, side, site_metadata) {
  suffix <- paste0("_", side)
  tower_position <- as.numeric(gas_df[[paste0("TowerPosition", suffix)]])
  time_mid <- coerce_neon_time(gas_df$timeMid)
  time_sync <- if ("match_time" %in% names(gas_df)) {
    coerce_neon_time(gas_df$match_time)
  } else {
    rep(as.POSIXct(NA), nrow(gas_df))
  }
  tair_C <- level_tair_C(gas_df, tower_position)
  rh_pct <- if ("RH" %in% names(gas_df)) {
    as.numeric(gas_df$RH)
  } else {
    rep(NA_real_, nrow(gas_df))
  }
  level_meta <- site_metadata %>%
    select(TowerPosition, TowerHeight_m_meta = TowerHeight_m, TimeTube_s)
  time_tube_s <- level_meta$TimeTube_s[match(tower_position, level_meta$TowerPosition)]
  tower_height_m <- level_meta$TowerHeight_m_meta[match(tower_position, level_meta$TowerPosition)]
  tower_height_m <- ifelse(
    is.na(tower_height_m),
    as.numeric(gas_df[[paste0("TowerHeight", suffix)]]),
    tower_height_m
  )

  tibble(
    timeBgn = coerce_neon_time(gas_df[[paste0("timeBgn", suffix)]]),
    timeEnd = coerce_neon_time(gas_df[[paste0("timeEnd", suffix)]]),
    timeMid = time_mid,
    time_sync = time_sync,
    time_sync_offset_s = as.numeric(difftime(time_sync, time_mid, units = "secs")),
    time_synchronization_source = ifelse(is.na(time_sync), NA_character_, "aligned_match_time"),
    TimeTube_s = time_tube_s,
    tube_time_source = ifelse(is.na(time_tube_s), "unavailable", "site_metadata_TimeTube"),
    TowerPosition = tower_position,
    TowerHeight_m = tower_height_m,
    mixing_ratio = as.numeric(gas_df[[paste0("mean", suffix)]]),
    mixing_ratio_variance = as.numeric(gas_df[[paste0("vari", suffix)]]),
    num_samp = as.numeric(gas_df[[paste0("numSamp", suffix)]]),
    qf = as.numeric(gas_df[[paste0("qfFinl", suffix)]]),
    pressure_kPa = as.numeric(gas_df$P_kPa),
    tair_C = tair_C,
    tair_K = tair_C + 273.15,
    rh_pct = rh_pct
  )
}


prepare_gas_profile <- function(gas_df, gas, site_metadata) {
  sync_offset_s <- time_sync_offset_s(gas)

  bind_rows(
    extract_profile_side(gas_df, "A", site_metadata),
    extract_profile_side(gas_df, "B", site_metadata)
  ) %>%
    mutate(
      mixing_ratio = ifelse(qf == 1, NA_real_, mixing_ratio),
      mixing_ratio_variance = ifelse(qf == 1, NA_real_, mixing_ratio_variance),
      mixing_ratio_uncertainty = standard_error_from_variance(mixing_ratio_variance, num_samp),
      time_sync = ifelse(
        is.na(time_sync),
        timeMid + sync_offset_s,
        time_sync
      ),
      time_sync = as.POSIXct(time_sync, origin = "1970-01-01", tz = "GMT"),
      time_sync = ifelse(
        is.na(TimeTube_s),
        time_sync,
        time_sync - TimeTube_s
      ),
      time_sync = as.POSIXct(time_sync, origin = "1970-01-01", tz = "GMT"),
      time_sync_offset_s = ifelse(
        is.na(time_sync_offset_s),
        sync_offset_s,
        time_sync_offset_s
      ),
      time_sync_offset_s = time_sync_offset_s - ifelse(is.na(TimeTube_s), 0, TimeTube_s),
      time_halfhour = lubridate::round_date(time_sync, unit = "30 minutes"),
      water_vapor_pressure_kPa = (rh_pct / 100) * sat_vapor_pressure_kPa(tair_C),
      dry_air_molar_density_molm3 = dry_air_molar_density(pressure_kPa, tair_K, rh_pct),
      dry_air_concentration = dry_air_molar_density_molm3 * mixing_ratio,
      dry_air_concentration_uncertainty = dry_air_molar_density_molm3 * mixing_ratio_uncertainty,
      time_synchronization_source = ifelse(
        is.na(time_synchronization_source),
        "fallback_gas_offset",
        "aligned_match_time"
      )
    ) %>%
    filter(!is.na(TowerHeight_m), !is.na(time_halfhour)) %>%
    group_by(time_sync, time_halfhour, TowerPosition, TowerHeight_m) %>%
    summarise(
      timeBgn = min(timeBgn, na.rm = TRUE),
      timeEnd = max(timeEnd, na.rm = TRUE),
      mixing_ratio = safe_mean(mixing_ratio),
      mixing_ratio_variance = safe_mean(mixing_ratio_variance),
      num_samp = safe_sum_int(num_samp),
      mixing_ratio_uncertainty = combine_standard_errors(mixing_ratio_uncertainty),
      pressure_kPa = safe_mean(pressure_kPa),
      tair_C = safe_mean(tair_C),
      rh_pct = safe_mean(rh_pct),
      water_vapor_pressure_kPa = safe_mean(water_vapor_pressure_kPa),
      dry_air_molar_density_molm3 = safe_mean(dry_air_molar_density_molm3),
      dry_air_concentration = safe_mean(dry_air_concentration),
      dry_air_concentration_uncertainty = combine_standard_errors(dry_air_concentration_uncertainty),
      time_sync_offset_s = safe_first(time_sync_offset_s),
      time_synchronization_source = safe_first(time_synchronization_source),
      TimeTube_s = safe_mean(TimeTube_s),
      tube_time_source = safe_first(tube_time_source),
      .groups = "drop"
    ) %>%
    arrange(time_halfhour, TowerHeight_m)
}


eddy_rate_name <- function(gas) {
  paste0("rate", capitalize(tolower(gas)))
}


extract_rate_df <- function(rate_list, gas) {
  rate_name <- eddy_rate_name(gas)

  bind_rows(lapply(rate_list, function(x) {
    tibble(
      timeBgn = as.POSIXct(
        x$timeBgn[[rate_name]],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "GMT"
      ),
      timeEnd = as.POSIXct(x$timeEnd[[rate_name]], tz = "GMT"),
      rate = as.numeric(x$mean[[rate_name]])
    )
  }))
}


rate_uncertainty_df <- function(interpolated_uncertainty, date, rate_window_s,
                                rate_interval_s, num_date = 1L) {
  set_data <- def.idx.diff(
    PrdWndwAgr = rate_window_s,
    PrdIncrAgr = rate_interval_s,
    numDate = num_date
  )

  time_out <- as.POSIXlt(seq.POSIXt(
    from = as.POSIXlt(
      paste(date, " 00:00:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    to = as.POSIXlt(
      paste(date, " 23:59:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    by = 60
  ), tz = "UTC")

  bind_rows(lapply(seq_len(length(set_data$Bgn) - 1), function(idx_agr) {
    bgn_next <- set_data$Bgn[idx_agr + 1]
    end_next <- set_data$End[idx_agr + 1]
    bgn_now <- set_data$Bgn[idx_agr]
    end_now <- set_data$End[idx_agr]

    next_uncertainty <- interpolated_uncertainty[bgn_next:end_next]
    now_uncertainty <- interpolated_uncertainty[bgn_now:end_now]
    next_uncertainty <- next_uncertainty[!is.na(next_uncertainty)]
    now_uncertainty <- now_uncertainty[!is.na(now_uncertainty)]

    next_mean_uncertainty <- if (length(next_uncertainty) == 0) {
      NA_real_
    } else {
      sqrt(sum(next_uncertainty^2)) / length(next_uncertainty)
    }
    now_mean_uncertainty <- if (length(now_uncertainty) == 0) {
      NA_real_
    } else {
      sqrt(sum(now_uncertainty^2)) / length(now_uncertainty)
    }

    rate_uncertainty <- sqrt(next_mean_uncertainty^2 + now_mean_uncertainty^2) / rate_interval_s

    tibble(
      timeBgn = as.POSIXct(
        format(time_out[set_data$End[idx_agr] - 1], format = "%Y-%m-%d %H:%M:%S"),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "GMT"
      ),
      timeEnd = as.POSIXct(time_out[(set_data$End[idx_agr] - 1) + (rate_interval_s / 60 - 1)] + 59, tz = "GMT"),
      rate_uncertainty = rate_uncertainty
    )
  }))
}


profile_level_rate <- function(level_df, gas, wndw_max_s, rate_window_s,
                               rate_interval_s, num_date = 1L) {
  level_df <- level_df %>%
    filter(!is.na(time_sync), !is.na(dry_air_concentration)) %>%
    arrange(time_sync)

  if (nrow(level_df) == 0) {
    return(tibble())
  }

  date <- as.Date(level_df$time_sync[[1]], tz = "GMT")
  data_inp <- tibble(
    timeBgn = format(level_df$time_sync, "%Y-%m-%dT%H:%M:%SZ", tz = "GMT"),
    numSamp = 0,
    mean = level_df$dry_air_concentration
  )
  uncertainty_inp <- tibble(
    timeBgn = format(level_df$time_sync, "%Y-%m-%dT%H:%M:%SZ", tz = "GMT"),
    numSamp = 0,
    mean = level_df$dry_air_concentration_uncertainty
  )

  interpolated <- def.itpl.time(
    dataInp = data_inp,
    methItpl = "linear",
    WndwMax = wndw_max_s
  )
  interpolated_uncertainty <- def.itpl.time(
    dataInp = uncertainty_inp,
    methItpl = "linear",
    WndwMax = wndw_max_s
  )

  rate_list <- def.time.rate.diff(
    dataInp = interpolated,
    numDate = num_date,
    PrdWndwAgr = rate_window_s,
    PrdIncrAgr = rate_interval_s,
    Date = format(date, "%Y-%m-%d"),
    idxVar = tolower(gas)
  )
  rate_uncertainty <- rate_uncertainty_df(
    interpolated_uncertainty = interpolated_uncertainty,
    date = format(date, "%Y-%m-%d"),
    rate_window_s = rate_window_s,
    rate_interval_s = rate_interval_s,
    num_date = num_date
  )

  extract_rate_df(rate_list, gas) %>%
    left_join(rate_uncertainty, by = c("timeBgn", "timeEnd")) %>%
    mutate(
      time_halfhour = lubridate::ceiling_date(timeEnd + 1, unit = "30 minutes"),
      interpolation_gap_s = vapply(
        time_halfhour,
        function(x) {
          min(abs(as.numeric(difftime(level_df$time_sync, x, units = "secs"))), na.rm = TRUE)
        },
        numeric(1)
      ),
      TowerPosition = level_df$TowerPosition[[1]],
      TowerHeight_m = level_df$TowerHeight_m[[1]]
    )
}


compute_eddy_storage_day <- function(day_profile, gas, levels_df, reso_spce_out_m,
                                     wndw_max_s, rate_window_s,
                                     rate_interval_s, min_valid_levels) {
  if (nrow(levels_df) == 0) {
    return(tibble())
  }

  rate_df <- day_profile %>%
    group_by(TowerPosition, TowerHeight_m) %>%
    group_split() %>%
    lapply(
      profile_level_rate,
      gas = gas,
      wndw_max_s = wndw_max_s,
      rate_window_s = rate_window_s,
      rate_interval_s = rate_interval_s
    ) %>%
    bind_rows()

  if (nrow(rate_df) == 0) {
    return(tibble())
  }

  time_grid <- rate_df %>%
    distinct(time_halfhour, timeBgn, timeEnd) %>%
    arrange(time_halfhour)

  rate_wide <- time_grid
  rate_matrix <- matrix(
    NA_real_,
    nrow = nrow(time_grid),
    ncol = nrow(levels_df),
    dimnames = list(NULL, as.character(levels_df$TowerHeight_m))
  )
  rate_uncertainty_matrix <- rate_matrix
  gap_matrix <- rate_matrix

  for (idx_level in seq_len(nrow(levels_df))) {
    level_rates <- rate_df %>%
      filter(
        TowerPosition == levels_df$TowerPosition[[idx_level]],
        TowerHeight_m == levels_df$TowerHeight_m[[idx_level]]
      ) %>%
      select(time_halfhour, rate, rate_uncertainty, interpolation_gap_s)

    level_joined <- time_grid %>%
      left_join(level_rates, by = "time_halfhour") %>%
      select(rate, rate_uncertainty, interpolation_gap_s)

    rate_matrix[, idx_level] <- level_joined$rate
    rate_uncertainty_matrix[, idx_level] <- level_joined$rate_uncertainty
    gap_matrix[, idx_level] <- level_joined$interpolation_gap_s
  }

  valid_level_count <- rowSums(!is.na(rate_matrix))
  max_interpolation_gap_s <- apply(gap_matrix, 1, function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    max(x, na.rm = TRUE)
  })

  vertical_rates <- lapply(seq_len(nrow(rate_matrix)), function(i) {
    if (valid_level_count[[i]] < min_valid_levels) {
      n_grid <- floor(max(levels_df$TowerHeight_m, na.rm = TRUE) / reso_spce_out_m)
      return(rep(NA_real_, n_grid))
    }

    def.itpl.spce(
      dataInp = rate_matrix[i, ],
      methItpl = "linear",
      resoSpceOut = reso_spce_out_m,
      lvlTowr = levels_df$TowerHeight_m
    )
  })
  vertical_rate_uncertainties <- lapply(seq_len(nrow(rate_uncertainty_matrix)), function(i) {
    if (valid_level_count[[i]] < min_valid_levels) {
      n_grid <- floor(max(levels_df$TowerHeight_m, na.rm = TRUE) / reso_spce_out_m)
      return(rep(NA_real_, n_grid))
    }

    def.itpl.spce(
      dataInp = rate_uncertainty_matrix[i, ],
      methItpl = "linear",
      resoSpceOut = reso_spce_out_m,
      lvlTowr = levels_df$TowerHeight_m
    )
  })

  vertical_rates <- do.call(rbind, vertical_rates)
  vertical_rate_uncertainties <- do.call(rbind, vertical_rate_uncertainties)
  vertical_rates <- as.data.frame(vertical_rates)
  names(vertical_rates) <- paste0("z_", seq_len(ncol(vertical_rates)))
  vertical_rates$timeBgn <- time_grid$timeBgn
  vertical_rates$timeEnd <- time_grid$timeEnd

  stor <- def.flux.stor(vertical_rates, lvlTowr = levels_df$TowerHeight_m)
  dz_m <- max(levels_df$TowerHeight_m, na.rm = TRUE) / ncol(vertical_rate_uncertainties)
  storage_flux_uncertainty <- apply(vertical_rate_uncertainties, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_real_)
    }
    sqrt(sum((x * dz_m)^2))
  })
  few_levels_qf <- valid_level_count < min_valid_levels
  large_gap_qf <- !is.na(max_interpolation_gap_s) & max_interpolation_gap_s > wndw_max_s
  storage_qf <- as.integer(few_levels_qf | large_gap_qf)
  qf_reason <- ifelse(few_levels_qf, paste0("fewer_than_", min_valid_levels, "_valid_profile_levels"), "ok")
  qf_reason <- ifelse(large_gap_qf & qf_reason == "ok", "large_time_interpolation_gap", qf_reason)
  qf_reason <- ifelse(
    large_gap_qf & qf_reason != "ok" & qf_reason != "large_time_interpolation_gap",
    paste(qf_reason, "large_time_interpolation_gap", sep = ";"),
    qf_reason
  )

  tibble(
    timeBgn = as.POSIXct(stor$timeBgn, tz = "GMT"),
    timeEnd = as.POSIXct(stor$timeEnd, tz = "GMT"),
    time_halfhour = time_grid$time_halfhour,
    storage_flux = ifelse(storage_qf == 1, NA_real_, as.numeric(stor$mean)),
    storage_flux_uncertainty = ifelse(storage_qf == 1, NA_real_, storage_flux_uncertainty),
    storage_qf = storage_qf,
    qf_reason = qf_reason,
    n_levels_total = nrow(levels_df),
    n_levels_used = valid_level_count,
    max_interpolation_gap_s = max_interpolation_gap_s,
    bottom_height_m = min(levels_df$TowerHeight_m, na.rm = TRUE),
    top_height_m = max(levels_df$TowerHeight_m, na.rm = TRUE),
    column_height_m = max(levels_df$TowerHeight_m, na.rm = TRUE),
    vertical_grid_cells = ncol(vertical_rates) - 2
  )
}


compute_existing_storage_gas <- function(site, gas, gas_df, gas_profile,
                                         profile_meta, site_metadata,
                                         framework_info) {
  site_name <- site
  gas_name <- gas
  storage_col <- existing_storage_column(gas)
  storage_col_name <- storage_col

  if (is.na(storage_col) || !storage_col %in% names(gas_df)) {
    return(NULL)
  }

  existing_storage <- tibble(
    time_halfhour = lubridate::round_date(coerce_neon_time(gas_df$timeMid), unit = "30 minutes"),
    storage_flux_source = as.numeric(gas_df[[storage_col]])
  ) %>%
    filter(!is.na(time_halfhour)) %>%
    group_by(time_halfhour) %>%
    summarise(
      storage_flux = safe_mean(storage_flux_source),
      storage_source_n = sum(!is.na(storage_flux_source)),
      storage_flux_uncertainty = {
        n_valid <- sum(!is.na(storage_flux_source))
        if (n_valid < 2) {
          NA_real_
        } else {
          safe_sd(storage_flux_source) / sqrt(n_valid)
        }
      },
      .groups = "drop"
    ) %>%
    mutate(
      storage_qf = as.integer(is.na(storage_flux)),
      qf_reason = ifelse(storage_qf == 1, paste0("missing_", storage_col_name), "ok"),
      timeBgn = time_halfhour,
      timeEnd = time_halfhour + lubridate::minutes(30),
      time_halfhour_storage_end = timeEnd,
      timeMid = time_halfhour,
      site = site_name,
      gas = gas_name,
      dt_s = 30 * 60,
      storage_flux_column_rate = NA_real_,
      storage_flux_ameriflux_sign = NA_real_,
      storage_flux_uncertainty_method = paste0(
        "standard error of ",
        storage_col_name,
        " values aggregated to each half-hour; NEON source storage uncertainty not present in aligned file"
      ),
      storage_flux_uncertainty_units = storage_uncertainty_units(gas_name),
      column_burden = NA_real_,
      units = native_units(gas_name),
      burden_units = burden_units(gas_name),
      concentration_units = concentration_units(gas_name),
      storage_source_column = storage_col_name,
      storage_value_source = existing_storage_description(gas_name)
    )

  if (identical(gas_name, "CO2")) {
    existing_storage$storage_flux_column_rate <- existing_storage$storage_flux
  }
  if (gas_name %in% c("CO2", "CH4")) {
    existing_storage$storage_flux_ameriflux_sign <- -existing_storage$storage_flux
  }

  level_summary <- site_metadata %>%
    summarise(
      bottom_height_m = min(TowerHeight_m, na.rm = TRUE),
      top_height_m = max(TowerHeight_m, na.rm = TRUE),
      column_height_m = max(TowerHeight_m, na.rm = TRUE),
      n_levels_total = n(),
      .groups = "drop"
    )

  n_levels_by_time <- gas_profile %>%
    group_by(time_halfhour) %>%
    summarise(
      n_levels_used = n_distinct(TowerPosition[!is.na(dry_air_concentration)]),
      max_interpolation_gap_s = NA_real_,
      vertical_grid_cells = NA_integer_,
      .groups = "drop"
    )

  existing_storage %>%
    left_join(profile_meta, by = "time_halfhour") %>%
    left_join(n_levels_by_time, by = "time_halfhour") %>%
    mutate(
      bottom_height_m = level_summary$bottom_height_m,
      top_height_m = level_summary$top_height_m,
      column_height_m = level_summary$column_height_m,
      n_levels_total = level_summary$n_levels_total
    ) %>%
    select(
      timeMid,
      site,
      gas,
      timeBgn,
      timeEnd,
      time_halfhour,
      time_halfhour_storage_end,
      pressure_kPa,
      tair_C,
      rh_pct,
      water_vapor_pressure_kPa,
      dry_air_molar_density_molm3,
      dry_air_concentration,
      dry_air_concentration_uncertainty,
      concentration_units,
      time_sync_offset_s,
      time_synchronization_source,
      TimeTube_s,
      tube_time_source,
      bottom_height_m,
      top_height_m,
      column_height_m,
      n_levels_total,
      n_levels_used,
      n_density_levels,
      max_interpolation_gap_s,
      vertical_grid_cells,
      storage_qf,
      qf_reason,
      column_burden,
      burden_units,
      dt_s,
      storage_flux_column_rate,
      storage_flux,
      storage_flux_uncertainty,
      storage_flux_uncertainty_units,
      storage_flux_uncertainty_method,
      storage_source_n,
      storage_flux_ameriflux_sign,
      units,
      storage_source_column,
      storage_value_source
    ) %>%
    mutate(
      storage_framework = framework_info$label,
      storage_method = paste("pulled from", existing_storage_description(gas_name), "in aligned concentration file"),
      time_synchronization_method = paste(
        "existing aligned storage term used directly;",
        "time_halfhour is rounded NEON timeMid"
      ),
      time_interpolation_method = "existing NEON aligned storage interpolation used directly",
      rate_calculation_method = "existing NEON aligned storage term used directly",
      vertical_interpolation_method = "existing NEON aligned storage term used directly",
      minimum_valid_levels = NA_integer_,
      site_tower_metadata_source = paste(unique(site_metadata$metadata_source), collapse = ";"),
      molar_density_method = ifelse(gas == "CO2", "embedded in existing NEON FC_stor_interp", NA_character_),
      density_correction = ifelse(gas == "CO2", "embedded in existing NEON FC_stor_interp", NA_character_),
      storage_flux_sign_convention = paste(
        "storage_flux is the native source value;",
        "storage_flux_ameriflux_sign is provided for CO2/CH4 correction-sign comparisons"
      ),
      storage_flux_time_convention = paste(
        "timeMid/time_halfhour label interval start;",
        "time_halfhour_storage_end preserves interval end"
      )
    )
}


compute_storage_flux_gas <- function(site, gas, min9Diff.list, site_metadata, framework_info) {
  site_name <- site
  gas_name <- gas

  if (!gas %in% names(min9Diff.list) ||
      is.null(min9Diff.list[[gas]]) ||
      nrow(min9Diff.list[[gas]]) == 0) {
    warning("Skipping ", site, " ", gas, ": aligned gas data not found.", call. = FALSE)
    return(tibble())
  }

  gas_profile <- prepare_gas_profile(min9Diff.list[[gas]], gas, site_metadata)

  if (nrow(gas_profile) == 0) {
    return(tibble())
  }

  profile_meta <- gas_profile %>%
    group_by(time_halfhour) %>%
    summarise(
      pressure_kPa = safe_mean(pressure_kPa),
      tair_C = safe_mean(tair_C),
      rh_pct = safe_mean(rh_pct),
      water_vapor_pressure_kPa = safe_mean(water_vapor_pressure_kPa),
      dry_air_molar_density_molm3 = safe_mean(dry_air_molar_density_molm3),
      dry_air_concentration = safe_mean(dry_air_concentration),
      dry_air_concentration_uncertainty = combine_standard_errors(dry_air_concentration_uncertainty),
      n_density_levels = n_distinct(TowerPosition[!is.na(dry_air_molar_density_molm3)]),
      time_sync_offset_s = safe_first(time_sync_offset_s),
      time_synchronization_source = safe_first(time_synchronization_source),
      TimeTube_s = safe_mean(TimeTube_s),
      tube_time_source = safe_first(tube_time_source),
      .groups = "drop"
    )

  existing_storage <- compute_existing_storage_gas(
    site = site,
    gas = gas,
    gas_df = min9Diff.list[[gas]],
    gas_profile = gas_profile,
    profile_meta = profile_meta,
    site_metadata = site_metadata,
    framework_info = framework_info
  )

  if (!is.null(existing_storage)) {
    return(existing_storage)
  }

  fixed_levels_df <- site_metadata %>%
    select(TowerPosition, TowerHeight_m) %>%
    filter(!is.na(TowerPosition), !is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)

  reso_spce_out_m <- 0.1
  wndw_max_s <- 40 * 60
  rate_window_s <- 4 * 60
  rate_interval_s <- 30 * 60

  storage_flux <- gas_profile %>%
    mutate(profile_date = as.Date(time_sync, tz = "GMT")) %>%
    filter(!is.na(profile_date)) %>%
    group_by(profile_date) %>%
    group_split() %>%
    lapply(
      compute_eddy_storage_day,
      gas = gas,
      levels_df = fixed_levels_df,
      reso_spce_out_m = reso_spce_out_m,
      wndw_max_s = wndw_max_s,
      rate_window_s = rate_window_s,
      rate_interval_s = rate_interval_s,
      min_valid_levels = min_valid_levels
    ) %>%
    bind_rows() %>%
    filter(!is.na(time_halfhour)) %>%
    arrange(time_halfhour)

  if (nrow(storage_flux) == 0) {
    return(tibble())
  }

  computed_storage <- storage_flux %>%
    left_join(profile_meta, by = "time_halfhour") %>%
    mutate(
      missing_density_qf = is.na(n_density_levels) | n_density_levels < min_valid_levels,
      qf_reason = ifelse(
        missing_density_qf & qf_reason == "ok",
        "missing_dry_air_density",
        qf_reason
      ),
      qf_reason = ifelse(
        missing_density_qf & qf_reason != "missing_dry_air_density" & qf_reason != "ok",
        paste(qf_reason, "missing_dry_air_density", sep = ";"),
        qf_reason
      ),
      storage_qf = as.integer(storage_qf == 1 | missing_density_qf),
      storage_flux_column_rate = ifelse(storage_qf == 1, NA_real_, storage_flux),
      storage_flux = storage_flux_column_rate,
      storage_flux_ameriflux_sign = NA_real_,
      storage_flux_uncertainty = ifelse(storage_qf == 1, NA_real_, storage_flux_uncertainty),
      storage_flux_uncertainty_units = storage_uncertainty_units(gas_name),
      storage_flux_uncertainty_method = paste(
        "propagated standard uncertainty from concentration variance and numSamp;",
        "time interpolation, finite-difference rate uncertainty, and vertical integration"
      ),
      storage_source_n = NA_integer_,
      time_halfhour_storage_end = time_halfhour,
      time_halfhour_storage_start = timeBgn,
      timeMid = time_halfhour_storage_start,
      time_halfhour = time_halfhour_storage_start,
      site = site_name,
      gas = gas_name,
      dt_s = rate_interval_s,
      column_burden = NA_real_,
      units = computed_native_units(gas_name),
      burden_units = burden_units(gas_name),
      concentration_units = concentration_units(gas_name),
      storage_source_column = NA_character_,
      storage_value_source = existing_storage_description(gas_name)
    ) %>%
    select(
      timeMid,
      site,
      gas,
      timeBgn,
      timeEnd,
      time_halfhour,
      time_halfhour_storage_end,
      pressure_kPa,
      tair_C,
      rh_pct,
      water_vapor_pressure_kPa,
      dry_air_molar_density_molm3,
      dry_air_concentration,
      dry_air_concentration_uncertainty,
      concentration_units,
      time_sync_offset_s,
      time_synchronization_source,
      TimeTube_s,
      tube_time_source,
      bottom_height_m,
      top_height_m,
      column_height_m,
      n_levels_total,
      n_levels_used,
      n_density_levels,
      max_interpolation_gap_s,
      vertical_grid_cells,
      storage_qf,
      qf_reason,
      column_burden,
      burden_units,
      dt_s,
      storage_flux_column_rate,
      storage_flux,
      storage_flux_uncertainty,
      storage_flux_uncertainty_units,
      storage_flux_uncertainty_method,
      storage_source_n,
      storage_flux_ameriflux_sign,
      units,
      storage_source_column,
      storage_value_source
    ) %>%
    mutate(
      storage_framework = framework_info$label,
      storage_method = framework_info$method,
      time_synchronization_method = paste(
        "NEON corrected match_time from aligned concentrations used when present;",
        "otherwise CH4 timeMid +90 s and CO2/H2O unchanged;",
        "TimeTube transit time is subtracted when site metadata are available"
      ),
      time_interpolation_method = paste0("def.itpl.time linear interpolation; WndwMax = ", wndw_max_s, " s"),
      rate_calculation_method = paste0(
        "def.time.rate.diff using ",
        rate_window_s,
        " s windows and ",
        rate_interval_s,
        " s increments"
      ),
      vertical_interpolation_method = paste0(
        "def.itpl.spce linear interpolation to ",
        reso_spce_out_m,
        " m fixed site-level vertical grid; def.flux.stor integrates over fixed tower height"
      ),
      minimum_valid_levels = min_valid_levels,
      site_tower_metadata_source = paste(unique(site_metadata$metadata_source), collapse = ";"),
      molar_density_method = "dry_air_density_(P_minus_e)_over_RT_from_RH_level_Tair_pressure",
      density_correction = "dry-air density correction applied using RH-derived water vapor pressure and level-specific Tair",
      storage_flux_sign_convention = paste(
        "storage_flux is the computed native column-rate value;",
        "storage_flux_ameriflux_sign is provided for CO2/CH4 correction-sign comparisons"
      ),
      storage_flux_time_convention = paste(
        "timeMid/time_halfhour label interval start to match NEON FC_stor_interp;",
        "time_halfhour_storage_end preserves the interval-end label"
      )
    )

  if (gas_name %in% c("CO2", "CH4")) {
    computed_storage$storage_flux_ameriflux_sign <- -computed_storage$storage_flux_column_rate
  }

  computed_storage
}


fill_first_nonmissing <- function(x) {
  if (is.character(x)) {
    valid <- x[!is.na(x) & x != ""]
    if (length(valid) == 0) {
      return(x)
    }
    x[is.na(x) | x == ""] <- valid[[1]]
    return(x)
  }

  valid <- x[!is.na(x)]
  if (length(valid) == 0) {
    return(x)
  }
  x[is.na(x)] <- valid[[1]]
  x
}


fill_constant_metadata <- function(gas_storage) {
  fill_cols <- intersect(
    c(
      "dt_s",
      "units",
      "burden_units",
      "concentration_units",
      "storage_flux_uncertainty_units",
      "storage_source_column",
      "storage_value_source",
      "storage_framework",
      "storage_method",
      "time_synchronization_method",
      "time_interpolation_method",
      "rate_calculation_method",
      "vertical_interpolation_method",
      "minimum_valid_levels",
      "site_tower_metadata_source",
      "molar_density_method",
      "density_correction",
      "storage_flux_sign_convention",
      "storage_flux_time_convention",
      "storage_flux_uncertainty_method",
      "bottom_height_m",
      "top_height_m",
      "column_height_m",
      "n_levels_total"
    ),
    names(gas_storage)
  )

  for (col_name in fill_cols) {
    gas_storage[[col_name]] <- fill_first_nonmissing(gas_storage[[col_name]])
  }

  gas_storage
}


short_linear_gapfill <- function(x, max_gap = 2L) {
  filled <- x
  idx <- seq_along(x)
  ok <- !is.na(x)

  if (sum(ok) < 2) {
    return(filled)
  }

  interpolated <- stats::approx(
    x = idx[ok],
    y = x[ok],
    xout = idx,
    rule = 1,
    ties = mean
  )$y

  runs <- rle(is.na(x))
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1

  for (run_idx in seq_along(runs$values)) {
    if (!runs$values[[run_idx]] || runs$lengths[[run_idx]] > max_gap) {
      next
    }

    run_start <- starts[[run_idx]]
    run_end <- ends[[run_idx]]
    has_left <- run_start > 1 && !is.na(x[[run_start - 1]])
    has_right <- run_end < length(x) && !is.na(x[[run_end + 1]])

    if (has_left && has_right) {
      filled[run_start:run_end] <- interpolated[run_start:run_end]
    }
  }

  filled
}


gapfill_storage_series <- function(gas_storage, window_days = 30L, short_gap_max = 2L) {
  gas_storage <- gas_storage %>%
    arrange(time_halfhour)

  raw_flux <- gas_storage$storage_flux
  filled_flux <- raw_flux
  filled_uncertainty <- gas_storage$storage_flux_uncertainty
  gapfilled <- is.na(raw_flux)
  method <- ifelse(is.na(raw_flux), NA_character_, "observed")
  n_donor <- ifelse(is.na(raw_flux), NA_integer_, 0L)

  short_filled <- short_linear_gapfill(raw_flux, max_gap = short_gap_max)
  short_idx <- is.na(raw_flux) & !is.na(short_filled)

  if (any(short_idx)) {
    filled_flux[short_idx] <- short_filled[short_idx]
    uncertainty_linear <- short_linear_gapfill(filled_uncertainty, max_gap = short_gap_max)
    filled_uncertainty[short_idx] <- uncertainty_linear[short_idx]
    method[short_idx] <- paste0("linear_short_gap_max_", short_gap_max, "_halfhours")
    n_donor[short_idx] <- 2L
  }

  donor_idx <- which(!is.na(raw_flux))
  missing_idx <- which(is.na(filled_flux))

  if (length(donor_idx) > 0 && length(missing_idx) > 0) {
    halfhour_of_day <- as.integer(format(gas_storage$time_halfhour, "%H")) * 60 +
      as.integer(format(gas_storage$time_halfhour, "%M"))

    for (idx_missing in missing_idx) {
      donor_window <- donor_idx[
        halfhour_of_day[donor_idx] == halfhour_of_day[[idx_missing]] &
          abs(as.numeric(difftime(
            gas_storage$time_halfhour[donor_idx],
            gas_storage$time_halfhour[[idx_missing]],
            units = "days"
          ))) <= window_days
      ]

      if (length(donor_window) > 0) {
        donor_values <- raw_flux[donor_window]
        donor_uncertainty <- filled_uncertainty[donor_window]
        filled_flux[[idx_missing]] <- stats::median(donor_values, na.rm = TRUE)
        filled_uncertainty[[idx_missing]] <- if (sum(!is.na(donor_values)) >= 2) {
          stats::sd(donor_values, na.rm = TRUE)
        } else {
          safe_mean(donor_uncertainty)
        }
        method[[idx_missing]] <- paste0("median_diurnal_window_", window_days, "d")
        n_donor[[idx_missing]] <- length(donor_window)
        next
      }

      donor_diurnal <- donor_idx[halfhour_of_day[donor_idx] == halfhour_of_day[[idx_missing]]]

      if (length(donor_diurnal) > 0) {
        donor_values <- raw_flux[donor_diurnal]
        donor_uncertainty <- filled_uncertainty[donor_diurnal]
        filled_flux[[idx_missing]] <- stats::median(donor_values, na.rm = TRUE)
        filled_uncertainty[[idx_missing]] <- if (sum(!is.na(donor_values)) >= 2) {
          stats::sd(donor_values, na.rm = TRUE)
        } else {
          safe_mean(donor_uncertainty)
        }
        method[[idx_missing]] <- "median_diurnal_climatology"
        n_donor[[idx_missing]] <- length(donor_diurnal)
      }
    }
  }

  fallback_idx <- which(is.na(filled_flux) & length(donor_idx) > 0)

  if (length(fallback_idx) > 0) {
    filled_flux[fallback_idx] <- stats::median(raw_flux[donor_idx], na.rm = TRUE)
    filled_uncertainty[fallback_idx] <- if (sum(!is.na(raw_flux[donor_idx])) >= 2) {
      stats::sd(raw_flux[donor_idx], na.rm = TRUE)
    } else {
      safe_mean(filled_uncertainty[donor_idx])
    }
    method[fallback_idx] <- "site_gas_median"
    n_donor[fallback_idx] <- length(donor_idx)
  }

  gas_storage$storage_flux_filled <- filled_flux
  gas_storage$storage_flux_filled_uncertainty <- filled_uncertainty
  gas_storage$storage_flux_is_gapfilled <- gapfilled & !is.na(filled_flux)
  gas_storage$storage_gapfill_method <- method
  gas_storage$storage_gapfill_n_donor <- n_donor

  if (safe_first(gas_storage$gas) %in% c("CO2", "CH4")) {
    gas_storage$storage_flux_ameriflux_sign_filled <- -filled_flux
  } else {
    gas_storage$storage_flux_ameriflux_sign_filled <- NA_real_
  }

  gas_storage
}


complete_halfhour_grid <- function(site_storage) {
  if (nrow(site_storage) == 0) {
    return(site_storage)
  }

  bind_rows(lapply(split(site_storage, site_storage$gas), function(gas_storage) {
    gas_storage <- gas_storage %>%
      filter(!is.na(time_halfhour)) %>%
      arrange(time_halfhour)

    if (nrow(gas_storage) == 0) {
      return(gas_storage)
    }

    site_name <- safe_first(gas_storage$site)
    gas_name <- safe_first(gas_storage$gas)
    time_grid <- tibble(
      site = site_name,
      gas = gas_name,
      time_halfhour = seq(
        from = min(gas_storage$time_halfhour, na.rm = TRUE),
        to = max(gas_storage$time_halfhour, na.rm = TRUE),
        by = "30 min"
      )
    )

    completed <- time_grid %>%
      left_join(gas_storage, by = c("site", "gas", "time_halfhour")) %>%
      mutate(
        storage_time_row_inserted = is.na(timeMid) & is.na(storage_qf) & is.na(storage_flux),
        timeMid = dplyr::coalesce(timeMid, time_halfhour),
        timeBgn = dplyr::coalesce(timeBgn, time_halfhour),
        timeEnd = dplyr::coalesce(timeEnd, time_halfhour + lubridate::minutes(30)),
        time_halfhour_storage_end = dplyr::coalesce(
          time_halfhour_storage_end,
          time_halfhour + lubridate::minutes(30)
        )
      )

    completed <- fill_constant_metadata(completed)
    gapfill_storage_series(completed)
  })) %>%
    arrange(site, gas, time_halfhour)
}


format_time_columns_for_csv <- function(storage_df) {
  time_cols <- intersect(
    c("timeMid", "timeBgn", "timeEnd", "time_halfhour", "time_halfhour_storage_end"),
    names(storage_df)
  )

  for (col_name in time_cols) {
    if (inherits(storage_df[[col_name]], "POSIXt")) {
      storage_df[[col_name]] <- format(
        storage_df[[col_name]],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "GMT"
      )
    }
  }

  storage_df
}


framework_info <- storage_framework()
site.list <- get_site_list(data_dir)

if (length(site.list) == 0) {
  stop(
    "No NEON site folders with _aligned_conc_flux_9min files were found in `",
    data_dir,
    "`."
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Estimating storage flux for ", length(site.list), " NEON tower sites...")
message("Reading aligned concentration inputs from ", data_dir)
message("Reading tower attributes from ", attr_data_dir)
message("Writing site CSVs to ", output_dir)
message("Storage framework: ", framework_info$label)

storage_list <- vector("list", length(site.list))
names(storage_list) <- site.list

for (site in site.list) {
  
  message("Working on ", site, "...")

  min9Diff.list <- load_aligned_site(data_dir, site)
  site_metadata <- build_site_metadata(site, data_dir, min9Diff.list)

  site_storage <- bind_rows(
    compute_storage_flux_gas(site, "CO2", min9Diff.list, site_metadata, framework_info),
    compute_storage_flux_gas(site, "H2O", min9Diff.list, site_metadata, framework_info),
    compute_storage_flux_gas(site, "CH4", min9Diff.list, site_metadata, framework_info)
  ) %>%
    complete_halfhour_grid()

  storage_list[[site]] <- site_storage

  csv_out <- file.path(output_dir, paste0(site, "_storage_flux.csv"))
  utils::write.csv(format_time_columns_for_csv(site_storage), csv_out, row.names = FALSE, na = "")
  message("Saved ", csv_out)

  rm(min9Diff.list, site_metadata, site_storage)
  
  
}

#NEON_storage <- bind_rows(storage_list) %>%
#  arrange(site, gas, timeMid) %>%
#  as_tibble()
NEON_storage <- storage_list
save(NEON_storage, file = file.path(output_dir, "NEON_storage.RData"))

message("Done. Saved `NEON_storage` to ", file.path(output_dir, "NEON_storage.RData"))
