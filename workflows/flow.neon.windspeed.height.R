## --------------------------------------------- ##
#     NEON Windspeed Height Summary + Panel -----
## --------------------------------------------- ##
# Purpose:
# Extract mean and standard error of windspeed at each tower height for all
# NEON tower sites, append mean canopy height for each site, save one CSV,
# and create a faceted panel plot of windspeed versus height.
#
# Input priority by site:
#   1. `*_WS2D2min.Rdata` + `*_attr.Rdata`
#   2. `*_AE_9min.Rdata`
#
# Outputs:
#   data/NEON_windspeed_height_summary.RData
#   data/NEON_windspeed_height_summary.csv
#   figures/neon_windspeed_height/NEON_windspeed_height_panel.png
#
# Optional environment variables:
#   FLUXDATA_DIR    input site directory root
#   OUTPUT_FIG_DIR  output directory for PNG files
#   OUTPUT_DATA_DIR output directory for RData/CSV files

rm(list = ls())

library(dplyr)
library(fs)
library(ggplot2)
library(readr)
library(tibble)
library(tidyr)


extract_rshp_positions <- function(df) {
  sort(unique(c(as.numeric(df$TowerPosition_A), as.numeric(df$TowerPosition_B))))
}


load_rshp_pairs <- function(rshp_file, canopy_classes = c("AA", "AW"), gas_filter = NULL) {
  if (!file.exists(rshp_file)) {
    return(tibble(
      site = character(),
      gas = character(),
      dLevelsAminusB = character(),
      TowerPosition_A = numeric(),
      TowerPosition_B = numeric()
    ))
  }

  objs <- load(rshp_file)
  candidate_names <- c("SITES_One2One_canopy_model", "SITES_One2One_canopy", "SITES_One2One")
  obj_name <- candidate_names[candidate_names %in% objs][1]

  if (is.na(obj_name)) {
    return(tibble(
      site = character(),
      gas = character(),
      dLevelsAminusB = character(),
      TowerPosition_A = numeric(),
      TowerPosition_B = numeric()
    ))
  }

  ccc_df <- get(obj_name)

  if (!is.data.frame(ccc_df) || !all(c("Site", "gas", "Good.CCC", "TowerPosition_A", "TowerPosition_B", "dLevelsAminusB") %in% names(ccc_df))) {
    return(tibble(
      site = character(),
      gas = character(),
      dLevelsAminusB = character(),
      TowerPosition_A = numeric(),
      TowerPosition_B = numeric()
    ))
  }

  out <- ccc_df %>%
    filter(Good.CCC == 1)

  if (!is.null(canopy_classes) && "Canopy_L1" %in% names(out)) {
    out <- out %>% filter(Canopy_L1 %in% canopy_classes)
  }

  if (!is.null(gas_filter) && "gas" %in% names(out)) {
    out <- out %>% filter(gas %in% gas_filter)
  }

  out %>%
    transmute(
      site = as.character(Site),
      gas = as.character(gas),
      dLevelsAminusB = as.character(dLevelsAminusB),
      TowerPosition_A = as.numeric(TowerPosition_A),
      TowerPosition_B = as.numeric(TowerPosition_B)
    ) %>%
    distinct()
}


load_rshp_lookup <- function(rshp_file, canopy_classes = c("AA", "AW"), gas_filter = NULL) {
  rshp_pairs <- load_rshp_pairs(
    rshp_file,
    canopy_classes = canopy_classes,
    gas_filter = gas_filter
  )

  if (nrow(rshp_pairs) == 0) {
    return(tibble(site = character(), TowerPosition = numeric()))
  }

  rshp_pairs %>%
    group_by(site) %>%
    summarise(TowerPosition = list(extract_rshp_positions(pick(everything()))), .groups = "drop") %>%
    tidyr::unnest(TowerPosition) %>%
    mutate(TowerPosition = as.numeric(TowerPosition))
}


get_attr_file <- function(site, data_dir) {
  candidates <- c(
    file.path(data_dir, site, paste0(site, "_attr.Rdata")),
    file.path(data_dir, site, "data", site, paste0(site, "_attr.Rdata")),
    file.path(data_dir, site, "data", paste0(site, "_attr.Rdata"))
  )

  match <- candidates[file.exists(candidates)]

  if (length(match) == 0) {
    return(NA_character_)
  }

  match[[1]]
}


get_site_list <- function(data_dir) {
  site_dirs <- fs::dir_ls(data_dir, type = "directory", recurse = FALSE)
  site_names <- basename(site_dirs)
  is_neon <- grepl("^[A-Z]{4}$", site_names)

  has_attr <- !is.na(vapply(
    site_names,
    function(site) get_attr_file(site, data_dir),
    FUN.VALUE = character(1)
  ))

  has_ws2d <- file.exists(file.path(data_dir, site_names, paste0(site_names, "_WS2D2min.Rdata"))) &
    has_attr

  has_ae <- file.exists(file.path(data_dir, site_names, paste0(site_names, "_AE_9min.Rdata")))

  sort(site_names[is_neon & (has_ws2d | has_ae)])
}


safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) {
    return(NA_real_)
  }

  sd(x, na.rm = TRUE)
}


safe_se <- function(x) {
  n <- sum(!is.na(x))

  if (n <= 1) {
    return(NA_real_)
  }

  stats::sd(x, na.rm = TRUE) / sqrt(n)
}


summarise_windspeed <- function(windspeed) {
  tibble(
    mean_windspeed_ms = safe_mean(windspeed),
    median_windspeed_ms = safe_median(windspeed),
    sd_windspeed_ms = safe_sd(windspeed),
    se_windspeed_ms = safe_se(windspeed),
    n_obs = sum(!is.na(windspeed))
  )
}


summarise_concentration <- function(concentration) {
  tibble(
    mean_concentration = safe_mean(concentration),
    sd_concentration = safe_sd(concentration),
    n_conc_obs = sum(!is.na(concentration))
  )
}


empty_concentration_profile <- function() {
  tibble(
    TowerPosition = numeric(),
    TowerHeight_m = numeric(),
    mean_concentration = numeric(),
    sd_concentration = numeric(),
    n_conc_obs = numeric()
  )
}


load_attr_df <- function(site, data_dir) {
  attr_file <- get_attr_file(site, data_dir)

  if (is.na(attr_file)) {
    return(NULL)
  }

  load(attr_file)

  if (exists("attr.df")) {
    return(attr.df)
  }

  NULL
}


load_site_wind_profile_ws2d <- function(site, data_dir, rshp_lookup = NULL) {
  load(file.path(data_dir, site, paste0(site, "_WS2D2min.Rdata")))
  attr.df <- load_attr_df(site, data_dir)

  if (is.null(attr.df)) {
    stop("No `attr.df` object found for site ", site)
  }

  ws_df <- if (exists("WS2D2min")) {
    WS2D2min
  } else if (exists("DATA") && "WS2D2min" %in% names(DATA)) {
    DATA$WS2D2min
  } else {
    stop("No `WS2D2min` object found in ", site, "_WS2D2min.Rdata")
  }

  canopy_height_m <- if (exists("attr.df") && "DistZaxsCnpy" %in% names(attr.df)) {
    safe_mean(as.numeric(attr.df$DistZaxsCnpy))
  } else {
    NA_real_
  }

  rshp_positions <- if (!is.null(rshp_lookup)) {
    rshp_lookup %>%
      filter(site == !!site) %>%
      pull(TowerPosition) %>%
      as.numeric()
  } else {
    numeric()
  }

  ws_df %>%
    transmute(
      TowerPosition = as.numeric(TowerPosition),
      windspeed_ms = as.numeric(windSpeedMean),
      qf = as.numeric(windSpeedFinalQF)
    ) %>%
    mutate(windspeed_ms = ifelse(qf == 1, NA_real_, windspeed_ms)) %>%
    group_by(TowerPosition) %>%
    group_modify(~ summarise_windspeed(.x$windspeed_ms)) %>%
    ungroup() %>%
    left_join(
      attr.df %>%
        transmute(
          TowerPosition = as.numeric(TowerPosition),
          TowerHeight_m = as.numeric(DistZaxsLvlMeasTow)
        ),
      by = "TowerPosition"
    ) %>%
    mutate(
      site = site,
      source = "WS2D2min",
      mean_canopy_height_m = canopy_height_m,
      canopy_height_source = "DistZaxsCnpy"
    ) %>%
    mutate(is_rshp = TowerPosition %in% rshp_positions) %>%
    filter(!is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_wind_profile_ae <- function(site, data_dir, rshp_lookup = NULL) {
  load(file.path(data_dir, site, paste0(site, "_AE_9min.Rdata")))
  df <- min9.FG.AE.list[[1]]
  attr.df <- load_attr_df(site, data_dir)

  ubar_cols <- grep("^ubar[0-9]+$", names(df), value = TRUE)
  if (length(ubar_cols) == 0) {
    stop("No `ubar` columns found in ", site, "_AE_9min.Rdata")
  }

  if (!is.null(attr.df)) {
    heights_df <- attr.df %>%
      transmute(
        TowerPosition = as.numeric(TowerPosition),
        TowerHeight_m = as.numeric(DistZaxsLvlMeasTow)
      ) %>%
      filter(!is.na(TowerPosition), !is.na(TowerHeight_m)) %>%
      distinct() %>%
      group_by(TowerPosition) %>%
      summarise(TowerHeight_m = mean(TowerHeight_m, na.rm = TRUE), .groups = "drop")

    canopy_height_m <- if ("DistZaxsCnpy" %in% names(attr.df)) {
      safe_mean(as.numeric(attr.df$DistZaxsCnpy))
    } else if ("z_veg_aero" %in% names(df)) {
      safe_mean(as.numeric(df$z_veg_aero))
    } else {
      NA_real_
    }

    canopy_height_source <- if ("DistZaxsCnpy" %in% names(attr.df)) "DistZaxsCnpy" else "z_veg_aero"

  } else {
    heights_a <- df %>%
      transmute(
        TowerPosition = as.numeric(TowerPosition_A),
        TowerHeight_m = as.numeric(TowerHeight_A)
      )

    heights_b <- df %>%
      transmute(
        TowerPosition = as.numeric(TowerPosition_B),
        TowerHeight_m = as.numeric(TowerHeight_B)
      )

    heights_df <- bind_rows(heights_a, heights_b) %>%
      filter(!is.na(TowerPosition), !is.na(TowerHeight_m)) %>%
      group_by(TowerPosition) %>%
      summarise(TowerHeight_m = mean(TowerHeight_m, na.rm = TRUE), .groups = "drop")

    canopy_height_m <- if ("z_veg_aero" %in% names(df)) {
      safe_mean(as.numeric(df$z_veg_aero))
    } else {
      NA_real_
    }

    canopy_height_source <- "z_veg_aero"
  }

  rshp_positions <- if (!is.null(rshp_lookup)) {
    rshp_lookup %>%
      filter(site == !!site) %>%
      pull(TowerPosition) %>%
      as.numeric()
  } else {
    numeric()
  }

  bind_rows(lapply(ubar_cols, function(col) {
    tower_position <- as.numeric(sub("^ubar", "", col))
    windspeed <- as.numeric(df[[col]])

    bind_cols(
      tibble(TowerPosition = tower_position),
      summarise_windspeed(windspeed)
    )
  })) %>%
    left_join(heights_df, by = "TowerPosition") %>%
    mutate(
      site = site,
      source = "AE_9min_ubar",
      mean_canopy_height_m = canopy_height_m,
      canopy_height_source = canopy_height_source
    ) %>%
    mutate(is_rshp = TowerPosition %in% rshp_positions) %>%
    filter(!is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_wind_profile <- function(site, data_dir, rshp_lookup = NULL) {
  ws2d_file <- file.path(data_dir, site, paste0(site, "_WS2D2min.Rdata"))
  attr_file <- get_attr_file(site, data_dir)
  ae_file <- file.path(data_dir, site, paste0(site, "_AE_9min.Rdata"))

  if (file.exists(ws2d_file) && !is.na(attr_file)) {
    return(load_site_wind_profile_ws2d(site, data_dir, rshp_lookup = rshp_lookup))
  }

  if (file.exists(ae_file)) {
    return(load_site_wind_profile_ae(site, data_dir, rshp_lookup = rshp_lookup))
  }

  stop("No supported wind profile source found for site ", site)
}


get_aligned_conc_file <- function(site, data_dir) {
  candidates <- c(
    file.path(data_dir, site, paste0(site, "_aligned_conc_flux_9min.RData")),
    file.path(data_dir, site, paste0(site, "_aligned_conc_flux_9min.Rdata"))
  )

  match <- candidates[file.exists(candidates)]

  if (length(match) == 0) {
    return(NA_character_)
  }

  match[[1]]
}


load_site_concentration_profile_pairwise <- function(conc_df, height_lookup) {
  conc_a <- conc_df %>%
    transmute(
      time_key = paste(timeBgn_A, timeEnd_A, sep = "_"),
      TowerPosition = as.numeric(TowerPosition_A),
      concentration = ifelse(as.numeric(qfFinl_A) == 1, NA_real_, as.numeric(mean_A))
    )

  conc_b <- conc_df %>%
    transmute(
      time_key = paste(timeBgn_B, timeEnd_B, sep = "_"),
      TowerPosition = as.numeric(TowerPosition_B),
      concentration = ifelse(as.numeric(qfFinl_B) == 1, NA_real_, as.numeric(mean_B))
    )

  bind_rows(conc_a, conc_b) %>%
    filter(!is.na(TowerPosition)) %>%
    distinct(time_key, TowerPosition, .keep_all = TRUE) %>%
    group_by(TowerPosition) %>%
    group_modify(~ summarise_concentration(.x$concentration)) %>%
    ungroup() %>%
    left_join(height_lookup, by = "TowerPosition") %>%
    filter(!is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_concentration_profile_wide <- function(all_data, gas) {
  conc_cols <- grep(paste0("^", gas, "_[0-9]+$"), names(all_data), value = TRUE)

  if (length(conc_cols) == 0) {
    return(empty_concentration_profile())
  }

  all_data %>%
    select(all_of(conc_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "column_name",
      values_to = "concentration"
    ) %>%
    mutate(
      TowerHeight_m = as.numeric(sub(paste0("^", gas, "_"), "", column_name)) / 100
    ) %>%
    group_by(TowerHeight_m) %>%
    group_modify(~ summarise_concentration(as.numeric(.x$concentration))) %>%
    ungroup() %>%
    mutate(TowerPosition = dplyr::dense_rank(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_concentration_profile <- function(site, data_dir, gas, height_lookup, e = NULL, objs = NULL) {
  conc_file <- get_aligned_conc_file(site, data_dir)

  if (is.na(conc_file)) {
    return(empty_concentration_profile())
  }

  if (is.null(e) || is.null(objs)) {
    e <- new.env(parent = emptyenv())
    objs <- load(conc_file, envir = e)
  }

  if ("min9Diff.list" %in% objs && gas %in% names(e$min9Diff.list)) {
    return(load_site_concentration_profile_pairwise(
      conc_df = e$min9Diff.list[[gas]],
      height_lookup = height_lookup
    ))
  }

  if ("all_data" %in% objs) {
    return(load_site_concentration_profile_wide(all_data = e$all_data, gas = gas))
  }

  empty_concentration_profile()
}


build_concentration_summary <- function(profile_summary, data_dir, gases = c("CO2", "H2O")) {
  site_height_lookup <- profile_summary %>%
    select(site, TowerPosition, TowerHeight_m) %>%
    distinct() %>%
    filter(!is.na(TowerPosition), !is.na(TowerHeight_m))

  site_values <- sort(unique(as.character(profile_summary$site)))
  mc_cores <- min(4L, parallel::detectCores(logical = FALSE))
  message("Loading concentration profiles for ", length(site_values), " sites using ", mc_cores, " workers...")

  site_results <- parallel::mclapply(site_values, function(site) {
    message("Loading concentration profiles for ", site, "...")

    height_lookup <- site_height_lookup %>%
      filter(site == !!site) %>%
      select(TowerPosition, TowerHeight_m)

    conc_file <- get_aligned_conc_file(site, data_dir)

    if (is.na(conc_file)) {
      return(bind_rows(lapply(gases, function(gas) {
        empty_concentration_profile() %>%
          mutate(site = site, gas = gas)
      })))
    }

    e <- new.env(parent = emptyenv())
    objs <- load(conc_file, envir = e)

    bind_rows(lapply(gases, function(gas) {
      load_site_concentration_profile(
        site = site,
        data_dir = data_dir,
        gas = gas,
        height_lookup = height_lookup,
        e = e,
        objs = objs
      ) %>%
        mutate(site = site, gas = gas)
    }))
  }, mc.cores = mc_cores)

  out <- bind_rows(site_results)

  if (!"TowerHeight_m" %in% names(out)) {
    return(
      empty_concentration_profile() %>%
        mutate(site = character(), gas = character())
    )
  }

  out %>%
    filter(!is.na(TowerHeight_m)) %>%
    as_tibble()
}


plot_panel_windspeed_only <- function(profile_summary, out_file) {
  plot_df <- profile_summary %>%
    filter(n_obs > 0, !is.na(mean_windspeed_ms), !is.na(TowerHeight_m)) %>%
    mutate(is_reliable = is_rshp_co2 | is_rshp_h2o)

  canopy_df <- plot_df %>%
    group_by(site) %>%
    summarise(mean_canopy_height_m = safe_mean(mean_canopy_height_m), .groups = "drop")

  p <- ggplot(plot_df, aes(x = mean_windspeed_ms, y = TowerHeight_m)) +
    geom_rect(
      data = canopy_df,
      aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = mean_canopy_height_m),
      inherit.aes = FALSE,
      fill = "#8fbc8f",
      alpha = 0.22,
      color = NA,
      na.rm = TRUE
    ) +
    geom_hline(
      aes(yintercept = mean_canopy_height_m),
      color = "#006400",
      linewidth = 0.35,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    geom_path(linewidth = 0.45, color = "gray35", na.rm = TRUE) +
    geom_errorbar(
      aes(
        xmin = mean_windspeed_ms - se_windspeed_ms,
        xmax = mean_windspeed_ms + se_windspeed_ms
      ),
      orientation = "y",
      width = 0.18,
      linewidth = 0.45,
      color = "black",
      na.rm = TRUE
    ) +
    geom_point(aes(color = is_reliable), size = 1.6, na.rm = TRUE) +
    facet_wrap(~ site, scales = "free_y") +
    scale_color_manual(
      values = c(`TRUE` = "goldenrod", `FALSE` = "black"),
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 5), expand = expansion(mult = c(0.03, 0.03))) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
    labs(
      x = "Mean windspeed (m s-1)",
      y = "Tower height (m)",
      title = "NEON tower windspeed profiles",
      subtitle = "Goldenrod points mark heights reliable in AA/AW RSHP pairs for CO2 or H2O; black horizontal bars show windspeed SE; dashed dark green line shows mean canopy height"
    ) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold", size = 8),
      plot.title = element_text(face = "bold")
    )

  ggplot2::ggsave(filename = out_file, plot = p, width = 18, height = 14, dpi = 300)
}


plot_panel_concentration_only <- function(profile_summary, out_file, gas, rshp_pairs = NULL) {
  rshp_flag_col <- paste0("is_rshp_", tolower(gas))
  mean_conc_col <- paste0("mean_concentration_", gas, "_profile")
  sd_conc_col <- paste0("sd_concentration_", gas, "_profile")

  plot_df <- profile_summary %>%
    filter(!is.na(TowerHeight_m)) %>%
    mutate(
      is_rshp_gas = .data[[rshp_flag_col]],
      mean_concentration = .data[[mean_conc_col]],
      sd_concentration = .data[[sd_conc_col]]
    )

  canopy_df <- plot_df %>%
    group_by(site) %>%
    summarise(mean_canopy_height_m = safe_mean(mean_canopy_height_m), .groups = "drop")

  conc_df <- plot_df %>%
    filter(!is.na(mean_concentration))

  conc_range <- range(
    conc_df$mean_concentration,
    na.rm = TRUE
  )

  if (!all(is.finite(conc_range))) {
    conc_range <- c(0, 1)
  }

  conc_pad <- 0.06 * diff(conc_range)

  if (!is.finite(conc_pad) || conc_pad == 0) {
    conc_pad <- 0.5
  }

  conc_range <- conc_range + c(-conc_pad, conc_pad)

  if (diff(conc_range) == 0) {
    conc_range <- conc_range + c(-0.5, 0.5)
  }

  if (nrow(conc_df) > 0) {
    conc_df <- conc_df %>%
      mutate(
        x_conc = mean_concentration,
        x_conc_lo = ifelse(
          is.na(sd_concentration),
          NA_real_,
          mean_concentration - sd_concentration
        ),
        x_conc_hi = ifelse(
          is.na(sd_concentration),
          NA_real_,
          mean_concentration + sd_concentration
        )
      )
  }

  pair_df <- tibble()

  if (!is.null(rshp_pairs) && nrow(rshp_pairs) > 0) {
    pair_heights_hi <- plot_df %>%
      select(site, TowerPosition, TowerHeight_m) %>%
      distinct() %>%
      rename(TowerPosition_A = TowerPosition, TowerHeight_A_m = TowerHeight_m)

    pair_heights_lo <- plot_df %>%
      select(site, TowerPosition, TowerHeight_m) %>%
      distinct() %>%
      rename(TowerPosition_B = TowerPosition, TowerHeight_B_m = TowerHeight_m)

    pair_x <- plot_df %>%
      mutate(x_extent = mean_concentration + ifelse(is.na(sd_concentration), 0, sd_concentration)) %>%
      group_by(site) %>%
      summarise(
        x_anchor = max(x_extent, na.rm = TRUE),
        x_step = max(0.2, 0.08 * max(x_extent, na.rm = TRUE)),
        .groups = "drop"
      )

    pair_df <- rshp_pairs %>%
      filter(gas == !!gas) %>%
      distinct(site, gas, dLevelsAminusB, TowerPosition_A, TowerPosition_B) %>%
      left_join(pair_heights_hi, by = c("site", "TowerPosition_A")) %>%
      left_join(pair_heights_lo, by = c("site", "TowerPosition_B")) %>%
      filter(!is.na(TowerHeight_A_m), !is.na(TowerHeight_B_m)) %>%
      mutate(
        y_low = pmin(TowerHeight_A_m, TowerHeight_B_m),
        y_high = pmax(TowerHeight_A_m, TowerHeight_B_m),
        pair_length_m = y_high - y_low
      ) %>%
      arrange(site, pair_length_m, y_low, y_high) %>%
      group_by(site) %>%
      mutate(pair_rank = row_number()) %>%
      ungroup() %>%
      left_join(pair_x, by = "site") %>%
      mutate(
        x_pair = x_anchor + pair_rank * (1.55 * x_step)
      )
  }

  conc_color <- if (gas == "CO2") "#8b0000" else "#0b7285"
  pair_color <- if (gas == "CO2") "green4" else "blue3"

  p <- ggplot(plot_df, aes(y = TowerHeight_m)) +
    geom_rect(
      data = canopy_df,
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0,
        ymax = mean_canopy_height_m
      ),
      inherit.aes = FALSE,
      fill = "#8fbc8f",
      alpha = 0.22,
      color = NA,
      na.rm = TRUE
    ) +
    geom_hline(
      aes(yintercept = mean_canopy_height_m),
      color = "#006400",
      linewidth = 0.35,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    geom_path(
      data = conc_df,
      aes(x = x_conc, y = TowerHeight_m, group = site),
      inherit.aes = FALSE,
      linewidth = 0.45,
      color = conc_color,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    geom_errorbar(
      data = conc_df,
      aes(xmin = x_conc_lo, xmax = x_conc_hi, y = TowerHeight_m),
      inherit.aes = FALSE,
      orientation = "y",
      width = 0.18,
      linewidth = 0.45,
      color = conc_color,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    geom_point(
      data = conc_df,
      aes(x = x_conc, y = TowerHeight_m),
      inherit.aes = FALSE,
      size = 1.2,
      color = conc_color,
      alpha = 0.95,
      na.rm = TRUE
    ) +
    geom_segment(
      data = pair_df,
      aes(
        x = x_pair,
        xend = x_pair,
        y = y_low,
        yend = y_high
      ),
      inherit.aes = FALSE,
      linewidth = 0.55,
      alpha = 0.95,
      color = pair_color,
      na.rm = TRUE
    ) +
    geom_point(
      data = conc_df,
      aes(x = x_conc, y = TowerHeight_m, color = is_rshp_gas),
      inherit.aes = FALSE,
      size = 1.5,
      na.rm = TRUE
    ) +
    facet_wrap(~ site, scales = "free_y") +
    scale_color_manual(
      values = c(`TRUE` = "goldenrod", `FALSE` = "black"),
      guide = "none"
    ) +
    scale_x_continuous(limits = conc_range, expand = expansion(mult = c(0.03, 0.38))) +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
    labs(
      x = paste0(gas, " concentration mean +/- SD"),
      y = "Tower height (m)",
      title = paste("NEON tower", gas, "concentration profiles"),
      subtitle = paste0(
        "Goldenrod points mark heights in AA/AW ", gas,
        " RSHP pairs (CCC > 0.5); vertical bars show ",
        gas,
        " RSHP pairs; profile shows mean +/- SD; dashed dark green line shows mean canopy height"
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold", size = 8),
      plot.title = element_text(face = "bold")
    )

  ggplot2::ggsave(
    filename = out_file,
    plot = p,
    width = 18,
    height = 14,
    dpi = 300
  )
}


default_flux_dir <- if (dir.exists("/Volumes/MaloneLab/Research/FluxGradient/FluxData")) {
  "/Volumes/MaloneLab/Research/FluxGradient/FluxData"
} else {
  "data"
}

data_dir <- Sys.getenv("FLUXDATA_DIR", unset = default_flux_dir)
fig_dir <- Sys.getenv("OUTPUT_FIG_DIR", unset = file.path("figures", "neon_windspeed_height"))
output_data_dir <- Sys.getenv("OUTPUT_DATA_DIR", unset = "data")
rshp_file <- Sys.getenv(
  "RSHP_FILE",
  unset = file.path("..", "lterwg-flux-gradient-evalSITES_One2One_canopy_model.Rdata")
)
target_canopy_classes <- c("AA", "AW")

fs::dir_create(fig_dir, recurse = TRUE)
fs::dir_create(output_data_dir, recurse = TRUE)
rshp_pairs_all <- load_rshp_pairs(rshp_file, canopy_classes = target_canopy_classes)
rshp_pairs_co2 <- load_rshp_pairs(rshp_file, canopy_classes = target_canopy_classes, gas_filter = "CO2")
rshp_pairs_h2o <- load_rshp_pairs(rshp_file, canopy_classes = target_canopy_classes, gas_filter = "H2O")
rshp_lookup_co2 <- load_rshp_lookup(rshp_file, canopy_classes = target_canopy_classes, gas_filter = "CO2")
rshp_lookup_h2o <- load_rshp_lookup(rshp_file, canopy_classes = target_canopy_classes, gas_filter = "H2O")

existing_summary_file <- file.path(output_data_dir, "NEON_windspeed_height_summary.csv")

if (file.exists(existing_summary_file)) {
  message("Loading existing windspeed-height summary from ", existing_summary_file, "...")
  NEON_windspeed_height_summary <- readr::read_csv(existing_summary_file, show_col_types = FALSE) %>%
    select(-any_of(c(
      "is_rshp_co2",
      "is_rshp_h2o"
    ))) %>%
    mutate(site = factor(site, levels = sort(unique(site))))
} else {
  site.list <- get_site_list(data_dir)

  if (length(site.list) == 0) {
    stop("No site folders with a supported wind profile source were found in `", data_dir, "`.")
  }

  message("Creating windspeed-height summaries for ", length(site.list), " sites...")

  summary_list <- vector("list", length(site.list))
  names(summary_list) <- site.list

  for (site in site.list) {
    message("Working on ", site, "...")
    summary_list[[site]] <- load_site_wind_profile(site = site, data_dir = data_dir, rshp_lookup = NULL)
  }

  NEON_windspeed_height_summary <- bind_rows(summary_list) %>%
    mutate(
      canopy_height_source = dplyr::coalesce(canopy_height_source, ifelse(source == "WS2D2min", "DistZaxsCnpy", "z_veg_aero")),
      site = factor(site, levels = sort(unique(site)))
    )
}

NEON_windspeed_height_summary <- NEON_windspeed_height_summary %>%
  left_join(
    rshp_lookup_co2 %>%
      mutate(is_rshp_co2 = TRUE),
    by = c("site", "TowerPosition")
  ) %>%
  left_join(
    rshp_lookup_h2o %>%
      mutate(is_rshp_h2o = TRUE),
    by = c("site", "TowerPosition")
  ) %>%
  mutate(
    is_rshp_co2 = dplyr::coalesce(is_rshp_co2, FALSE),
    is_rshp_h2o = dplyr::coalesce(is_rshp_h2o, FALSE)
  ) %>%
  arrange(site, TowerHeight_m) %>%
  as_tibble()

has_concentration_cols <- all(c(
  "mean_concentration_CO2_profile",
  "sd_concentration_CO2_profile",
  "n_conc_obs_CO2_profile",
  "mean_concentration_H2O_profile",
  "sd_concentration_H2O_profile",
  "n_conc_obs_H2O_profile"
) %in% names(NEON_windspeed_height_summary))

if (!has_concentration_cols) {
  concentration_summary <- build_concentration_summary(
    profile_summary = NEON_windspeed_height_summary,
    data_dir = data_dir,
    gases = c("CO2", "H2O")
  )

  concentration_wide <- concentration_summary %>%
    select(site, TowerHeight_m, gas, mean_concentration, sd_concentration, n_conc_obs) %>%
    mutate(gas = paste0(gas, "_profile")) %>%
    pivot_wider(
      names_from = gas,
      values_from = c(mean_concentration, sd_concentration, n_conc_obs),
      names_sep = "_"
    )

  NEON_windspeed_height_summary <- NEON_windspeed_height_summary %>%
    left_join(concentration_wide, by = c("site", "TowerHeight_m"))
}

save(
  NEON_windspeed_height_summary,
  file = file.path(output_data_dir, "NEON_windspeed_height_summary.RData")
)

readr::write_csv(
  NEON_windspeed_height_summary %>% mutate(site = as.character(site)),
  file.path(output_data_dir, "NEON_windspeed_height_summary.csv")
)

plot_panel_windspeed_only(
  profile_summary = NEON_windspeed_height_summary,
  out_file = file.path(fig_dir, "NEON_windspeed_height_panel_wind.png")
)

plot_panel_concentration_only(
  profile_summary = NEON_windspeed_height_summary,
  out_file = file.path(fig_dir, "NEON_windspeed_height_panel_CO2.png"),
  gas = "CO2",
  rshp_pairs = rshp_pairs_co2
)

plot_panel_concentration_only(
  profile_summary = NEON_windspeed_height_summary,
  out_file = file.path(fig_dir, "NEON_windspeed_height_panel_H2O.png"),
  gas = "H2O",
  rshp_pairs = rshp_pairs_h2o
)

message("Done. Summary saved to ", file.path(output_data_dir, "NEON_windspeed_height_summary.csv"))
message("Done. Panel plots saved to ", file.path(fig_dir, "NEON_windspeed_height_panel_wind.png"),
        ", ", file.path(fig_dir, "NEON_windspeed_height_panel_CO2.png"),
        " and ", file.path(fig_dir, "NEON_windspeed_height_panel_H2O.png"))
