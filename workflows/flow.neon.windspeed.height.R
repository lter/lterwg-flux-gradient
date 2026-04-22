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


load_site_wind_profile_ws2d <- function(site, data_dir) {
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
    filter(!is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_wind_profile_ae <- function(site, data_dir) {
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
    filter(!is.na(TowerHeight_m)) %>%
    arrange(TowerHeight_m)
}


load_site_wind_profile <- function(site, data_dir) {
  ws2d_file <- file.path(data_dir, site, paste0(site, "_WS2D2min.Rdata"))
  attr_file <- get_attr_file(site, data_dir)
  ae_file <- file.path(data_dir, site, paste0(site, "_AE_9min.Rdata"))

  if (file.exists(ws2d_file) && !is.na(attr_file)) {
    return(load_site_wind_profile_ws2d(site, data_dir))
  }

  if (file.exists(ae_file)) {
    return(load_site_wind_profile_ae(site, data_dir))
  }

  stop("No supported wind profile source found for site ", site)
}


plot_panel_wind_profile <- function(profile_summary, out_file) {
  plot_df <- profile_summary %>%
    filter(n_obs > 0, !is.na(mean_windspeed_ms), !is.na(TowerHeight_m))

  p <- ggplot(plot_df, aes(x = mean_windspeed_ms, y = TowerHeight_m)) +
    geom_hline(
      aes(yintercept = mean_canopy_height_m),
      color = "#b23a48",
      linewidth = 0.35,
      linetype = "dashed",
      na.rm = TRUE
    ) +
    geom_path(linewidth = 0.45, color = "gray35", na.rm = TRUE) +
    geom_point(size = 1.2, color = "black", na.rm = TRUE) +
    geom_errorbar(
      aes(
        xmin = mean_windspeed_ms - se_windspeed_ms,
        xmax = mean_windspeed_ms + se_windspeed_ms
      ),
      orientation = "y",
      width = 0,
      linewidth = 0.3,
      color = "gray55",
      na.rm = TRUE
    ) +
    facet_wrap(~ site, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.08))) +
    labs(
      x = "Mean windspeed (m s-1)",
      y = "Tower height (m)",
      title = "NEON tower windspeed profiles",
      subtitle = "Points show mean windspeed at each measurement height; horizontal bars show standard error; dashed red line shows mean canopy height"
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


data_dir <- Sys.getenv("FLUXDATA_DIR", unset = "data")
fig_dir <- Sys.getenv("OUTPUT_FIG_DIR", unset = file.path("figures", "neon_windspeed_height"))
output_data_dir <- Sys.getenv("OUTPUT_DATA_DIR", unset = "data")

fs::dir_create(fig_dir, recurse = TRUE)
fs::dir_create(output_data_dir, recurse = TRUE)

site.list <- get_site_list(data_dir)

if (length(site.list) == 0) {
  stop("No site folders with a supported wind profile source were found in `", data_dir, "`.")
}

message("Creating windspeed-height summaries for ", length(site.list), " sites...")

summary_list <- vector("list", length(site.list))
names(summary_list) <- site.list

for (site in site.list) {
  message("Working on ", site, "...")
  summary_list[[site]] <- load_site_wind_profile(site = site, data_dir = data_dir)
}

NEON_windspeed_height_summary <- bind_rows(summary_list) %>%
  mutate(
    canopy_height_source = dplyr::coalesce(canopy_height_source, ifelse(source == "WS2D2min", "DistZaxsCnpy", "z_veg_aero")),
    site = factor(site, levels = sort(unique(site)))
  ) %>%
  arrange(site, TowerHeight_m) %>%
  as_tibble()

save(
  NEON_windspeed_height_summary,
  file = file.path(output_data_dir, "NEON_windspeed_height_summary.RData")
)

readr::write_csv(
  NEON_windspeed_height_summary %>% mutate(site = as.character(site)),
  file.path(output_data_dir, "NEON_windspeed_height_summary.csv")
)

plot_panel_wind_profile(
  profile_summary = NEON_windspeed_height_summary,
  out_file = file.path(fig_dir, "NEON_windspeed_height_panel.png")
)

message("Done. Summary saved to ", file.path(output_data_dir, "NEON_windspeed_height_summary.csv"))
message("Done. Panel plot saved to ", file.path(fig_dir, "NEON_windspeed_height_panel.png"))
