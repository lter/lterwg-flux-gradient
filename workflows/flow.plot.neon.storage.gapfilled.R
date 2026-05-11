## --------------------------------------------- ##
#      Plot NEON Gapfilled Storage Fluxes -----
## --------------------------------------------- ##
# Purpose:
# Create per-site panel plots for gapfilled CO2, H2O, and CH4 storage fluxes
# with shaded uncertainty regions.
#
# Input:
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/*_storage_flux.csv
#
# Output:
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/figures/
#     SITE_gapfilled_storage_fluxes_uncertainty_panels_full.png
#     SITE_gapfilled_storage_fluxes_uncertainty_panels_zoom.png
#
# Optional command-line use:
#   Rscript workflows/flow.plot.neon.storage.gapfilled.R [storage_dir] [figures_dir]

library(dplyr)
library(ggplot2)
library(lubridate)

storage_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux"
figures_dir <- file.path(storage_dir, "figures")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  storage_dir <- args[[1]]
  figures_dir <- file.path(storage_dir, "figures")
}
if (length(args) >= 2) {
  figures_dir <- args[[2]]
}

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

required_columns <- c(
  "site",
  "gas",
  "time_halfhour",
  "storage_flux_filled",
  "storage_flux_filled_uncertainty",
  "storage_flux_is_gapfilled",
  "storage_flux_uncertainty_units"
)


read_storage_csv <- function(path) {
  storage <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing_columns <- setdiff(required_columns, names(storage))

  if (length(missing_columns) > 0) {
    warning(
      "Skipping ",
      basename(path),
      ": missing required columns ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
    return(NULL)
  }

  storage
}


prepare_plot_data <- function(storage) {
  storage %>%
    filter(
      !is.na(storage_flux_filled),
      !is.na(storage_flux_filled_uncertainty)
    ) %>%
    mutate(
      time_halfhour = as.POSIXct(
        time_halfhour,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "GMT"
      ),
      storage_flux_is_gapfilled = as.logical(storage_flux_is_gapfilled),
      ymin = storage_flux_filled - storage_flux_filled_uncertainty,
      ymax = storage_flux_filled + storage_flux_filled_uncertainty,
      gas_panel = paste0(gas, " (", storage_flux_uncertainty_units, ")")
    ) %>%
    filter(!is.na(time_halfhour)) %>%
    mutate(
      gas_panel = factor(
        gas_panel,
        levels = unique(gas_panel[order(match(gas, c("CO2", "H2O", "CH4")))])
      )
    )
}


make_storage_plot <- function(plot_df, site_name, subtitle, point_size,
                              point_alpha, ribbon_alpha) {
  ggplot(plot_df, aes(x = time_halfhour)) +
    geom_ribbon(
      aes(ymin = ymin, ymax = ymax),
      fill = "#F28E2B",
      alpha = ribbon_alpha,
      linewidth = 0
    ) +
    geom_line(aes(y = storage_flux_filled), color = "#174A7C", linewidth = 0.25) +
    geom_point(
      data = plot_df %>% filter(storage_flux_is_gapfilled),
      aes(y = storage_flux_filled),
      color = "#C43C39",
      fill = "#F7B267",
      shape = 21,
      size = point_size,
      stroke = 0.25,
      alpha = point_alpha
    ) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.22) +
    facet_wrap(~ gas_panel, ncol = 1, scales = "free_y") +
    labs(
      title = paste(site_name, "Gapfilled Storage Fluxes"),
      subtitle = subtitle,
      x = "Time (UTC)",
      y = "Storage flux"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}


plot_site_file <- function(path, figures_dir) {
  storage <- read_storage_csv(path)
  if (is.null(storage)) {
    return(tibble())
  }

  plot_df <- prepare_plot_data(storage)
  if (nrow(plot_df) == 0) {
    warning("Skipping ", basename(path), ": no plottable rows.", call. = FALSE)
    return(tibble())
  }

  site_name <- unique(plot_df$site)
  if (length(site_name) != 1) {
    site_name <- sub("_storage_flux[.]csv$", "", basename(path))
  }

  full_plot <- make_storage_plot(
    plot_df = plot_df,
    site_name = site_name,
    subtitle = paste(
      "Blue line = gapfilled continuous series;",
      "orange-red points = gapfilled half-hours;",
      "shaded band = +/-1 filled uncertainty"
    ),
    point_size = 0.25,
    point_alpha = 0.45,
    ribbon_alpha = 0.28
  )

  full_path <- file.path(
    figures_dir,
    paste0(site_name, "_gapfilled_storage_fluxes_uncertainty_panels_full.png")
  )
  ggsave(full_path, full_plot, width = 13, height = 9, dpi = 300)

  zoom_start <- min(plot_df$time_halfhour, na.rm = TRUE)
  zoom_end <- zoom_start + lubridate::days(14)
  zoom_df <- plot_df %>%
    filter(time_halfhour >= zoom_start, time_halfhour <= zoom_end)

  zoom_path <- NA_character_
  if (nrow(zoom_df) > 0) {
    zoom_plot <- make_storage_plot(
      plot_df = zoom_df,
      site_name = site_name,
      subtitle = paste(
        "First 14 days in file;",
        "orange-red points = gapfilled half-hours;",
        "shaded band = +/-1 filled uncertainty"
      ),
      point_size = 0.9,
      point_alpha = 0.9,
      ribbon_alpha = 0.5
    )

    zoom_path <- file.path(
      figures_dir,
      paste0(site_name, "_gapfilled_storage_fluxes_uncertainty_panels_zoom.png")
    )
    ggsave(zoom_path, zoom_plot, width = 13, height = 9, dpi = 300)
  }

  tibble(
    site = site_name,
    csv = path,
    full_plot = full_path,
    zoom_plot = zoom_path,
    rows = nrow(plot_df),
    gapfilled_rows = sum(plot_df$storage_flux_is_gapfilled, na.rm = TRUE)
  )
}


csv_files <- list.files(
  storage_dir,
  pattern = "_storage_flux[.]csv$",
  full.names = TRUE
)

if (length(csv_files) == 0) {
  stop("No *_storage_flux.csv files found in ", storage_dir, ".", call. = FALSE)
}

message("Writing storage-flux figures to ", figures_dir)
message("Found ", length(csv_files), " storage CSV files.")

plot_summary <- bind_rows(lapply(csv_files, plot_site_file, figures_dir = figures_dir))

summary_csv <- file.path(figures_dir, "storage_gapfilled_plot_summary.csv")
utils::write.csv(plot_summary, summary_csv, row.names = FALSE, na = "")

message("Done. Wrote ", nrow(plot_summary), " site plot summaries to ", summary_csv)
