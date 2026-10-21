## --------------------------------------------- ##
#   Plot NEON and AmeriFlux Storage Comparison -----
## --------------------------------------------- ##
# Purpose:
# Create a site-level figure summarizing the NEON CO2 storage flux comparison
# against AmeriFlux BASE `SC`.

rm(list = ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(scales)
library(tibble)


comparison_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/AmeriFlux_comparison"
summary_file <- file.path(comparison_dir, "NEON_AmeriFlux_storage_flux_summary.csv")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  comparison_dir <- args[[1]]
  summary_file <- file.path(comparison_dir, "NEON_AmeriFlux_storage_flux_summary.csv")
}

summary_df <- readr::read_csv(summary_file, show_col_types = FALSE) %>%
  filter(status == "ok", n_matched > 0) %>%
  mutate(
    site_bias_order = reorder(neon_site, bias_neon_minus_ameriflux),
    site_rmse_order = reorder(neon_site, rmse),
    site_corr_order = reorder(neon_site, correlation)
  )

if (nrow(summary_df) == 0) {
  stop("No matched storage-flux comparison rows found in ", summary_file, call. = FALSE)
}

total_matched <- sum(summary_df$n_matched, na.rm = TRUE)
median_rmse <- median(summary_df$rmse, na.rm = TRUE)
median_bias <- median(summary_df$bias_neon_minus_ameriflux, na.rm = TRUE)
median_corr <- median(summary_df$correlation, na.rm = TRUE)

caption_text <- paste0(
  "Each point or bar is one NEON/AmeriFlux site pair. ",
  "Matched half-hours: ", comma(total_matched), ". ",
  "Median bias: ", number(median_bias, accuracy = 0.001), "; ",
  "median RMSE: ", number(median_rmse, accuracy = 0.001), "; ",
  "median r: ", number(median_corr, accuracy = 0.001), "."
)

theme_storage <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, color = "grey30")
  )

mean_min <- min(c(summary_df$neon_mean, summary_df$ameriflux_mean), na.rm = TRUE)
mean_max <- max(c(summary_df$neon_mean, summary_df$ameriflux_mean), na.rm = TRUE)

p_mean <- ggplot(summary_df, aes(x = ameriflux_mean, y = neon_mean)) +
  geom_abline(slope = 1, intercept = 0, color = "grey55", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey85", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey85", linewidth = 0.4) +
  geom_point(aes(size = n_matched), color = "#2F6F73", alpha = 0.78) +
  coord_equal(xlim = c(mean_min, mean_max), ylim = c(mean_min, mean_max)) +
  scale_size_continuous(labels = comma, range = c(1.8, 6), name = "Matched half-hours") +
  labs(
    title = "Mean Storage Flux",
    subtitle = "Site means; line shows 1:1",
    x = "AmeriFlux SC (umol m-2 s-1)",
    y = "NEON storage flux (umol m-2 s-1)"
  ) +
  theme_storage

p_bias <- ggplot(summary_df, aes(x = site_bias_order, y = bias_neon_minus_ameriflux)) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.4) +
  geom_col(aes(fill = bias_neon_minus_ameriflux > 0), width = 0.78, show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("#B05A4A", "#2F6F73")) +
  labs(
    title = "Bias By Site",
    subtitle = "NEON minus AmeriFlux",
    x = NULL,
    y = "Bias (umol m-2 s-1)"
  ) +
  theme_storage +
  theme(axis.text.y = element_text(size = 7))

p_rmse <- ggplot(summary_df, aes(x = site_rmse_order, y = rmse)) +
  geom_col(fill = "#5D6F99", width = 0.78) +
  coord_flip() +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = "RMSE By Site",
    subtitle = "Half-hour paired storage fluxes",
    x = NULL,
    y = "RMSE (umol m-2 s-1)"
  ) +
  theme_storage +
  theme(axis.text.y = element_text(size = 7))

p_corr <- ggplot(summary_df, aes(x = site_corr_order, y = correlation)) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.4) +
  geom_col(fill = "#8A6E3E", width = 0.78) +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(
    title = "Correlation By Site",
    subtitle = "Pearson r for paired half-hours",
    x = NULL,
    y = "Correlation"
  ) +
  theme_storage +
  theme(axis.text.y = element_text(size = 7))

summary_plot <- (p_mean | p_bias) / (p_rmse | p_corr) +
  plot_annotation(
    title = "NEON vs AmeriFlux CO2 Storage Flux",
    subtitle = "NEON profile-derived storage flux compared with AmeriFlux BASE SC",
    caption = caption_text,
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "grey25"),
      plot.caption = element_text(hjust = 0, color = "grey30")
    )
  )

png_out <- file.path(comparison_dir, "NEON_AmeriFlux_storage_flux_summary.png")
pdf_out <- file.path(comparison_dir, "NEON_AmeriFlux_storage_flux_summary.pdf")

ggsave(png_out, summary_plot, width = 14, height = 10, dpi = 300)
ggsave(pdf_out, summary_plot, width = 14, height = 10)

message("Saved ", png_out)
message("Saved ", pdf_out)
