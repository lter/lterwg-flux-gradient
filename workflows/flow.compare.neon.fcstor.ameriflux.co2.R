## --------------------------------------------- ##
# Compare NEON aligned FC_stor_interp to AmeriFlux SC -----
## --------------------------------------------- ##

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(readr)
library(scales)
library(tibble)


aligned_file <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Aligned_Concentrations/ABBY/ABBY_aligned_conc_flux_30min.RData"
ameriflux_file <- "/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux/Files/AMF_US-xAB_BASE_HH_10-5.csv"
output_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/AmeriFlux_comparison/ABBY_CO2_NEON_FC_stor_interp"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  aligned_file <- args[[1]]
}
if (length(args) >= 2) {
  ameriflux_file <- args[[2]]
}
if (length(args) >= 3) {
  output_dir <- args[[3]]
}


parse_fluxnet_time <- function(x) {
  lubridate::ymd_hm(as.character(x), tz = "GMT")
}


parse_neon_time <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd"),
    tz = "GMT"
  )
}


safe_cor <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  stats::cor(x[ok], y[ok])
}


safe_rmse <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(NA_real_)
  }
  sqrt(mean((x[ok] - y[ok])^2))
}


base_header <- function(base_file) {
  header_line <- readLines(base_file, n = 20)
  header_line <- header_line[!grepl("^#", header_line)][[1]]
  strsplit(header_line, ",", fixed = TRUE)[[1]]
}


read_ameriflux_sc <- function(base_file) {
  header <- base_header(base_file)
  needed <- c("TIMESTAMP_START", "TIMESTAMP_END", "SC")
  missing_cols <- setdiff(needed, header)

  if (length(missing_cols) > 0) {
    stop(
      "Missing required AmeriFlux columns in ",
      base_file,
      ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  col_classes <- rep("NULL", length(header))
  names(col_classes) <- header
  col_classes[needed] <- "character"

  utils::read.csv(
    base_file,
    comment.char = "#",
    colClasses = col_classes,
    check.names = FALSE
  ) %>%
    transmute(
      time_halfhour = parse_fluxnet_time(TIMESTAMP_START),
      ameriflux_storage_flux = na_if(as.numeric(SC), -9999)
    )
}


read_neon_fc_stor <- function(aligned_rdata) {
  env <- new.env(parent = emptyenv())
  load(aligned_rdata, envir = env)

  if (!exists("min30Diff.list", envir = env)) {
    stop("No `min30Diff.list` object found in ", aligned_rdata, ".", call. = FALSE)
  }
  if (!"CO2" %in% names(env$min30Diff.list)) {
    stop("No CO2 table found in `min30Diff.list` from ", aligned_rdata, ".", call. = FALSE)
  }

  co2 <- env$min30Diff.list$CO2
  needed <- c("timeMid", "FC_stor_interp")
  missing_cols <- setdiff(needed, names(co2))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required NEON aligned columns in ",
      aligned_rdata,
      ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  co2 %>%
    transmute(
      time_halfhour = lubridate::round_date(parse_neon_time(timeMid), unit = "30 minutes"),
      neon_fc_stor_interp = as.numeric(FC_stor_interp)
    ) %>%
    group_by(time_halfhour) %>%
    summarise(
      neon_fc_stor_interp = mean(neon_fc_stor_interp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      neon_fc_stor_interp = ifelse(is.nan(neon_fc_stor_interp), NA_real_, neon_fc_stor_interp),
      neon_fc_stor_ameriflux_sign = -neon_fc_stor_interp
    )
}


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

neon_fc_stor <- read_neon_fc_stor(aligned_file)
ameriflux_sc <- read_ameriflux_sc(ameriflux_file)

matched <- neon_fc_stor %>%
  inner_join(ameriflux_sc, by = "time_halfhour") %>%
  mutate(
    diff_neon_minus_ameriflux = neon_fc_stor_ameriflux_sign - ameriflux_storage_flux
  ) %>%
  arrange(time_halfhour)

paired <- matched %>%
  filter(!is.na(neon_fc_stor_ameriflux_sign), !is.na(ameriflux_storage_flux))

if (nrow(paired) == 0) {
  stop("No non-missing paired NEON FC_stor_interp/AmeriFlux SC records were found.", call. = FALSE)
}

summary_stats <- tibble(
  neon_site = "ABBY",
  ameriflux_site = "US-xAB",
  n_joined = nrow(matched),
  n_paired = nrow(paired),
  first_paired_time = min(paired$time_halfhour),
  last_paired_time = max(paired$time_halfhour),
  neon_mean = mean(paired$neon_fc_stor_ameriflux_sign),
  ameriflux_mean = mean(paired$ameriflux_storage_flux),
  bias_neon_minus_ameriflux = mean(paired$diff_neon_minus_ameriflux),
  rmse = safe_rmse(paired$neon_fc_stor_ameriflux_sign, paired$ameriflux_storage_flux),
  correlation = safe_cor(paired$neon_fc_stor_ameriflux_sign, paired$ameriflux_storage_flux),
  neon_storage_source = "aligned min30Diff.list CO2 FC_stor_interp; sign flipped to AmeriFlux SC convention"
)

matched_file <- file.path(output_dir, "ABBY_CO2_NEON_FC_stor_interp_AmeriFlux_matched.csv")
summary_file <- file.path(output_dir, "ABBY_CO2_NEON_FC_stor_interp_AmeriFlux_summary.csv")
png_file <- file.path(output_dir, "ABBY_CO2_NEON_FC_stor_interp_AmeriFlux_comparison.png")
pdf_file <- file.path(output_dir, "ABBY_CO2_NEON_FC_stor_interp_AmeriFlux_comparison.pdf")

readr::write_csv(matched, matched_file)
readr::write_csv(summary_stats, summary_file)

plot_limit <- quantile(
  abs(c(paired$neon_fc_stor_ameriflux_sign, paired$ameriflux_storage_flux)),
  probs = 0.995,
  na.rm = TRUE
)
plot_limit <- max(plot_limit, 1)

daily <- paired %>%
  mutate(date = as.Date(time_halfhour)) %>%
  group_by(date) %>%
  summarise(
    neon_fc_stor_ameriflux_sign = mean(neon_fc_stor_ameriflux_sign),
    ameriflux_storage_flux = mean(ameriflux_storage_flux),
    diff_neon_minus_ameriflux = mean(diff_neon_minus_ameriflux),
    .groups = "drop"
  )

scatter_label <- paste0(
  "n = ", comma(summary_stats$n_paired),
  "\nr = ", number(summary_stats$correlation, accuracy = 0.001),
  "\nRMSE = ", number(summary_stats$rmse, accuracy = 0.001),
  "\nBias = ", number(summary_stats$bias_neon_minus_ameriflux, accuracy = 0.001)
)

theme_storage <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(hjust = 0, color = "grey30"),
    legend.position = "bottom"
  )

p_scatter <- ggplot(paired, aes(x = ameriflux_storage_flux, y = neon_fc_stor_ameriflux_sign)) +
  geom_hline(yintercept = 0, color = "grey85", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey85", linewidth = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "grey45", linewidth = 0.5) +
  geom_point(alpha = 0.18, size = 0.8, color = "#2F6F73") +
  annotate(
    "label",
    x = -plot_limit,
    y = plot_limit,
    label = scatter_label,
    hjust = 0,
    vjust = 1,
    size = 3.3,
    linewidth = 0.2,
    fill = "white"
  ) +
  coord_equal(xlim = c(-plot_limit, plot_limit), ylim = c(-plot_limit, plot_limit)) +
  labs(
    title = "Half-Hour CO2 Storage Flux",
    subtitle = "NEON aligned FC_stor_interp vs AmeriFlux BASE SC; line shows 1:1",
    x = "AmeriFlux SC (umol m-2 s-1)",
    y = "-NEON FC_stor_interp (umol m-2 s-1)"
  ) +
  theme_storage

p_daily <- ggplot(daily, aes(x = date)) +
  geom_hline(yintercept = 0, color = "grey82", linewidth = 0.4) +
  geom_line(aes(y = ameriflux_storage_flux, color = "AmeriFlux SC"), linewidth = 0.45) +
  geom_line(aes(y = neon_fc_stor_ameriflux_sign, color = "NEON -FC_stor_interp"), linewidth = 0.45) +
  scale_color_manual(values = c("AmeriFlux SC" = "#7A5C2E", "NEON -FC_stor_interp" = "#2F6F73")) +
  labs(
    title = "Daily Mean Storage Flux",
    subtitle = "Daily means of paired half-hours",
    x = NULL,
    y = "CO2 storage flux (umol m-2 s-1)",
    color = NULL
  ) +
  theme_storage

p_diff <- ggplot(paired, aes(x = diff_neon_minus_ameriflux)) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.4) +
  geom_histogram(bins = 80, fill = "#5D6F99", color = "white", linewidth = 0.15) +
  coord_cartesian(xlim = quantile(paired$diff_neon_minus_ameriflux, c(0.005, 0.995), na.rm = TRUE)) +
  labs(
    title = "Difference Distribution",
    subtitle = "NEON minus AmeriFlux",
    x = "Difference (umol m-2 s-1)",
    y = "Half-hours"
  ) +
  theme_storage

caption <- paste0(
  "Matched on AmeriFlux TIMESTAMP_START and rounded NEON timeMid. ",
  "NEON source: CO2 FC_stor_interp from ",
  aligned_file,
  ". FC_stor_interp is sign-flipped here to match AmeriFlux SC correction convention."
)

comparison_plot <- (p_scatter | p_daily) / p_diff +
  plot_layout(heights = c(1, 0.82)) +
  plot_annotation(
    title = "ABBY CO2 Storage Flux: NEON FC_stor_interp vs AmeriFlux",
    subtitle = "Benchmarking the existing NEON storage term before tuning the CH4 reconstruction",
    caption = caption,
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "grey25"),
      plot.caption = element_text(hjust = 0, color = "grey30")
    )
  )

ggsave(png_file, comparison_plot, width = 14, height = 9, dpi = 300)
ggsave(pdf_file, comparison_plot, width = 14, height = 9)

message("Saved matched data: ", matched_file)
message("Saved summary: ", summary_file)
message("Saved figure: ", png_file)
message("Saved figure: ", pdf_file)
print(summary_stats)
