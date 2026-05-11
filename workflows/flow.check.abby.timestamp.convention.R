## --------------------------------------------- ##
# ABBY CO2 timestamp convention sensitivity -----
## --------------------------------------------- ##

library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(readr)
library(scales)
library(tibble)


neon_file <- "/tmp/neon_storage_abby_ameriflux_sign_test/ABBY_storage_flux.csv"
ameriflux_file <- "/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux/Files/AMF_US-xAB_BASE_HH_10-5.csv"
output_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/AmeriFlux_comparison/ABBY_CO2_timestamp_convention"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  neon_file <- args[[1]]
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


parse_storage_time <- function(x) {
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
      ameriflux_start = parse_fluxnet_time(TIMESTAMP_START),
      ameriflux_end = parse_fluxnet_time(TIMESTAMP_END),
      ameriflux_midpoint = ameriflux_start + (ameriflux_end - ameriflux_start) / 2,
      ameriflux_storage_flux = na_if(as.numeric(SC), -9999)
    )
}


read_neon_co2 <- function(storage_file) {
  readr::read_csv(storage_file, show_col_types = FALSE) %>%
    filter(gas == "CO2") %>%
    transmute(
      neon_time = parse_storage_time(time_halfhour),
      neon_storage_flux = as.numeric(storage_flux),
      neon_storage_flux_column_rate = if ("storage_flux_column_rate" %in% names(.)) {
        as.numeric(storage_flux_column_rate)
      } else {
        NA_real_
      },
      storage_qf = if ("storage_qf" %in% names(.)) {
        as.integer(storage_qf)
      } else {
        NA_integer_
      }
    ) %>%
    group_by(neon_time) %>%
    summarise(
      neon_storage_flux = mean(neon_storage_flux, na.rm = TRUE),
      neon_storage_flux_column_rate = mean(neon_storage_flux_column_rate, na.rm = TRUE),
      storage_qf = suppressWarnings(max(storage_qf, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      neon_storage_flux = ifelse(is.nan(neon_storage_flux), NA_real_, neon_storage_flux),
      neon_storage_flux_column_rate = ifelse(is.nan(neon_storage_flux_column_rate), NA_real_, neon_storage_flux_column_rate),
      storage_qf = ifelse(is.infinite(storage_qf), NA_integer_, storage_qf)
    )
}


metric_row <- function(neon_df, ameriflux_df, convention, shift_min) {
  amf_time_col <- paste0("ameriflux_", convention)
  amf_df <- ameriflux_df %>%
    transmute(
      compare_time = .data[[amf_time_col]],
      ameriflux_storage_flux = ameriflux_storage_flux
    )

  joined <- neon_df %>%
    transmute(
      compare_time = neon_time + minutes(shift_min),
      neon_storage_flux = neon_storage_flux
    ) %>%
    inner_join(amf_df, by = "compare_time")

  ok <- !is.na(joined$neon_storage_flux) & !is.na(joined$ameriflux_storage_flux)

  tibble(
    ameriflux_timestamp_convention = convention,
    neon_shift_min = shift_min,
    n_joined = nrow(joined),
    n_paired = sum(ok),
    bias_neon_minus_ameriflux = ifelse(any(ok), mean(joined$neon_storage_flux[ok] - joined$ameriflux_storage_flux[ok]), NA_real_),
    rmse = safe_rmse(joined$neon_storage_flux, joined$ameriflux_storage_flux),
    correlation = safe_cor(joined$neon_storage_flux, joined$ameriflux_storage_flux)
  )
}


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

neon_co2 <- read_neon_co2(neon_file)
ameriflux_sc <- read_ameriflux_sc(ameriflux_file)
conventions <- c("start", "midpoint", "end")
shifts_min <- seq(-180, 180, by = 15)

timestamp_results <- bind_rows(lapply(conventions, function(convention) {
  bind_rows(lapply(shifts_min, function(shift_min) {
    metric_row(neon_co2, ameriflux_sc, convention, shift_min)
  }))
})) %>%
  mutate(
    plausible_timestamp_choice = case_when(
      ameriflux_timestamp_convention == "end" & neon_shift_min == 0 ~ "current_END_match",
      ameriflux_timestamp_convention == "start" & neon_shift_min == 0 ~ "direct_START_match",
      ameriflux_timestamp_convention == "midpoint" & neon_shift_min == 0 ~ "direct_MIDPOINT_match",
      ameriflux_timestamp_convention == "end" & neon_shift_min == -30 ~ "NEON_labels_START_if_AMF_END",
      ameriflux_timestamp_convention == "start" & neon_shift_min == 30 ~ "NEON_labels_END_if_AMF_START",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(rmse)

ranked_file <- file.path(output_dir, "ABBY_CO2_timestamp_convention_sensitivity.csv")
best_file <- file.path(output_dir, "ABBY_CO2_timestamp_convention_best.csv")
png_file <- file.path(output_dir, "ABBY_CO2_timestamp_convention_sensitivity.png")
pdf_file <- file.path(output_dir, "ABBY_CO2_timestamp_convention_sensitivity.pdf")

readr::write_csv(timestamp_results, ranked_file)
readr::write_csv(slice_head(timestamp_results, n = 20), best_file)

best_by_rmse <- slice_min(timestamp_results, rmse, n = 1, with_ties = FALSE)
current_end <- timestamp_results %>%
  filter(ameriflux_timestamp_convention == "end", neon_shift_min == 0)

theme_storage <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(hjust = 0, color = "grey30"),
    legend.position = "bottom"
  )

plot_df <- timestamp_results %>%
  filter(!is.na(rmse), !is.na(correlation))

label_text <- paste0(
  "Current END, 0 min: RMSE ",
  number(current_end$rmse, accuracy = 0.001),
  ", r ",
  number(current_end$correlation, accuracy = 0.001),
  "\nBest tested: ",
  best_by_rmse$ameriflux_timestamp_convention,
  ", shift ",
  best_by_rmse$neon_shift_min,
  " min; RMSE ",
  number(best_by_rmse$rmse, accuracy = 0.001),
  ", r ",
  number(best_by_rmse$correlation, accuracy = 0.001)
)

p_rmse <- ggplot(plot_df, aes(x = neon_shift_min, y = rmse, color = ameriflux_timestamp_convention)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  annotate(
    "label",
    x = min(shifts_min),
    y = max(plot_df$rmse, na.rm = TRUE),
    label = label_text,
    hjust = 0,
    vjust = 1,
    size = 3.2,
    linewidth = 0.2,
    fill = "white"
  ) +
  scale_color_manual(values = c(start = "#6C6AA8", midpoint = "#567D46", end = "#B36B3F")) +
  labs(
    title = "RMSE By Timestamp Convention",
    x = "NEON timestamp shift (minutes)",
    y = "RMSE (umol m-2 s-1)",
    color = "AmeriFlux timestamp"
  ) +
  theme_storage

p_cor <- ggplot(plot_df, aes(x = neon_shift_min, y = correlation, color = ameriflux_timestamp_convention)) +
  geom_hline(yintercept = 0, color = "grey75", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  scale_color_manual(values = c(start = "#6C6AA8", midpoint = "#567D46", end = "#B36B3F")) +
  labs(
    title = "Correlation By Timestamp Convention",
    x = "NEON timestamp shift (minutes)",
    y = "Pearson r",
    color = "AmeriFlux timestamp"
  ) +
  theme_storage

comparison_plot <- p_rmse / p_cor +
  patchwork::plot_annotation(
    title = "ABBY CO2 Storage Flux Timestamp Convention Check",
    subtitle = "NEON storage_flux compared against AmeriFlux SC under start/end/midpoint timestamp choices",
    caption = paste0(
      "NEON file: ",
      neon_file,
      "\nAmeriFlux file: ",
      ameriflux_file
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "grey25"),
      plot.caption = element_text(hjust = 0, color = "grey30")
    )
  )

ggsave(png_file, comparison_plot, width = 12, height = 9, dpi = 300)
ggsave(pdf_file, comparison_plot, width = 12, height = 9)

message("Saved ranked sensitivity results: ", ranked_file)
message("Saved top-20 sensitivity results: ", best_file)
message("Saved figure: ", png_file)
message("Saved figure: ", pdf_file)
print(slice_head(timestamp_results, n = 20))
