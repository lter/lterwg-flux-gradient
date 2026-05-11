## --------------------------------------------- ##
#      Compare NEON and AmeriFlux Storage Flux -----
## --------------------------------------------- ##
# Purpose:
# Compare NEON CO2 storage flux estimates with AmeriFlux BASE storage flux
# (`SC`) for matching NEON/AmeriFlux tower sites.
#
# Notes:
# - AmeriFlux FLUXNET_SUBSET files do not contain `SC`; this workflow needs
#   AmeriFlux BASE files, either as `AMF_SITE_BASE_HH_*.csv` files or
#   `AMF_SITE_BASE-BADM_*.zip` bundles.
# - Set `download_missing_base <- TRUE` to download missing BASE-BADM files
#   through `amerifluxr::amf_download_base()`. This requires an AmeriFlux
#   account and the `amerifluxr` package.
#
# Optional command-line use:
#   Rscript workflows/flow.compare.ameriflux.storage.R [download_missing_base] [output_dir] [ameriflux_dir] [neon_storage_dir]

rm(list = ls())

if (dir.exists("/tmp/Rlib")) {
  .libPaths(unique(c("/tmp/Rlib", .libPaths())))
}

library(dplyr)
library(fs)
library(lubridate)
library(readr)
library(stringr)
library(tibble)


neon_storage_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux"
ameriflux_dir <- "/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux"
ameriflux_files_dir <- file.path(ameriflux_dir, "Files")
metadata_file <- "/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv"
output_dir <- file.path(neon_storage_dir, "AmeriFlux_comparison")

download_missing_base <- FALSE
ameriflux_user_id <- "smalone"
ameriflux_user_email <- "sparkle.malone@yale.edu"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  download_missing_base <- as.logical(args[[1]])
}
if (length(args) >= 2) {
  output_dir <- args[[2]]
}
if (length(args) >= 3) {
  ameriflux_dir <- args[[3]]
  ameriflux_files_dir <- file.path(ameriflux_dir, "Files")
}
if (length(args) >= 4) {
  neon_storage_dir <- args[[4]]
}


parse_fluxnet_time <- function(x) {
  lubridate::ymd_hm(as.character(x), tz = "GMT")
}


safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
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


sanitize_missing <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ na_if(.x, -9999)))
}


base_header <- function(base_file) {
  header_line <- readLines(base_file, n = 20)
  header_line <- header_line[!grepl("^#", header_line)][[1]]
  strsplit(header_line, ",", fixed = TRUE)[[1]]
}


read_site_map <- function(metadata_file) {
  readr::read_csv(metadata_file, show_col_types = FALSE) %>%
    rename_with(make.names) %>%
    transmute(
      ameriflux_site = Site.Id.AF,
      neon_site = Site_Id.NEON
    ) %>%
    filter(!is.na(ameriflux_site), !is.na(neon_site))
}


find_base_csv <- function(ameriflux_files_dir, ameriflux_site) {
  files <- fs::dir_ls(
    ameriflux_files_dir,
    regexp = paste0("AMF_", ameriflux_site, "_BASE_(HH|HR)_.*\\.csv$"),
    recurse = FALSE,
    type = "file"
  )

  if (length(files) == 0) {
    return(NA_character_)
  }

  files[[which.max(file.info(files)$mtime)]]
}


find_base_zip <- function(ameriflux_dir, ameriflux_site) {
  files <- fs::dir_ls(
    ameriflux_dir,
    regexp = paste0("AMF_", ameriflux_site, "_BASE-BADM_.*\\.zip$"),
    recurse = FALSE,
    type = "file"
  )

  if (length(files) == 0) {
    return(NA_character_)
  }

  files[[which.max(file.info(files)$mtime)]]
}


unzip_base_csv <- function(base_zip, ameriflux_files_dir) {
  zip_files <- utils::unzip(base_zip, list = TRUE)
  base_csv <- zip_files$Name[
    grepl("_BASE_(HH|HR)_.*\\.csv$", zip_files$Name)
  ][[1]]

  utils::unzip(base_zip, files = base_csv, exdir = ameriflux_files_dir, overwrite = TRUE)
  file.path(ameriflux_files_dir, basename(base_csv))
}


download_base_files <- function(site_map, ameriflux_dir) {
  if (!download_missing_base) {
    return(invisible(NULL))
  }

  if (!requireNamespace("amerifluxr", quietly = TRUE)) {
    stop(
      "`amerifluxr` is not installed. Install it before setting ",
      "`download_missing_base <- TRUE`.",
      call. = FALSE
    )
  }

  missing_sites <- site_map %>%
    mutate(
      base_csv = vapply(
        ameriflux_site,
        function(site) find_base_csv(ameriflux_files_dir, site),
        character(1)
      ),
      base_zip = vapply(
        ameriflux_site,
        function(site) find_base_zip(ameriflux_dir, site),
        character(1)
      )
    ) %>%
    filter(is.na(base_csv), is.na(base_zip)) %>%
    pull(ameriflux_site)

  if (length(missing_sites) == 0) {
    return(invisible(NULL))
  }

  amerifluxr::amf_download_base(
    user_id = ameriflux_user_id,
    user_email = ameriflux_user_email,
    site_id = missing_sites,
    data_product = "BASE-BADM",
    data_policy = "CCBY4.0",
    agree_policy = TRUE,
    intended_use = "model",
    intended_use_text = "Compare AmeriFlux and NEON storage flux at NEON sites",
    verbose = TRUE,
    out_dir = ameriflux_dir
  )
}


get_base_file <- function(ameriflux_site) {
  base_csv <- find_base_csv(ameriflux_files_dir, ameriflux_site)
  if (!is.na(base_csv)) {
    return(base_csv)
  }

  base_zip <- find_base_zip(ameriflux_dir, ameriflux_site)
  if (!is.na(base_zip)) {
    return(unzip_base_csv(base_zip, ameriflux_files_dir))
  }

  NA_character_
}


read_ameriflux_sc <- function(ameriflux_site, neon_site) {
  base_file <- get_base_file(ameriflux_site)

  if (is.na(base_file)) {
    return(tibble(
      ameriflux_site = ameriflux_site,
      neon_site = neon_site,
      status = "missing_base_file"
    ))
  }

  header <- base_header(base_file)
  sc_cols <- header[header == "SC" | grepl("^SC_", header)]

  if (length(sc_cols) == 0) {
    return(tibble(
      ameriflux_site = ameriflux_site,
      neon_site = neon_site,
      status = "missing_sc_column",
      base_file = base_file
    ))
  }

  cols <- c("TIMESTAMP_START", "TIMESTAMP_END", sc_cols)
  col_classes <- rep("NULL", length(header))
  names(col_classes) <- header
  col_classes[cols] <- "character"

  base_df <- utils::read.csv(
    base_file,
    comment.char = "#",
    colClasses = col_classes,
    check.names = FALSE
  )

  sc_df <- lapply(base_df[sc_cols], as.numeric) %>% as.data.frame()
  sc_matrix <- as.matrix(sc_df)
  sc_matrix[sc_matrix == -9999] <- NA_real_
  ameriflux_storage_flux <- rowMeans(sc_matrix, na.rm = TRUE)
  ameriflux_storage_flux[is.nan(ameriflux_storage_flux)] <- NA_real_

  tibble(
    ameriflux_site = ameriflux_site,
    neon_site = neon_site,
    time_start = parse_fluxnet_time(base_df$TIMESTAMP_START),
    time_end = parse_fluxnet_time(base_df$TIMESTAMP_END),
    ameriflux_storage_flux = ameriflux_storage_flux,
    ameriflux_storage_cols = paste(sc_cols, collapse = ";"),
    base_file = base_file,
    status = "ok"
  ) %>%
    mutate(time_halfhour = time_end) %>%
    select(
      ameriflux_site,
      neon_site,
      time_halfhour,
      ameriflux_storage_flux,
      ameriflux_storage_cols,
      base_file,
      status
    )
}


read_neon_storage <- function(neon_site) {
  neon_file <- file.path(neon_storage_dir, paste0(neon_site, "_storage_flux.csv"))

  if (!file.exists(neon_file)) {
    return(tibble(neon_site = neon_site, status = "missing_neon_storage_file"))
  }

  readr::read_csv(neon_file, show_col_types = FALSE) %>%
    filter(gas == "CO2") %>%
    mutate(
      neon_site = neon_site,
      time_halfhour = lubridate::ymd_hms(time_halfhour, tz = "GMT"),
      storage_flux = as.numeric(storage_flux)
    ) %>%
    group_by(neon_site, time_halfhour) %>%
    summarise(
      neon_storage_flux = safe_mean(storage_flux),
      neon_n_storage_records = sum(!is.na(storage_flux)),
      .groups = "drop"
    )
}


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
download_base_files(read_site_map(metadata_file), ameriflux_dir)

site_map <- read_site_map(metadata_file) %>%
  filter(file.exists(file.path(neon_storage_dir, paste0(neon_site, "_storage_flux.csv"))))

message("Comparing storage flux for ", nrow(site_map), " mapped NEON/AmeriFlux sites...")

ameriflux_status <- vector("list", nrow(site_map))
comparison_list <- vector("list", nrow(site_map))

for (i in seq_len(nrow(site_map))) {
  ameriflux_site <- site_map$ameriflux_site[[i]]
  neon_site <- site_map$neon_site[[i]]

  message("Working on ", neon_site, " / ", ameriflux_site, "...")

  ameriflux_sc <- read_ameriflux_sc(ameriflux_site, neon_site)
  if (!"base_file" %in% names(ameriflux_sc)) {
    ameriflux_sc$base_file <- NA_character_
  }
  ameriflux_status[[i]] <- ameriflux_sc %>%
    distinct(ameriflux_site, neon_site, status, base_file)

  if (!"time_halfhour" %in% names(ameriflux_sc)) {
    comparison_list[[i]] <- tibble()
    next
  }

  neon_storage <- read_neon_storage(neon_site)

  comparison_list[[i]] <- neon_storage %>%
    inner_join(
      ameriflux_sc %>% filter(status == "ok"),
      by = c("neon_site", "time_halfhour")
    ) %>%
    mutate(
      diff_neon_minus_ameriflux = neon_storage_flux - ameriflux_storage_flux
    )
}

comparison <- bind_rows(comparison_list)
status <- bind_rows(ameriflux_status)

if (nrow(comparison) == 0) {
  summary <- site_map %>%
    mutate(
      n_matched = 0L,
      neon_mean = NA_real_,
      ameriflux_mean = NA_real_,
      bias_neon_minus_ameriflux = NA_real_,
      rmse = NA_real_,
      correlation = NA_real_
    ) %>%
    left_join(status, by = c("neon_site", "ameriflux_site")) %>%
    arrange(neon_site)
} else {
  summary <- comparison %>%
    group_by(neon_site, ameriflux_site) %>%
    summarise(
      n_matched = sum(!is.na(neon_storage_flux) & !is.na(ameriflux_storage_flux)),
      neon_mean = safe_mean(neon_storage_flux),
      ameriflux_mean = safe_mean(ameriflux_storage_flux),
      bias_neon_minus_ameriflux = safe_mean(diff_neon_minus_ameriflux),
      rmse = safe_rmse(neon_storage_flux, ameriflux_storage_flux),
      correlation = safe_cor(neon_storage_flux, ameriflux_storage_flux),
      .groups = "drop"
    ) %>%
    right_join(site_map, by = c("neon_site", "ameriflux_site")) %>%
    left_join(status, by = c("neon_site", "ameriflux_site")) %>%
    arrange(neon_site)
}

readr::write_csv(
  comparison,
  file.path(output_dir, "NEON_AmeriFlux_storage_flux_matched.csv")
)

readr::write_csv(
  summary,
  file.path(output_dir, "NEON_AmeriFlux_storage_flux_summary.csv")
)

readr::write_csv(
  status,
  file.path(output_dir, "NEON_AmeriFlux_storage_flux_base_status.csv")
)

message("Done. Wrote comparison outputs to ", output_dir)
