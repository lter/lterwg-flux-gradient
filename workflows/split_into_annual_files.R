## -------------------------------------------------- ##
#                  Housekeeping -----
## -------------------------------------------------- ##
# Purpose:
# This script will split the combined 2021-2026 calculated gradient fluxes into annual files for submission to EDI.
# Period 1: Aug 2021 - Dec 2021, period 2: Jan 2022 - Dec 2022, and so on etc. Cutoff time is 00:00:00 UTC.
# 9-min MBR/AE/WP files: by timeMid (UTC midpoint of the paired samples; EC is interpolated to it).
# Ensemble files: by time_start_utc (the half-hour that contains timeMid).
# Storage files: by time_halfhour.
# Rows are copied unchanged.

library(tidyverse)
library(lubridate)

# Set path to root folder that will contain the folders for split annual data 
output_root_path <- "[SET OUTPUT ROOT PATH HERE]"
# Set path to root folder that contains the folder for combined years data (may be the same path as above)
input_root_path <- "[SET INPUT ROOT PATH HERE]"

# Set path to folder that contains Ameriflux_NEON field-sites.csv (will most likely be your working directory)
wd_path <- "[SET WORKING DIRECTORY HERE]"

# Create the output annual folders
for (i in c(2021:2026)){
  dir.create(path = file.path(output_root_path, paste0("NEON_GradientFlux_EDI_", i, "_v3b")), showWarnings = F)
}

# Create folder for diagnostics (to check if all rows are there after the split)
dir.create(path = file.path(output_root_path, "split_diagnostics"), showWarnings = F)

# Grab list of sites
metadata <- read.csv(file.path(wd_path, "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

## -------------------------------------------------- ##
#             Splitting Gradient Fluxes -----
## -------------------------------------------------- ##

# Specify input folder
combined_folder <- file.path(input_root_path, "NEON_GradientFlux_EDI_2021_2026_v3b")

for (w in 1:length(site.list)){
  # Create list to store statistics later
  check_stats <- list()

  # Specify site
  site <- site.list[w]
  message("Now on: ", site, " ", Sys.time())

  # Specify site input folder
  site_input_folder <- file.path(combined_folder, site)

  for (j in c("AE", "MBR", "WP")){
    # Specify data type
    data_type <- j

    # Read in data
    gradient_flux_data <- read_csv(file.path(site_input_folder, paste0(site, "_", data_type, "_9min.df.final.csv")))

    # Create list to store row counts
    check_nrow_sub <- list()
    # Save total row count
    check_nrow_sub[[paste0(data_type, "_total")]] <- nrow(gradient_flux_data)

    for (i in 2021:2026) {
      # Specify target year
      target_year <- i
      # Specify year output folder
      year_output_folder <- file.path(output_root_path, paste0("NEON_GradientFlux_EDI_", target_year, "_v3b"))

      # Create site output folder
      site_output_folder <- file.path(year_output_folder, site)
      dir.create(path = site_output_folder, showWarnings = F)

      # Specify cutoff date and time for the target year
      start_time <- paste0(target_year, "-01-01 00:00:00")
      end_time <- paste0(target_year,"-12-31 23:59:59")

      if (data_type == "MBR"){

        gradient_flux_data_v2 <- gradient_flux_data %>%
          # Create a new column that has the timestamps to check against
          dplyr::mutate(time_check = dplyr::case_when(
            # Use timeMid if it exists
            !is.na(timeMid) ~ timeMid,
            # If not, use match_time,
            !is.na(match_time) ~ match_time,
            # Etc.
            !is.na(timeEnd_A) ~ timeEnd_A,
            !is.na(timeEndA) ~ timeEndA
          ))

      } else {

        gradient_flux_data_v2 <- gradient_flux_data %>%
          # Create a new column that has the timestamps to check against
          dplyr::mutate(time_check = dplyr::case_when(
            # Use timeMid if it exists
            !is.na(timeMid) ~ timeMid,
            # If not, use match_time,
            !is.na(match_time) ~ match_time,
            # Etc.
            !is.na(timeEnd_A) ~ timeEnd_A
          ))
      }

      # Subset
      gradient_flux_data_sub <- gradient_flux_data_v2 %>%
        dplyr::filter(time_check >= ymd_hms(start_time) & time_check <= ymd_hms(end_time)) %>%
        dplyr::select(-time_check)

      # Save subset row count
      # check_nrow_sub is a list of stats for either AE, MBR, or WP
      check_nrow_sub[[paste0(data_type, "_", target_year)]] <- nrow(gradient_flux_data_sub)

      # Rename first column as blank (this is how it was in the original files)
      colnames(gradient_flux_data_sub)[1] <- ""

      # Export
      write.csv(gradient_flux_data_sub, file.path(site_output_folder, paste0(site, "_", data_type, "_9min.df.final.csv")), row.names = F)
    }

    # Convert row count list to dataframe
    check_nrow_sub_df <- as.data.frame(check_nrow_sub) %>%
      tidyr::pivot_longer(everything(), names_to = "data_type", values_to = "nrow") %>%
      dplyr::mutate(NEON_site = site, .before = data_type) %>%
      dplyr::mutate(total_or_sub = dplyr::case_when(
        stringr::str_detect(data_type, "total") ~ "total",
        T ~ "sub"
      ))

    # Save row count dataframe to statistics list
    # check_stats is a list of stats for AE, MBR, and WP
    check_stats[[paste0(site, "_", data_type)]] <- check_nrow_sub_df
  }

  # Convert statistics list to dataframe
  check_stats_df <- check_stats %>%
    list_rbind() %>%
    tidyr::separate_wider_delim(data_type, delim = "_", names = c("method", "partition")) %>%
    dplyr::group_by(NEON_site, method, total_or_sub) %>%
    dplyr::mutate(sum_total = sum(nrow))

  # Export statistics dataframe
  write.csv(check_stats_df, file.path(output_root_path, "split_diagnostics", paste0(site, "_GF_split_diagnostics.csv")), row.names = F)

}


## -------------------------------------------------- ##
#             Splitting Ensemble Fluxes -----
## -------------------------------------------------- ##

# Specify input folder
combined_folder <- file.path(input_root_path, "NEON_GradientFlux_EDI_2021_2026_v3b")

for (x in 1:length(site.list)){
  # Create list to store statistics later
  check_stats <- list()

  # Specify site
  site <- site.list[x]
  message("Now on: ", site, " ", Sys.time())

  # Specify site input folder
  site_input_folder <- file.path(combined_folder, site)

  if (file.exists(file.path(site_input_folder, paste0(site, "_GF_ENSEMBLE_30min.csv")))){

    # Read in data
    # Specify column types to avoid parsing errors
    ensemble_data <- read_csv(file.path(site_input_folder, paste0(site, "_GF_ENSEMBLE_30min.csv")),
                              col_types = cols(site = col_character(),
                                               gas = col_character(),
                                               time_start_utc = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               time_end_utc = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               time_start_local = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               time_end_local = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               FG_ENSEMBLE = col_double(),
                                               FG_ENSEMBLE_SD = col_double(),
                                               FG_ENSEMBLE_NPAIRS = col_double(),
                                               FG_ENSEMBLE_NVALUES = col_double(),
                                               FG_ENSEMBLE_APPROACHES = col_character(),
                                               FG_ENSEMBLE_FLAG = col_character(),
                                               EC_mean = col_double(),
                                               EC_30min = col_double(),
                                               ensemble_method = col_character()))

    # Save total row count
    check_stats[["ensemble_total"]] <- nrow(ensemble_data)

    for (m in 2021:2026){
      # Specify target year
      target_year <- m
      # Specify year output folder
      year_output_folder <- file.path(output_root_path, paste0("NEON_GradientFlux_EDI_", target_year, "_v3b"))

      # Create site output folder
      site_output_folder <- file.path(year_output_folder, site)
      dir.create(path = site_output_folder, showWarnings = F)

      # Specify cutoff date and time for the target year
      start_time <- paste0(target_year, "-01-01 00:00:00")
      end_time <- paste0(target_year,"-12-31 23:59:59")

      # Subset
      ensemble_data_sub <- ensemble_data %>%
        dplyr::filter(time_start_utc >= ymd_hms(start_time) & time_start_utc <= ymd_hms(end_time))

      # Save subset row count
      check_stats[[paste0("ensemble_", target_year)]] <- nrow(ensemble_data_sub)

      # Export
      write.csv(ensemble_data_sub, file.path(site_output_folder, paste0(site, "_GF_ENSEMBLE_30min.csv")), row.names = F)
    }

    # Convert row count list to statistics dataframe
    check_stats_df <- as.data.frame(check_stats) %>%
      tidyr::pivot_longer(everything(), names_to = "data_type", values_to = "nrow") %>%
      dplyr::mutate(NEON_site = site, .before = data_type) %>%
      dplyr::mutate(total_or_sub = dplyr::case_when(
        stringr::str_detect(data_type, "total") ~ "total",
        T ~ "sub"
      )) %>%
      tidyr::separate_wider_delim(data_type, delim = "_", names = c("method", "partition")) %>%
      dplyr::group_by(NEON_site, method, total_or_sub) %>%
      dplyr::mutate(sum_total = sum(nrow))

    # Export statistics dataframe
    write.csv(check_stats_df, file.path(output_root_path, "split_diagnostics", paste0(site, "_ensemble_split_diagnostics.csv")), row.names = F)


  } else {
    message("Ensemble file does not exist for site ", site)
    next
  }

}

## -------------------------------------------------- ##
#             Splitting Storage Fluxes -----
## -------------------------------------------------- ##

# Specify input folder
combined_folder <- file.path(input_root_path, "NEON_GradientFlux_EDI_2021_2026_v3b")

for (y in 1:length(site.list)){
  # Create list to store statistics later
  check_stats <- list()

  # Specify site
  site <- site.list[y]
  message("Now on: ", site, " ", Sys.time())

  # Specify site input folder
  site_input_folder <- file.path(combined_folder, site)

  # Read in data
  storage_data <- read_csv(file.path(site_input_folder, paste0(site, "_STORAGE_FLUX_30min.csv")))

  # Save total row count
  check_stats[["storage_total"]] <- nrow(storage_data)

  for (n in 2021:2026){
    # Specify target year
    target_year <- n
    # Specify year output folder
    year_output_folder <- file.path(output_root_path, paste0("NEON_GradientFlux_EDI_", target_year, "_v3b"))

    # Create site output folder
    site_output_folder <- file.path(year_output_folder, site)
    dir.create(path = site_output_folder, showWarnings = F)

    # Specify cutoff date and time for the target year
    start_time <- paste0(target_year, "-01-01 00:00:00")
    end_time <- paste0(target_year,"-12-31 23:59:59")

    # Subset
    storage_data_sub <- storage_data %>%
      dplyr::filter(time_halfhour >= ymd_hms(start_time) & time_halfhour <= ymd_hms(end_time))

    # Save subset row count
    check_stats[[paste0("storage_", target_year)]] <- nrow(storage_data_sub)

    # Export
    write.csv(storage_data_sub, file.path(site_output_folder, paste0(site, "_STORAGE_FLUX_30min.csv")), row.names = F)
  }

  # Convert row count list to statistics dataframe
  check_stats_df <- as.data.frame(check_stats) %>%
    tidyr::pivot_longer(everything(), names_to = "data_type", values_to = "nrow") %>%
    dplyr::mutate(NEON_site = site, .before = data_type) %>%
    dplyr::mutate(total_or_sub = dplyr::case_when(
      stringr::str_detect(data_type, "total") ~ "total",
      T ~ "sub"
    )) %>%
    tidyr::separate_wider_delim(data_type, delim = "_", names = c("method", "partition")) %>%
    dplyr::group_by(NEON_site, method, total_or_sub) %>%
    dplyr::mutate(sum_total = sum(nrow))

  # Export statistics dataframe
  write.csv(check_stats_df, file.path(output_root_path, "split_diagnostics", paste0(site, "_storage_split_diagnostics.csv")), row.names = F)

}
