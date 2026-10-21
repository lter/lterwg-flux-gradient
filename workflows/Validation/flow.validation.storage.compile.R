## --------------------------------------------- ##
#     Compile Validation Storage RData -----
## --------------------------------------------- ##
# Purpose:
# Reads individual per-site validation storage flux CSVs already on disk and
# assembles them into Validation_storage.RData, matching the structure
# produced by flow.validation.storage.R (a named list of tibbles, one element
# per site).
#
# Run this instead of the full flow.validation.storage.R when the per-site
# CSVs have already been generated.
#
# Input:
#   /Volumes/MaloneLab/Research/FluxGradient/Validation_Sites/Storage_Flux/SITE_storage_flux.csv
#
# Output:
#   /Volumes/MaloneLab/Research/FluxGradient/Validation_Sites/Storage_Flux/Validation_storage.RData

library(dplyr)
library(readr)
library(lubridate)

output_dir <- "/Volumes/MaloneLab/Research/FluxGradient/Validation_Sites/Storage_Flux"

# Find all per-site CSVs
csv_files <- list.files(output_dir, pattern = "_storage_flux\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No *_storage_flux.csv files found in ", output_dir)
}

message("Found ", length(csv_files), " site CSV(s). Reading...")

# Parse site name from filename and read each CSV into a tibble
storage_list <- lapply(csv_files, function(f) {
  site <- sub("_storage_flux\\.csv$", "", basename(f))
  message("  Reading ", site, "...")
  df <- readr::read_csv(f, show_col_types = FALSE, na = c("", "NA"))

  # Re-parse time columns to POSIXct (written as character strings in the CSV)
  time_cols <- intersect(
    c("timeMid", "timeBgn", "timeEnd", "time_halfhour", "time_halfhour_storage_end"),
    names(df)
  )
  for (col in time_cols) {
    if (is.character(df[[col]])) {
      df[[col]] <- as.POSIXct(df[[col]], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    }
  }

  as_tibble(df)
})

names(storage_list) <- sub("_storage_flux\\.csv$", "", basename(csv_files))

message("Saving Validation_storage.RData...")
Validation_storage <- storage_list
save(Validation_storage, file = file.path(output_dir, "Validation_storage.RData"))
message("Done. Saved to ", file.path(output_dir, "Validation_storage.RData"))
message("Sites included: ", paste(names(Validation_storage), collapse = ", "))
