## --------------------------------------------- ##
#         Compile NEON Storage RData -----
## --------------------------------------------- ##
# Purpose:
# Reads individual per-site storage flux CSVs already on disk and assembles
# them into NEON_storage.RData, matching the structure produced by
# flow.neon.storage.R (a named list of tibbles, one element per site).
#
# Run this instead of the full flow.neon.storage.R when the per-site CSVs
# have already been generated.
#
# Input:
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/SITE_storage_flux.csv
#
# Output:
#   /Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux/NEON_storage.RData

library(dplyr)
library(readr)
library(lubridate)

output_dir <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_Storage_Flux"

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

message("Saving NEON_storage.RData...")
NEON_storage <- storage_list
save(NEON_storage, file = file.path(output_dir, "NEON_storage.RData"))
message("Done. Saved to ", file.path(output_dir, "NEON_storage.RData"))
message("Sites included: ", paste(names(NEON_storage), collapse = ", "))
