# For loading data from Google Drive
library(googledrive)
library(readr)
library(dplyr)

# Authenticate with Google Drive
googledrive::drive_auth()

# Define the Google Drive path for FI-Hyy data
gdrive_path <- "https://drive.google.com/drive/folders/1MznM0IT_MYNkFQihpeI0o1JtHFXIFLGV"

# Create a local folder for data storage
dir.create(path = file.path("fihyy_data"), showWarnings = FALSE)

# List files in the FI-Hyy folder
fi_hyy_files <- googledrive::drive_ls(path = googledrive::as_id(gdrive_path))
print(fi_hyy_files)

# Select specific files to download
files_to_download <- c(
  "fihyy_attr.csv",                          # Attribute file
  "FI-Hyy_concentration_profile_1min.csv",   # Concentration profile data
  "FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv", # Flux data (using this instead of gasflux_30min)
  "FI-Hyy_met_1min.csv",                     # Meteorological data
  "FI-Hyy_variables.csv",                    # Variables metadata
  "FI-Hyy_metadata.xlsx"                     # Additional metadata if available
)

# Filter the file list to include only the files we want
files_to_download_ids <- fi_hyy_files %>%
  dplyr::filter(name %in% files_to_download)

# Download each file
for (i in 1:nrow(files_to_download_ids)) {
  file_id <- files_to_download_ids$id[i]
  file_name <- files_to_download_ids$name[i]
  download_path <- file.path("fihyy_data", file_name)
  
  message(paste("Downloading", file_name))
  googledrive::drive_download(
    file = file_id,
    path = download_path,
    overwrite = TRUE
  )
}

# Read the files into R to examine their structure
# Try to read the attribute file
if (file.exists(file.path("fihyy_data", "fihyy_attr.csv"))) {
  attr_df <- read.csv(file.path("fihyy_data", "fihyy_attr.csv"))
  print("Structure of attr_df:")
  str(attr_df)
  print("Column names of attr_df:")
  print(names(attr_df))
} else {
  message("Attribute file not found")
}

# Read concentration profile data
if (file.exists(file.path("fihyy_data", "FI-Hyy_concentration_profile_1min.csv"))) {
  conc_data <- read.csv(file.path("fihyy_data", "FI-Hyy_concentration_profile_1min.csv"))
  print("Structure of concentration profile data:")
  str(conc_data)
  print("Column names of concentration profile data:")
  print(names(conc_data))
} else {
  message("Concentration profile file not found")
}

# Read flux data (using FLUXNET-CH4 file)
flux_file <- "FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv"
if (file.exists(file.path("fihyy_data", flux_file))) {
  flux_data <- read.csv(file.path("fihyy_data", flux_file))
  print("Structure of flux data:")
  str(flux_data)
  print("Column names of flux data:")
  print(names(flux_data))
} else {
  message("Flux data file not found")
}

# Read meteorological data
if (file.exists(file.path("fihyy_data", "FI-Hyy_met_1min.csv"))) {
  met_data <- read.csv(file.path("fihyy_data", "FI-Hyy_met_1min.csv"))
  print("Structure of meteorological data:")
  str(met_data)
  print("Column names of meteorological data:")
  print(names(met_data))
} else {
  message("Meteorological data file not found")
}

# Read variables metadata if available
if (file.exists(file.path("fihyy_data", "FI-Hyy_variables.csv"))) {
  var_meta <- read.csv(file.path("fihyy_data", "FI-Hyy_variables.csv"))
  print("Structure of variables metadata:")
  str(var_meta)
  print("Column names of variables metadata:")
  print(names(var_meta))
} else {
  message("Variables metadata file not found")
}

# Read metadata Excel file if available
if (file.exists(file.path("fihyy_data", "FI-Hyy_metadata.xlsx"))) {
  library(readxl)
  meta_data <- readxl::read_excel(file.path("fihyy_data", "FI-Hyy_metadata.xlsx"))
  print("Structure of metadata:")
  str(meta_data)
  print("Column names of metadata:")
  print(names(meta_data))
} else {
  message("Metadata file not found")
}
