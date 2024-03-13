## --------------------------------------------------- ##
            # CH4 (Methane) Harmonization
## --------------------------------------------------- ##
# Script Authors: Sparkle Malone, Nick Lyon

# PURPOSE:
## Harmonize (i.e., synonmize column names) of PI-provided CH4 data
## Desired format matches NEON data format

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, ltertools)

# Clear environment
rm(list = ls())

# Create a local folder for data storage
dir.create(path = file.path("methane"), showWarnings = F)
dir.create(path = file.path("methane", "raw_methane"), showWarnings = F)

## ----------------------------- ##
        # Data Download ----
## ----------------------------- ##

# Identify desired files
ch4_files <- dplyr::bind_rows(
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk")),
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1qPrBaZxX7XBBKq77eEmVXALSoDmUB2_I")),
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1AOct-UbwpzkuLMT9EnEspRX_QnX07T4G")) ) %>% 
  # Keep only CSVs / Excel files
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv") == T |
                  stringr::str_detect(string = name, pattern = ".xls") == T) %>% 
  # Drop non-data files that are the right file types
  dplyr::filter(stringr::str_detect(string = name, pattern = "metadata") != T &
                  stringr::str_detect(string = name, pattern = "data request") != T )

# Check structure
ch4_files

# Download desired files
purrr::walk2(.x = ch4_files$id, .y = ch4_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, path = file.path("methane", "raw_methane", .y)))

## ----------------------------- ##
          # Key Prep ----
## ----------------------------- ##
# NOTE: This code is only run once to make the 'column key' skeleton
## Which will be subsequently updated by hand

# Want to remake the skeleton?
remake_key <- FALSE

# Only make a new key if that is set to TRUE
if(remake_key == TRUE){
  
  # Generate key object
  ch4_key_v0 <- ltertools::begin_key(raw_folder = file.path("methane", "raw_methane"),
                                     data_format = c("csv", "xlsx", "xls"),
                                     guess_tidy = FALSE)
  
  # Check structure of _that_
  dplyr::glimpse(ch4_key_v0)
  
  # Export
  write.csv(x = ch4_key_v0, na = "", row.names = F,
            file = file.path("methane", "methane_key_skeleton.csv"))
  
}

## ----------------------------- ##
# Harmonization ----
## ----------------------------- ##

# Clear environment (again)
rm(list = ls())

# Identify key
(key_id <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1jrOJIu5WfdzmlbL9vMkUNfzBpRC-W0Wd")) %>% 
  dplyr::filter(name == "Methane-Key") )

# Download key
googledrive::drive_download(file = key_id$id, overwrite = T,
                              path = file.path("methane", "methane-key.csv"))

# Read the key back in
ch4_key <- read.csv(file = file.path("methane", "methane-key.csv"))

# Check structure
dplyr::glimpse(ch4_key)

# Harmonize the raw data with that key
ch4_tidy <- ltertools::harmonize(key = ch4_key, 
                                 raw_folder = file.path("methane", "raw_methane"),
                                 data_format = c("csv", "xlsx", "xls"),
                                 quiet = FALSE)

# Check structure
dplyr::glimpse(ch4_tidy)

# Export locally
write.csv(x = ch4_tidy, na = "", row.names = F,
          file = file.path("methane", "methane_non-neon_harmonized.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("methane", "methane_non-neon_harmonized.csv"), 
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1bxPr64QS-lH-V9zTFkEPhXIUK51LEoBs"))

# End ----
