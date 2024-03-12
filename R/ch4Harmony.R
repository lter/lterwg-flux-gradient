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

## ----------------------------- ##
# Key Prep ----
## ----------------------------- ##

# Identify desired files
ch4_files <- dplyr::bind_rows(
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk")),
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1qPrBaZxX7XBBKq77eEmVXALSoDmUB2_I")),
  googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1AOct-UbwpzkuLMT9EnEspRX_QnX07T4G")) ) %>% 
  # Keep only CSVs / Excel files
  dplyr::filter(stringr::str_detect(string = name, pattern = ".csv") == TRUE |
                  stringr::str_detect(string = name, pattern = ".xls") == TRUE) %>% 
  # Drop 'metadata'
  dplyr::filter(stringr::str_detect(string = name, pattern = "metadata") != TRUE)

# Check structure
dplyr::glimpse(ch4_files)
## tibble::view(ch4_files)

# Download desired files


## ----------------------------- ##
# Harmonization ----
## ----------------------------- ##





# End ----
