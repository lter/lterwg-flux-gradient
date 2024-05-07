## ------------------------------------------------- ##
          # Make Non-NEON Attribute Tables
## ------------------------------------------------- ##
# Script author(s): Nick Lyon, Kyle Delwiche

# Purpose:
## Make attribute tables for non-NEON sites that are consistent with those of NEON sites

## ---------------------------- ##
      # Housekeeping ----
## ---------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl)

# Authorize Google Drive
googledrive::drive_auth()
## You'll need to select a pre-authorized email via the Console

# Make sure a 'data' folder exists locally
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment
rm(list = ls())

## ---------------------------- ##
# Wrangling ----
## ---------------------------- ##

## Done elsewhere (talk to Kyle)

## ---------------------------- ##
        # CSV to RData ----
## ---------------------------- ##

# Identify all Drive URLs with attributes files
attr_urls <- c(
  "https://drive.google.com/drive/u/0/folders/1MznM0IT_MYNkFQihpeI0o1JtHFXIFLGV", # Fl-Hyy
  "https://drive.google.com/drive/u/0/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk", # SE-Deg
  "https://drive.google.com/drive/u/0/folders/1F1qZkAZywNUq_fyS1OmlG3C9AkGo6fdc", # SE-Sto
  "https://drive.google.com/drive/u/0/folders/1qPrBaZxX7XBBKq77eEmVXALSoDmUB2_I", # SE-Svb
  "https://drive.google.com/drive/u/0/folders/1AOct-UbwpzkuLMT9EnEspRX_QnX07T4G" # US-Uaf
)

# Loop across these folders
for(focal_url in attr_urls){
  
  # Identify Drive folder
  drive_url <- googledrive::as_id(focal_url)
  
  # Identify attributes table CSV
  attr_csv <- googledrive::drive_ls(path = drive_url) %>% 
    dplyr::filter(stringr::str_detect(string = name, pattern = "_attr.csv"))
  
  # Skip if there isn't an attributes file (or if there's more than 1)
  if(nrow(attr_csv) != 1){
    message("More/fewer than 1 attribute file found. This script may not be appropriate")
    
    # Otherwise, do the workflow
  } else {
    
    # Processing message
    message("Processing ", attr_csv$name, " into equivalent RData and ZIP files")
    
    # Download from Google Drive into 'data/' folder
    purrr::walk2(.x = attr_csv$id, .y = attr_csv$name,
                 .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                    path = file.path("data", .y)))
    
    # Read in that CSV
    attr.df <- read.csv(file = file.path("data", attr_csv$name))
    
    # Pare down the CSV file name a bit
    file_slug <- gsub(pattern = "_attr.csv", replacement = "", x = attr_csv$name)
    
    # Save the table as an RData object
    save(attr.df, file = file.path("data", paste0(file_slug, "_attr.RData")))
    
    # Zip it too
    zip(zipfile = file.path("data", paste0(file_slug, "_attr.zip")),
        files = file.path("data", paste0(file_slug, "_attr.RData")))
    
    # Upload both back to the same Drive folder the CSV came from
    ## RData
    googledrive::drive_upload(media = file.path("data", paste0(file_slug, "_attr.RData")),
                              path = drive_url, overwrite = T)
    ## Zip
    googledrive::drive_upload(media = file.path("data", paste0(file_slug, "_attr.zip")),
                              path = drive_url, overwrite = T)
    
  } # Close `else` conditional
} # Close loop

# End ----
