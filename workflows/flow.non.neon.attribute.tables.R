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

# Identify Drive folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk")

# Identify attributes table CSV
attr_csv <- googledrive::drive_ls(path = drive_url) %>% 
  dplyr::filter(stringr::str_detect(string = name, pattern = "_attr.csv"))

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



csv_to_rdata <- function(csv = NULL, rdata = NULL){
  
  if(is.null(csv) == TRUE | is.null(rdata) == TRUE)
    stop("Both arguments must be provided")
  
  if(tolower(tools::file_ext(csv)) != "csv")
    stop("'csv' must be a comma separated value (.csv) file")
  
  
  data <- read.csv(file = csv)
  
  
  
}


## ---------------------------- ##
    # SE-Deg Attributes ----
## ---------------------------- ##

# Identify Drive data folder for site
site_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk")

# Identify data folders in that Drive folder
site_files <- googledrive::drive_ls(path = site_drive) %>%
  # Can add more file names inside of `c(...)`
  dplyr::filter(name %in% c("SE-Deg_concentration_profile_30min.xlsx"))

# Check to make sure that worked
site_files

# Identify the name of the site
site_name <- "SE-Deg"
  
# Make a sub-folder for that site's data
dir.create(path = file.path("data", site_name), showWarnings = F)

# Download data into that path
## May take a bit if the data file is large
purrr::walk2(.x = site_files$name, .y = site_files$id,
             .f = ~ googledrive::drive_download(file = .y, overwrite = T,
                                                path = file.path("data", site_name, .x)))

# Read in data
data <- readxl::read_excel(path = file.path("data", site_name, 
                                            "SE-Deg_concentration_profile_30min.xlsx"))

# Glimpse data
dplyr::glimpse(data)

# Wrangle data to proper format for attribute table
attr.df <- data %>% 
  # Rename desired columns
  dplyr::rename(
    DistZaxsCnpy = RAW_DATA_NAME,
    DistZaxsDisp = RAW_DATA_NAME2,
    # ...
  ) %>% 
  # Make any columns in attribute table that lack parallels in this data
  dplyr::mutate(
    TimeTube = NA,
    # ...
  ) %>% 
  # Keep only those columns
  dplyr::select(dplyr::all_of(x = attr_colnames)) %>% 
  # Keep only unique rows (i.e., refine to only attribute information)
  dplyr::distinct()

# Check structure
dplyr::glimpse(attr.df)

# Save as a .RData object
save(attr.df, file = file.path("data", site_name, paste0(sitename, "_attr")))

# Zip it
zip(zipfile = file.path("data", site_name, paste0(sitename, "_attr.zip")),
    files = file.path("data", site_name, paste0(sitename, "_attr.Rdata")))

# Upload to Google Drive
googledrive::drive_upload(media = file.path("data", site_name, 
                                            aste0(sitename, "_attr.zip")),
                          path = site_name, overwrite = T)

# Clear environment of everything *except* vector of attribute names
rm(list = setdiff(x = ls(), y = c("attr_colnames")))

## ---------------------------- ##
# Next Site... ----
## ---------------------------- ##

# Theoretically can copy/paste preceding chunk and just change the following bits:
## 1. 'site_drive' link
## 2. file name(s) downloaded / read in for this code section
## 3. 'site_name' object


# End ----
