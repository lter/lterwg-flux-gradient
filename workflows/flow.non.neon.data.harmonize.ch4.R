## --------------------------------------------------- ##
            # CH4 (Methane) Harmonization
## --------------------------------------------------- ##
# Script Authors: Kyle Delwiche, Sparkle Malone, Nick Lyon

# PURPOSE:
## Harmonize (i.e., synonymize column names) of PI-provided CH4 data
## Desired format matches NEON data format
## Make one methane-key per site
## update and save site-specific data into their individual folders
## next step: run flow.<siteID>.data.format.conc.diffs.R

## ----------------------------- ##
# Housekeeping ----
## ----------------------------- ##

# Load needed libraries
# install.packages("librarian")
library(data.table)
library(readr)
librarian::shelf(tidyverse, googledrive, lter/ltertools)

# Clear environment
rm(list = ls())

#Site to work on
siteID <- 'SE-Sto'

if (siteID == 'US-Uaf'){
  gdrive_path <- "https://drive.google.com/drive/u/0/folders/1AOct-UbwpzkuLMT9EnEspRX_QnX07T4G"
} else if (siteID == 'SE-Sto'){
  gdrive_path <- "https://drive.google.com/drive/u/1/folders/1F1qZkAZywNUq_fyS1OmlG3C9AkGo6fdc"
} else if (siteID == 'SE-Deg'){
  gdrive_path <- "https://drive.google.com/drive/u/1/folders/1MzyDvXudL-A3ZGlzukbhil19fsx3s7Mk"
} else if (siteID == 'SE-Svb'){
  gdrive_path <- "https://drive.google.com/drive/u/1/folders/1qPrBaZxX7XBBKq77eEmVXALSoDmUB2_I"
}

# Create a local folder for data storage
dir.create(path = file.path("methane"), showWarnings = F)
dir.create(path = file.path("methane", "raw_methane"), showWarnings = F)

## ----------------------------- ##
        # Data Download ----
## ----------------------------- ##

#Identify which files to keep, by site.  specify directly bc data is so different it's difficult to create standard filtering rules for file names
if (siteID == 'US-Uaf'){
  ch4_files_to_keep <- c('US-Uaf CH4_concentration.csv',
                 'AMF_US-Uaf_BASE_HH_12-5.csv')
                 
} else if (siteID == 'SE-Sto'){
  ch4_files_to_keep <- c('SE-Sto_met_30min.csv',
                  'SE-Sto_gas_fluxes_30min.csv',
                  'SE-Sto_concentration_profile_30min.csv')
} else if (siteID == 'SE-Deg'){
  ch4_files_to_keep <- c('SE-Deg_gasflux_biomet_30min',
                         'SE-Deg_concentration_profile_30min')
}else if (siteID == 'SE-Svb'){
  ch4_files_to_keep <- c('CH4_SE_SVB_FLUX+PROFILE_2019')
}


## ----------------------------- ##
# Data Download ----
## ----------------------------- ##

# Identify desired files 
ch4_files <- googledrive::drive_ls(
  path = googledrive::as_id(gdrive_path),
  ) %>%  
  # Filter to keep only specified files
  dplyr::filter(name %in% ch4_files_to_keep) %>% 
  # Add explicit path column for safety
  dplyr::mutate(target_path = file.path("methane", "raw_methane", name))

# Download files with explicit path specification
purrr::walk2(
  .x = ch4_files$id,
  .y = ch4_files$target_path,
  .f = ~ googledrive::drive_download(
    file = googledrive::as_id(.x),
    path = .y,
    overwrite = TRUE
  )
)


# Check structure
ch4_files



# # Download desired files
# purrr::walk2(.x = ch4_files$id, .y = ch4_files$name,
#              .f = ~ googledrive::drive_download(file = .x, overwrite = T, path = file.path("methane", "raw_methane", .y)))

# Delete any files that don't contain the data we need at this step
all_files <- list.files(path = "methane/raw_methane", full.names = TRUE)
files_to_delete <- all_files[!basename(all_files) %in% ch4_files_to_keep]
unlink(files_to_delete)

# Some of the datafiles have initial rows of text that mess up future manipulations, so manually delete these
# Identify the file with the problematic first row
if (siteID == 'US-Uaf'){
  file_path <- "methane/raw_methane/AMF_US-Uaf_BASE_HH_12-5.csv"
  data <- fread(file_path, skip = 1)
  fwrite(data, file_path)   #re-save US-Uaf BASE file 
}

## ----------------------------- ##
          # Key Prep -- at this point the methane/raw_methane folder contains only data that needs columns renamed
## ----------------------------- ##
# NOTE: This code is only run once to make the 'column key' skeleton
## Which will be subsequently updated by hand

# Want to remake the skeleton?
remake_key <- TRUE

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
            file = file.path("methane", paste("methane_key_skeleton_",siteID,".csv")))
  
}

## ----------------------------- ##
# Harmonization ---- 
# before running this step, take site-specific methane_key, save it on Drive, and add tidy_names
# tidy_names based on xxxxxxxx
## ----------------------------- ##

# Clear environment (again)
rm(list = setdiff(ls(), "gdrive_path", "SE-Sto"))


# Identify key
(key_id <- googledrive::drive_ls(path = googledrive::as_id(gdrive_path)) %>% 
  dplyr::filter(name == paste("Methane_key_", siteID, sep='') ))

# Download key
googledrive::drive_download(file = key_id$id, overwrite = T,
                              path = file.path("methane", "methane-key.csv"))

# Read the key back in
ch4_key <- read.csv(file = file.path("methane", "methane-key.csv"))

# Check structure
dplyr::glimpse(ch4_key)

# Harmonize the raw data with that key
# Get list of files in the raw_folder
all_files <- list.files(path = "methane/raw_methane", full.names = TRUE)
#rename column names in each file individually, save
for (file in all_files) {
  # Read the file
  if (tools::file_ext(file) == "csv") {
    data <- read_csv(file)
  } else {
    data <- readxl::read_excel(file)
  }
  
  # Get the filename without extension
  filename <- tools::file_path_sans_ext(basename(file))
  
  # Filter the key for this file
  file_key <- ch4_key %>% filter(source == paste0(filename, ".", tools::file_ext(file)))
  
  # Rename columns
  for (i in 1:nrow(file_key)) {
    if (!is.na(file_key$tidy_name[i])) {
      data <- data %>% rename(!!file_key$tidy_name[i] := file_key$raw_name[i])
    }
  }
 
  # Save the modified file
  write_csv(data, file.path(raw_folder, paste0(filename, "_harmonized.csv")))
}

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
