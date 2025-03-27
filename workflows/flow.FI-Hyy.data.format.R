# Process FI-Hyy data and add Google Drive integration
# For loading data to/from google drive
email <- 'jonathan.gewirtzman@yale.edu'  # Use the same email as US-Uaf script
site <- 'FI-Hyy'

# ------ Prerequisites ------
library(dplyr)
library(lubridate)
library(purrr)
library(googledrive)  # For Google Drive integration
library(fs)           # For file path handling

# Set up temporary directory for file downloads
dirTmp <- fs::path(tempdir(), site)
dir.create(dirTmp, recursive = TRUE)

# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # May need email=TRUE for interactive auth
drive_url_extSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1jrOJIu5WfdzmlbL9vMkUNfzBpRC-W0Wd")
data_folder <- googledrive::drive_ls(path = drive_url_extSiteData)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Define files to download
focal_files <- c(
  "FI-Hyy_concentration_profile_1min.csv",
  "FI-Hyy_met_1min.csv",
  "FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv"
)

# Download files from Google Drive
for(focal_file in focal_files){
  # Find the file identifier
  file_id <- subset(site_folder, name == focal_file)
  
  # Download the file
  pathDnld <- fs::path(dirTmp, focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip if needed
  if(grepl(pattern='.zip', focal_file)){
    utils::unzip(pathDnld, exdir=dirTmp)
  }
}

# Read the downloaded data files
fihyy <- read.csv(fs::path(dirTmp, 'FI-Hyy_concentration_profile_1min.csv'))
fihyy_met <- read.csv(fs::path(dirTmp, 'FI-Hyy_met_1min.csv'))
fihhy_gas <- read.csv(fs::path(dirTmp, 'FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv'))

# ----- Your existing data processing code starts here -----
# Replace -9999 with NA in all dataframes
fihyy[fihyy == -9999] <- NA
fihyy_met[fihyy_met == -9999] <- NA
fihhy_gas[fihhy_gas == -9999] <- NA

# Parse Time columns for the first two datasets
fihyy$Time <- ymd_hms(fihyy$Time)
fihyy_met$Time <- ymd_hms(fihyy_met$Time)

# For fihhy_gas, convert from numeric timestamp format
# Assuming TIMESTAMP_START is in YYYYMMDDHHmm format as numeric
fihhy_gas <- fihhy_gas %>%
  mutate(Time = as.POSIXct(as.character(TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC"))

# Add 30-min time column to all dataframes
fihyy <- fihyy %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))
fihyy_met <- fihyy_met %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))
fihhy_gas <- fihhy_gas %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))

# Summarize high-frequency data to 30-min
fihyy_agg <- fihyy %>%
  group_by(Time_30min) %>%
  summarise(across(c(CH4_168, CH4_672, CH4_1250, CO2_168, CO2_672, CO2_1250), 
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")

fihyy_met_agg <- fihyy_met %>%
  group_by(Time_30min) %>%
  summarise(across(c(PA, TA_168, TA_672, TA_1250, VPD_168, VPD_672, VPD_1250), 
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# For fihhy_gas, select only relevant columns before aggregating
fihhy_gas_agg <- fihhy_gas %>%
  group_by(Time_30min) %>%
  summarise(
    NEE = mean(NEE, na.rm = TRUE),
    H = mean(H, na.rm = TRUE),
    LE = mean(LE, na.rm = TRUE),
    FCH4 = mean(FCH4, na.rm = TRUE),
    USTAR = mean(USTAR, na.rm = TRUE),
    TA = mean(TA_F, na.rm = TRUE),  # Using TA_F as it appears to have values
    PA = mean(PA, na.rm = TRUE),
    VPD = mean(VPD_F, na.rm = TRUE)  # Using VPD_F as it appears to have values
  )

# Now join everything by 30-min interval
all_data <- fihyy_agg %>%
  full_join(fihyy_met_agg, by = "Time_30min") %>%
  full_join(fihhy_gas_agg, by = "Time_30min")

# Add suffix to columns from gas data to avoid name conflicts
names(all_data)[names(all_data) == "PA.y"] <- "PA_flux"
names(all_data)[names(all_data) == "PA.x"] <- "PA_met"

# Create a simple attribute data frame with tower heights for this site
# This is a minimal version just to maintain the expected format
tower_heights <- data.frame(
  TowerPosition = c(1, 2, 3),
  TowerHeight = c(168, 672, 1250)
)

# Create a minimal attribute data frame
attr.df <- data.frame(
  DistZaxsLvlMeasTow = c(168, 672, 1250),
  TowerPosition = c(1, 2, 3),
  SiteID = rep("FI-Hyy", 3)
)

# ----- Your existing data processing code ends here -----

# Save results to Google Drive
# Save the processed data
fileSave <- fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.RData'))
fileZip <- fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.zip'))
save(all_data, file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip, files=paste0(site, '_aligned_conc_flux_9min.RData'))
setwd(wdPrev)

# Save in same folder as NEON site data
drive_url_NEONSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folderUpld <- googledrive::drive_ls(path = drive_url_NEONSiteData)

# Check if folder for this site exists, create if not
site_exists <- any(data_folderUpld$name == site)
if(!site_exists) {
  site_folder <- googledrive::drive_mkdir(site, path = drive_url_NEONSiteData)
  site_folder_id <- site_folder$id
} else {
  site_folder_id <- data_folderUpld$id[data_folderUpld$name == site]
}

# Upload the data file
googledrive::drive_upload(
  media = fileZip, 
  path = site_folder_id, 
  name = paste0(site, '_aligned_conc_flux_9min.zip'),
  overwrite = TRUE
) 

# Save the attribute data
pathSaveAttr <- fs::path(dirTmp, "data", site)
dir.create(pathSaveAttr, recursive = TRUE)
fileSaveAttr <- fs::path(pathSaveAttr, paste0(site, '_attr.RData'))
fileZipAttr <- fs::path(dirTmp, paste0(site, '_attr.zip'))
save(attr.df, file=fileSaveAttr)

# Zip and upload the attribute file
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZipAttr, files=fs::path("data", site, paste0(site, '_attr.RData')))
setwd(wdPrev)

googledrive::drive_upload(
  media = fileZipAttr, 
  path = site_folder_id,
  name = paste0(site, '_attr.zip'),
  overwrite = TRUE
)

# Print confirmation
cat("Processing complete. Files uploaded to Google Drive:\n")
cat(paste0("1. ", site, "_aligned_conc_flux_9min.zip\n"))
cat(paste0("2. ", site, "_attr.zip\n"))