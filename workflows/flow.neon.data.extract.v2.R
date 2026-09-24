## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Extracts from the downloaded, unzipped NEON data files

# Output(s):
# SITE_1min.Rdata (local)
# SITE_1min.zip (local & Google Drive)
# SITE_9min.Rdata (local)
# SITE_9min.zip (local & Google Drive)
# SITE_30min.Rdata (local)
# SITE_30min.zip (local & Google Drive)
# SITE_attr.Rdata (local)
# SITE_attr.zip (local & Google Drive)
# SITE_WS2D2min.Rdata (local)
# SITE_WS2D2min.zip (local & Google Drive)

# NOTE IMPORTANT INFORMATION: 
# all of the flow scripts are written assuming the end user has connected their R studio project to the lterwg-flux-gradient GitHub repo 
# AND that they have created a data folder 
# AND that within that data folder there are site folders named with the NEON sitecode

# See WorkFlow_master.R to set the variables needed to run this script
# Variables needed: gh_repo, download_extract_dir, export_extracted_google, 
# data_folder (if export_extracted_google == 1), site.list

# Load libraries
library(neonUtilities)
library(readr)
library(BiocManager)
library(rhdf5)
library(dplyr)
library(oce)
library(tidyr)
library(lubridate)
library(naniar)
library(ggplot2)
library(R.utils)
library(gtools)
library(googledrive)

# BiocManager::install("rhdf5")

# Load in data compiling functions
source(file.path(gh_repo, "functions", "compile.neon.data.v2.R"))
source(file.path(gh_repo, "functions", "compile.neon.site.attr.R"))
source(file.path(gh_repo, "functions", "grab.neon.gas.9min.6min.R"))
source(file.path(gh_repo, "functions", "grab.neon.met.flux.30min.R"))
source(file.path(gh_repo, "functions", "grab.neon.met.1min.R"))

## --------------------------------------------- ##
#               Data Extraction -----
## --------------------------------------------- ##

for (sitecode in site.list){
  
  print(sitecode)
  
  # Grab h5 files to be passed to SiteAttributes and SiteDF
  h5files <- list.files(path = file.path(download_extract_dir, "data", sitecode, "filesToStack00200"), 
                        pattern = ".h5$", 
                        full.names = T)
  
  # Grab attribute data
  attr.df <- compile.neon.site.attr(hd.files = h5files, sitecode = sitecode)
  
  # Grab gas concentration and met data at desired frequency (1min, 9min, 30min)
  
  min9.list <- compile.neon.data.v2(
    h5files = h5files,
    sitecode = sitecode,
    frequency = "9min",
    skip_errors = TRUE)
  
  min1.list <- compile.neon.data.v2(
    h5files = h5files,
    sitecode = sitecode,
    frequency = "1min" ,
    skip_errors = TRUE)
  
  min30.list <- compile.neon.data.v2(
    h5files = h5files,
    sitecode = sitecode,
    frequency = "30min",
    skip_errors = TRUE )
  
  # Load in previously downloaded met (RH, WS2D)
  load(file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_NonEddyMetVars.Rdata")))
  RH1min <- DATA$RH1min
  WS2D2min <- DATA$WS2D2min
  RH30min <- DATA$RH30min
  WS2D30min <- DATA$WS2D30min
  PAR1min <- DATA$PAR1min
  PAR30min <- DATA$PAR30min
  
  # Match previously downloaded met to correct list
  min1.list$RH <- RH1min
  min1.list$PAR <- PAR1min
  min30.list$RH <- RH30min
  min30.list$WS2D <- WS2D30min
  min30.list$PAR <- PAR30min
  
  # Save as Rdata objects
  save(min1.list, file = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_1min.Rdata")))
  save(min9.list, file = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_9min.Rdata")))
  save(min30.list, file = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_30min.Rdata")))
  save(attr.df, file = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_attr.Rdata")))
  save(WS2D2min, file = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_WS2D2min.Rdata")))
  
  # Zip Rdata objects
  zip(zipfile = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_1min.zip")), 
      files = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_1min.Rdata")))
  
  zip(zipfile = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_9min.zip")), 
      files = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_9min.Rdata")))
  
  zip(zipfile = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_30min.zip")), 
      files = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_30min.Rdata")))
  
  zip(zipfile = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_attr.zip")), 
      files = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_attr.Rdata")))
  
  zip(zipfile = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_WS2D2min.zip")), 
      files = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_WS2D2min.Rdata")))
  
  if (export_extracted_google == 1){
  
  # Upload to Google Drive
  # IMPORTANT REMINDER:
  # if you have not gone through the process of validating your email with googledrive in R this code will not work 
  # please refer to https://lter.github.io/scicomp/tutorial_googledrive-pkg.html for help
  # NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
  site_folder <- data_folder$id[data_folder$name==sitecode]
  
  googledrive::drive_upload(media = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_1min.zip")), 
                            overwrite = T, 
                            path = site_folder)
  
  googledrive::drive_upload(media = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_9min.zip")), 
                            overwrite = T, 
                            path = site_folder)
  
  googledrive::drive_upload(media = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_30min.zip")), 
                            overwrite = T, 
                            path = site_folder)
  
  googledrive::drive_upload(media = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_attr.zip")), 
                            overwrite = T, 
                            path = site_folder)
  
  googledrive::drive_upload(media = file.path(download_extract_dir, "data", sitecode, paste0(sitecode, "_WS2D2min.zip")), 
                            overwrite = T, 
                            path = site_folder)
  }
}
