# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1IbdBGHMteLhFThQzCAA3AeoKh4-xHgWg")
#add userinfo for saving and uploading the file to G drive
user <- "AH"
sitecode <- 'BONA'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)

# Load functions in this repo
source(file.path("R/MO_Length.R"))
source(file.path("R/eddydiffAE.R"))
source(file.path("R/eddydiffWP.R"))
source(file.path("R/FG_AE.WP.R"))
source(file.path("R/computeFG.AE.WP.R"))

# Final note: This script takes approx 10 min to run per site. 
# -------------------------------------------------------
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
site_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir(),sitecode)
dir.create(dirTmp)
#focal_file = "KONZ_30m.zip"
for(focal_file in site_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(site_folder, name == focal_file)
  
  # Download that file
  pathDnld <- fs::path(dirTmp,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip
  if(grepl(pattern='.zip',focal_file)){
    utils::unzip(pathDnld,exdir=dirTmp)
  }
  
}
# Load 9 min interpolated data and attribute info
#problem loading in .Rdata objects currently being saved to 2 different directories?
#for KONZ files are unzipped in the same location
fileIn <- fs::path(dirTmp, paste0(sitecode,'_aligned_conc_flux_9min.Rdata'))
load(fileIn)
# fileIn <- fs::path(dirTmp,'data',sitecode,paste0(sitecode,'_attr.Rdata'))
# load(fileIn)
#if already downloaded data off of G drive or used flow.formatConcentrationDiffs.R
#load in interpolated 9 min data
#load(file.path("data", sitecode, paste0(sitecode,"_min9Diff.Rdata")))
# load(file.path("data", sitecode, "KONZ_attr.Rdata"))

#call function to calculate eddy diffusivity using AE method
#add in calculation for all gas concentrations
min9.K.AE.list <- eddydiffAE(sitecode = sitecode, min9 = min9Diff.list)
#call function to calculate eddy diffusivity using WP method
min9.K.WP.list <- eddydiffWP(sitecode = sitecode, min9 = min9Diff.list)
#call function to compute fluxes, function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
min9.FG.AE.list <- computeFG.AE.WP(min9.K = min9.K.AE.list)
min9.FG.WP.list <- computeFG.AE.WP(min9.K = min9.K.WP.list)
#save as R.data objects
save(min9.FG.AE.list, file = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
save(min9.FG.WP.list, file = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".Rdata")))
#zip R.data objects
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), files = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".zip")), files = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".Rdata")))
#upload to Google Drive
#IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
#NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
googledrive::drive_upload(media = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)
