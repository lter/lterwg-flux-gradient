# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
#add userinfo for saving and uploading the file to G drive
user <- "AH"
#the R.data/zipfiles are labeled based on the method used to calculate the fluxes (i.e. AE, WP, MBR)
method <- 'WP'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(changepoint)
#library(REddyProc)

# Load functions in this repo
source(file.path("functions/calc.iqr.R"))
source(file.path("functions/flag.all.gas.stability.R"))
source(file.path("functions/calc.rmse.R"))
source(file.path("functions/flag.flux.spikes.R"))
source(file.path("functions/calc.flag.ustar.thres.R"))
source(file.path("functions/compile.quality.flags.add.cols.fluxes.R"))
source(file.path("functions/add.hour.col.R"))
source(file.path("functions/flag.calc.flux.diff.R"))
source(file.path("functions/flag.iqr.R"))

#Pull data from gdrive
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
validation_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir(),method)
dir.create(dirTmp)
for(focal_file in validation_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(validation_folder, name == focal_file)
  
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
#Load validation data frames 
#problem loading in .Rdata objects currently being saved to 2 different directories?
#TO DO: better understand how to control where files are unzipped to when downloaded off of gdrive
fileIn <- fs::path(dirTmp, paste0("data/Validation/SITES_", method, ".Rdata"))
load(fileIn)
#if data is already downloaded and saved
load(file.path("data", "Validation", paste0("SITES_", method, ".Rdata")))

#run quality flag functions and calculate residuals, note we cannot calculate residuals for CH4 yet
#save list of df as .Rdata object, zip, and upload to google drive
#zip
#upload to g drive
if(method=="WP"){
  SITES_WP_validation <- compile.quality.flags.add.cols.fluxes(list.sites = SITES_WP, method = method)
  save(SITES_WP_validation, file = file.path("data", "Validation", "SITES_WP_val.Rdata"))
  #zip(zipfile = paste0("data/Validation/SITES_WP_val.zip"), files = paste0("data/Validation/SITES_WP_val.Rdata"))
  googledrive::drive_upload(media = paste0("data/Validation/SITES_WP_val.Rdata"), overwrite = T, path = drive_url)
}
if(method == "AE"){
  SITES_AE_validation <- compile.quality.flags.add.cols.fluxes(list.sites = SITES_AE, method = method)
  save(SITES_AE_validation, file = file.path("data", "Validation", "SITES_AE_val.Rdata"))
  zip(zipfile = paste0("data/Validation/SITES_AE_val.zip"), files = paste0("data/Validation/SITES_AE_val.Rdata"))
  googledrive::drive_upload(media = paste0("data/Validation/SITES_AE_val.Rdata"), overwrite = T, path = drive_url)
}
if(method == "MBR"){
  SITES_MBR_validation <- compile.quality.flags.add.cols.fluxes(list.sites = SITES_MBR, method = method)
  save(SITES_MBR, file = file.path("data", "Validation", "SITES_MBR_val.Rdata"))
  zip(zipfile = paste0("data/Validation/SITES_MBR_val.zip"), files = paste0("data/Validation/SITES_MBR_val.Rdata"))
  googledrive::drive_upload(media = paste0("data/Validation/SITES_MBR_val.zip"), overwrite = T, path = drive_url)
}
#TO DO ADD PROPER TZ CORRECTION FOR GUAN TO add.hour.column.R

