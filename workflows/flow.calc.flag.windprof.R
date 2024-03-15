# Pull data from google drive
email <- 'jaclyn_matthes@g.harvard.edu'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)

# Load functions for the wind profile flux gradient calculation
source(file.path("functions/MO_Length.R"))
source(file.path("functions/calc.eddydiff.windprof.R"))
source(file.path("functions/FG_AE.WP.R"))
source(file.path("functions/computeFG.AE.WP.R"))
source(file.path("functions/calculate.stability.correction.R"))

# Authenticate with Google Drive
sitecode <- "HARV"
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

# Uncomment the next line and comment the following line if you want all the files
#fileDnld <- site_folder$name 
fileDnld <- paste0(site,'_aligned_conc_flux_9min.zip')

message(paste0('Downloading aligned concentration & flux data for ',site))
for(focal_file in fileDnld){
  
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

# Load the data 
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
load(fileIn)

#call function to calculate eddy diffusivity using WP method
min9.K.WP.list <- calc.eddydiff.windprof(sitecode = sitecode, min9 = min9Diff.list)

# call function to compute fluxes
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
# 
min9.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.WP.list, 
                                               bootstrap = 1, nsamp=1000)

#save as R.data objects
save(min9.FG.WP.list, file = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".Rdata")))
#zip R.data objects
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".zip")), files = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".Rdata")))
#upload to Google Drive
#IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
#NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
googledrive::drive_upload(media = file.path("data", sitecode, paste0(sitecode,"_WP_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)
