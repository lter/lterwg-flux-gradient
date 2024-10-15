# Pull data from google drive
#email <- 'alexisrose0525@gmail.com'
email <- 'areysan@ncsu.edu'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
#library(dplyr)

DoWP=1 # Do Wind Profile Method here as well? 1 for true O for False.
Savecsv=1 # Save csv files to analyze in matlab? 1 for true 0 for False.

# Load functions in this repo
source(file.path("functions/MO_Length_CRS.R"))
source(file.path("functions/calc.eddydiff.aero.R"))
source(file.path("functions/calc.gas.aero.windprof.flux.R"))
source(file.path("functions/calc.gas.aero.windprof.flux_WP.R"))
source(file.path("functions/calc.eqn.aero.windprof.flux.R"))
source(file.path("functions/calculate.stability.correction.R"))
source(file.path("functions/calc.aerodynamic.canopy.height.R"))

# Pull averaged data for concentration difference across height
# and associated micromet variables from Google Drive

site <- "NIWO"

googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

# Uncomment the next line and comment the following line if you want all the files
#fileDnld <- site_folder$name 
fileDnld <- c(paste0(site,'_aligned_conc_flux_9min.zip'), 
              paste0(site,'_aligned_conc_flux_30min.zip'))

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

# Load the data after download from Google Drive
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
load(fileIn)

fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_30min.RData'))
load(fileIn)

# Calculate eddy diffusivity with the aerodynamic method
min9.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 = min9Diff.list)

min30.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 = min30Diff.list)

# Compute aerodynamic flux gradient fluxes for all gases
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
min9.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.AE.list,
                                               bootstrap = 0, nsamp = 1000)
min30.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.AE.list,
                                               bootstrap = 0, nsamp = 1000)

if (DoWP==1){
# Apply Wind Profile Method
min9.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min9.K.AE.list,
                                               bootstrap = 0, nsamp = 1000)
min30.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min30.K.AE.list,
                                                bootstrap = 0, nsamp = 1000)
}


# Save calculated aerodynamic flux gradient fluxes as R.data objects
# save(min9.FG.AE.list, file = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
# #zip R.data objects
# zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), files = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
# #upload to Google Drive
# #IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
# #NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
# googledrive::drive_upload(media = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)
user = "crs"

# Save 9-minute 
fileSave <- fs::path(dirTmp,paste0(site,"_AE_9min_", user,"_",Sys.Date(),".Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_AE_9min_", user,"_",Sys.Date(),".zip"))
save(min9.FG.AE.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_AE_9min_", user,"_",Sys.Date(),".Rdata"))
setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

# Save 30-minute
fileSave <- fs::path(dirTmp,paste0(site,"_AE_30min_", user,"_",Sys.Date(),".Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_AE_30min_", user,"_",Sys.Date(),".zip"))
save(min30.FG.AE.list,file=fileSave)
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_AE_30min_", user,"_",Sys.Date(),".Rdata"))
setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

# Optional. Save csv to analyze in Matlab

if (Savecsv==1){
## AE Method
# Min 9

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_CO2_data_", site, ".csv")
write.csv(min9.FG.AE.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_CH4_data_", site, ".csv")
write.csv(min9.FG.AE.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_H2O_data_", site, ".csv")
write.csv(min9.FG.AE.list$H2O, MyFile, row.names = FALSE)

# Min 30

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_CO2_data_", site, ".csv")
write.csv(min30.FG.AE.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_CH4_data_", site, ".csv")
write.csv(min30.FG.AE.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_H2O_data_", site, ".csv")
write.csv(min30.FG.AE.list$H2O, MyFile, row.names = FALSE)

## WP Method
# Min 9

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_CO2_data_", site, ".csv")
write.csv(min9.FG.WP.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_CH4_data_", site, ".csv")
write.csv(min9.FG.WP.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_H2O_data_", site, ".csv")
write.csv(min9.FG.WP.list$H2O, MyFile, row.names = FALSE)

# Min 30

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_CO2_data_", site, ".csv")
write.csv(min30.FG.WP.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_CH4_data_", site, ".csv")
write.csv(min30.FG.WP.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_H2O_data_", site, ".csv")
write.csv(min30.FG.WP.list$H2O, MyFile, row.names = FALSE)
}