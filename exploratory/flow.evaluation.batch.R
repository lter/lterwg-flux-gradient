# flow.evaluation:

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

#------ CHANGE THIS STUFF ------

email <- 'csturtevant@battelleecology.org'
DirRepo <- "." # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/9min' # We'll deposit output files here prior to uploading to Google Drive
DnldFromGoogleDrive <- TRUE # Enter TRUE if you don't have the files listed in dnld_files below locally in the localdir directory

# ------------------------------

# Download necessary files from Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)

dnld_files=c("SITES_WP_9min.Rdata","SITES_AE_9min.Rdata","SITES_MBR_9min.Rdata")
if(DnldFromGoogleDrive == TRUE){
  for (focal_file in dnld_files){
  message('Downloading ',focal_file, ' to ',localdir)
  file_id <- subset(data_folder, name == focal_file)
  pathDnld <- fs::path(localdir,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = fs::path(localdir,focal_file),
                              overwrite = T)
  
  }
}

# Load the files
for (focal_file in dnld_files){
  message('Loading ',focal_file, ' from ',localdir)
  load(fs::path(localdir,focal_file))
}

message('Running script...')

# Application of Filter Functions: ####
source(fs::path(DirRepo,'exploratory/flow.evaluation.filter.R'))


fileSave <- fs::path(localdir,"FilteredData_ALLSites.Rdata")
save( SITES_WP_9min_FILTER,SITES_AE_9min_FILTER, SITES_MBR_9min_FILTER ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Application of the One2One Analysis ####
load(fileSave)
source(fs::path(DirRepo,'exploratory/flow.evaluation.One2One.R'))

fileSave <- fs::path(localdir,"One2One_ALLSites.Rdata")
save( SITES_One2One,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,"FilteredData_ALLSites_BH.Rdata")
save( SITES_WP_9min_FILTER_BH,SITES_AE_9min_FILTER_BH, SITES_MBR_9min_FILTER_BH ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Application of the Diurnal Analysis ####
load(fileSave)

source(fs::path(DirRepo,'exploratory/flow.evaluation.diurnal.R'))

fileSave <- fs::path(localdir,"DiurnalSummary_ALLSites_BH.Rdata")
save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,"Diurnal_ALLSites_BH.Rdata")
save( Diurnal.AE.H2O , Diurnal.MBR.H2O,  Diurnal.WP.H2O,
      Diurnal.AE.CO2 , Diurnal.MBR.CO2,  Diurnal.WP.CO2,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Carbon Exchange PARMS: ####

load(fs::path(localdir,"FilteredData_ALLSites_BH.Rdata"))

source(fs::path(DirRepo,'exploratory/flow.evaluation.cparms.R'))

fileSave <- fs::path(localdir,'CarbonParms.Rdata')
save( SITES_MBR_9min_CPARMS_FG ,
      SITES_MBR_9min_CPARMS_EC ,
      SITES_AE_9min_CPARMS_FG,
      SITES_AE_9min_CPARMS_EC,
      SITES_WP_9min_CPARMS_EC, 
      SITES_WP_9min_CPARMS_FG ,
      MBR.CPARMS,
      AE.CPARMS ,
      WP.CPARMS,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
