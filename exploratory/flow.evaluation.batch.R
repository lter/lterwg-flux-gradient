# flow.evaluation:

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

# -------------- Change this stuff -------------
email <- 'csturtevant@battelleecology.org'
DirRepo <- 'C:/Users/csturtevant/Documents/Git/lterwg-flux-gradient' # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/filterTesting' # We'll deposit output files here prior to uploading to Google Drive
DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir
sffx <- c('ALLSites','MS1Sites')[2]
MS1Sites <- c('HARV','KONZ','JORN','GUAN')

dnld_files=c("SITES_WP_9min_MS1Sites.Rdata",
             "SITES_AE_9min_MS1Sites.Rdata",
             "SITES_MBR_9min_MS1Sites.Rdata")


# ---------------------------------------------

# Download necessary files from Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)

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
  gc()
}


message('Running script for ',sffx, '...')
if(sffx == 'MS1Sites'){
  SITES_WP_9min <- SITES_WP_9min[MS1Sites]
  gc()
  SITES_AE_9min <- SITES_AE_9min[MS1Sites]
  gc()
  SITES_MBR_9min <- SITES_MBR_9min[MS1Sites]
  gc()
  
}

# Application of Filter Functions: ####
message('Running Filter...')

source(fs::path(DirRepo,'exploratory/flow.evaluation.filter.R'))


fileSave <- fs::path(localdir,paste0("FilteredData_",sffx,".Rdata"))
save( SITES_WP_9min_FILTER,SITES_AE_9min_FILTER, SITES_MBR_9min_FILTER ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("FilterReport_",sffx,".Rdata"))
save( SITES_WP_9min.report,SITES_AE_9min.report, SITES_MBR_9min.report ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Application of the One2One Analysis ####
message('Running One2One with CCC computation for best height...')
source(fs::path(DirRepo,'exploratory/flow.evaluation.One2One.CCC.R'))

fileSave <- fs::path(localdir,paste0("One2One_",sffx,".Rdata"))
save( SITES_One2One,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("FilteredData_",sffx,"_BH.Rdata"))
save( SITES_WP_9min_FILTER_BH,SITES_AE_9min_FILTER_BH, SITES_MBR_9min_FILTER_BH ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Application of the Diurnal Analysis ####
load(fileSave)

source(fs::path(DirRepo,'exploratory/flow.evaluation.diurnal.R'))

fileSave <- fs::path(localdir,paste0("DiurnalSummary_",sffx,"_BH.Rdata"))
save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("Diurnal_",sffx,"_BH.Rdata"))
save( Diurnal.AE.H2O , Diurnal.MBR.H2O,  Diurnal.WP.H2O,
      Diurnal.AE.CO2 , Diurnal.MBR.CO2,  Diurnal.WP.CO2,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Carbon Exchange PARMS: ####
load(fs::path(localdir,paste0("FilteredData_",sffx,"_BH.Rdata")))

source(fs::path(DirRepo,'exploratory/flow.evaluation.cparms.R'))

fileSave <- fs::path(localdir,paste0('CarbonParms_',sffx,'.Rdata'))
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
