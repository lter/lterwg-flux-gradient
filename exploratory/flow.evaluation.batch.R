# flow.evaluation:

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

# -------------- Change this stuff -------------
#DirRepo <- 'C:/Users/csturtevant/Documents/Git/lterwg-flux-gradient' # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(DirRepo)
#localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/filterTesting' # We'll deposit output files here prior to uploading to Google Drive

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# ---------------------------------------------

# Download necessary files from Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)


if(DnldFromGoogleDrive == TRUE){
  for( site in site.list){
    print(site)
    
    site <- site
    
    paste(site, "_Evaluation.Rdata", sep = "")
    dnld_files <- c()
    
    localdir.site <- paste(localdir,"/", site, sep = "")
    
  for (focal_file in dnld_files){
    
    message('Downloading ',focal_file, ' to ',localdir)
  
    site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
    file_id <- subset(site_folder, name == focal_file)
    
    pathDnld <- fs::path(localdir.site,focal_file)
    googledrive::drive_download(file = file_id$id, 
                              path = fs::path(localdir.site,focal_file),
                              overwrite = T)
    zip.file <- fs::path(localdir.site,focal_file)
    unzip(zip.file, exdir=localdir.site)
  
  }
} }

# Application of Filter Functions: ####
message('Running Filter...')

source(fs::path(DirRepo,'exploratory/flow.evaluation.filter.R'))

# Compiles Dataframes into one list:
source(fs::path(DirRepo,'exploratory/flow.evaluation_SITELIST.R'))

# Application of the One2One Analysis ####
message('Running One2One with CCC computation for best height...')
# set where you want plots to go:
dir.one2one <- '/Volumes/MaloneLab/Research/FluxGradient/One2One_Plots'
source(fs::path(DirRepo,'exploratory/flow.evaluation.One2One.CCC.R'))

fileSave <- fs::path(localdir,paste0("SITES_One2One.Rdata"))
save( SITES_One2One,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("FilteredData_BH.Rdata"))
save( SITES_WP_9min_FILTER_BH,SITES_AE_9min_FILTER_BH, SITES_MBR_9min_FILTER_BH ,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Zip the plots and upload to google"

files2zip <- dir(dir.one2one, full.names = TRUE)
zip(zipfile = 'One2One.zip', files = files2zip)

fileSave <- paste(dir.one2one, 'One2One.zip', sep="/")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Application of the Diurnal Analysis ####
load(fs::path(localdir,paste0("FilteredData_BH.Rdata")))
# set where you want plots to go:
dir.diel <- '/Volumes/MaloneLab/Research/FluxGradient/DIEL_Plots'

source(fs::path(DirRepo,'exploratory/flow.evaluation.diurnal.R'))

fileSave <- fs::path(localdir,paste0("DiurnalSummary_BH.Rdata"))

save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("Diurnal_SITES_BH.Rdata"))

save( Diurnal.AE.H2O , Diurnal.MBR.H2O,  Diurnal.WP.H2O,
      Diurnal.AE.CO2 , Diurnal.MBR.CO2,  Diurnal.WP.CO2,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Zip the plots and upload to google"

files2zip <- dir(dir.diel, full.names = TRUE)
zip(zipfile = 'DIEL.zip', files = files2zip)

fileSave <- paste(dir.diel, 'DIEL.zip', sep="/")
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
