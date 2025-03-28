
# Uses the aligned concentration file combined with the 30min and 9min data files to calculates fluxes and saves locally. You must download the aligned concentration data using flow.download.aligned_concflux.R.

library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

data_folder <- googledrive::drive_ls(path = drive_url)

# Add local directory for downloaded data here:
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData' # MaloneLab Server

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")

# Add all sites here:
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# something is wrong with TEAK, TOOL, WREF- cant run ae and wp
## Error in calculate.stability.correction(gas = H2O) : object 'TopLevel' not found
# Gradient Flux Calculations: ####

for( site in site.list ){
  
  setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")
  
  sitecode <- site
  print(sitecode)
  
  # Load Data:
  load(fs::path(paste(localdir, site,sep="/"), paste0(site,'_aligned_conc_flux_30min.RData')))
  load(fs::path(paste(localdir, site,sep="/"), paste0(site,'_aligned_conc_flux_9min.RData')))

  dirTmp <- paste(localdir, site,sep="/")
    
  print('Data Loaded')
  
  print( 'Running MBR')
  source(file.path("exploratory/flow.calc.flag.mbr.batch.R"))
  print('MBR Done')
  
  setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")
  print( 'Running AE')
  source(file.path("exploratory/flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")
  print( 'Running WP')
  source(file.path("exploratory/flow.calc.flag.windprof.batch.R"))
  print('WP Done')
  
  print('done')

}

message('Next run the flow.validation.dataframe.batch.R')
