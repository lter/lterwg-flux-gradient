## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Uses the aligned concentration file combined with the 30min and 9min data files to calculates fluxes and saves locally. 
# You must download the aligned concentration data using flow.download.aligned.conc.flux.R.

# Output(s):
# SITE_AE_9min.Rdata (local & Google Drive)
# SITE_AE_9min.zip (local)
# SITE_MBR_9min.RData (local)
# SITE_MBR_9min.zip (local & Google Drive)
# SITE_WP_9min.Rdata (local)
# SITE_WP_9min.zip (local & Google Drive)

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

# Add all sites here:
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv') # has a list of all the sites

# Get unique sites
site.list <- metadata$Site %>% unique()

# Add local directory for downloaded data here:
localdir1 <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData' # MaloneLab Server

# Add local directory for your Flux repo here:
localdir2 <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(localdir2)

## --------------------------------------------- ##
#               Authenticate -----
## --------------------------------------------- ##

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 

# Authenticate with Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

# Data on google drive
data_folder <- googledrive::drive_ls(path = drive_url)

## --------------------------------------------- ##
#         Gradient Flux Calculations -----
## --------------------------------------------- ##

for(site in site.list){
  
  setwd(localdir2)
  
  sitecode <- site
  print(sitecode)
  
  # Load Data:
  load(fs::path(localdir1, site, paste0(site, '_aligned_conc_flux_30min.RData')))
  load(fs::path(localdir1, site, paste0(site, '_aligned_conc_flux_9min.RData')))

  dirTmp <- file.path(localdir1, site)
    
  print('Data Loaded')
  
  print('Running MBR')
  source(file.path("workflows", "flow.calc.flag.mbr.batch.R"))
  print('MBR Done')
  
  setwd(localdir2)
  print('Running AE')
  source(file.path("workflows", "flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  setwd(localdir2)
  print('Running WP')
  source(file.path("workflows", "flow.calc.flag.windprof.batch.R"))
  print('WP Done')
  
  print('done')
  rm(min9)
}

message('Next run the flow.evaluation.dataframe.R')
