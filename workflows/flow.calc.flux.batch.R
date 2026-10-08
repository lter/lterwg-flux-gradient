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

# See WorkFlow_master.R to set the variables needed to run this script
# Variables needed: gh_repo, aligned_conc_dir, save_calc_temp, export_calc_google,
# data_folder (if export_calc_google == 1), site.list

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

## --------------------------------------------- ##
#         Gradient Flux Calculations -----
## --------------------------------------------- ##

for(site in site.list){
  
  # Load Data:
  load(fs::path(aligned_conc_dir, site, paste0(site, '_aligned_conc_flux_30min.RData')))
  load(fs::path(aligned_conc_dir, site, paste0(site, '_aligned_conc_flux_9min.RData')))
   
  if (save_calc_temp == 0){
    dirTmp <- file.path(aligned_conc_dir, site)
  }
  else if (save_calc_temp == 1){
    dirTmp <- fs::path(tempdir(), site)
    dir.create(dirTmp)
  }
  print('Data Loaded')
  
  print('Running MBR')
  source(file.path(gh_repo, "workflows", "flow.calc.flag.mbr.batch.R"))
  print('MBR Done')
  
  print('Running AE')
  source(file.path(gh_repo, "workflows", "flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  print('Running WP')
  source(file.path(gh_repo, "workflows", "flow.calc.flag.windprof.batch.R"))
  print('WP Done')
  
  print('done')
  rm(min9)
}

message('Next run the flow.evaluation.dataframe.R')
