# flow.evaluation: Makes the dataframes needed for gradient flux evaluation

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 


setwd(localdir)

load( "SITES_WP_9min.Rdata")
load( "SITES_AE_9min.Rdata")
load( "SITES_MBR_9min.Rdata")

# Subset list to only include sites of interest:
 sites <- c("KONZ", "HARV", "JORN", "GUAN")
 
 SITES_MBR_9min <- SITES_MBR_9min[ sites]
 SITES_AE_9min <- SITES_AE_9min[ sites]
 SITES_WP_9min <- SITES_WP_9min[ sites]

 # Application of Filter Functions: ####
 source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.filter.R')
 
 save( SITES_WP_9min_FILTER,SITES_AE_9min_FILTER, SITES_MBR_9min_FILTER ,
       file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 
 save( SITES_WP_9min.report,SITES_AE_9min.report, SITES_MBR_9min.report ,
       file="/Volumes/MaloneLab/Research/FluxGradient/FilterReport_MS1Sites.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilterReport_MS1Sites.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 

 # Application of the One2One Analysis ####
 
 source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.One2One.R')
 
 save( SITES_One2One,
       file="/Volumes/MaloneLab/Research/FluxGradient/One2One_MS1Sites.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/One2One_MS1Sites.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 
 save( SITES_WP_9min_FILTER_BH,SITES_AE_9min_FILTER_BH, SITES_MBR_9min_FILTER_BH ,
       file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites_BH.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites_BH.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 
# Fit Diurnal for that month:

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.diurnal.R')

save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file="/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_MS1Sites_BH.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_MS1Sites_BH.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Carbon Exchange PARMS: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.cparms.R')

save( SITES_MBR_9min_CPARMS_FG ,
      SITES_MBR_9min_CPARMS_EC ,
      SITES_AE_9min_CPARMS_FG,
      SITES_AE_9min_CPARMS_EC,
      SITES_WP_9min_CPARMS_EC, 
      SITES_WP_9min_CPARMS_FG ,
      MBR.CPARMS,
      AE.CPARMS ,
      WP.CPARMS,
      file= '/Volumes/MaloneLab/Research/FluxGradient/CarbonParms_MS1Sites.Rdata')

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/CarbonParms_MS1Sites.Rdata')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

message("Next run flow.evaluation.figures_MS1.R ")
