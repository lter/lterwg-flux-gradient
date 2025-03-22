# flow.evaluation:

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

setwd(localdir)

load( "SITES_WP_30min.Rdata")
load( "SITES_AE_30min.Rdata")
load( "SITES_MBR_30min.Rdata")


# Application of Filter Functions: ####
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.filter.R')


save( SITES_WP_30min_FILTER,SITES_AE_30min_FILTER, SITES_MBR_30min_FILTER ,
      file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Application of the One2One Analysis ####
load("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites.Rdata")

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.One2One.R')

save( SITES_One2One,
      file="/Volumes/MaloneLab/Research/FluxGradient/One2One_ALLSites.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/One2One_ALLSites.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

save( SITES_WP_30min_FILTER_BH,SITES_AE_30min_FILTER_BH, SITES_MBR_30min_FILTER_BH ,
      file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites_BH.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites_BH.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Application of the Diurnal Analysis ####
load("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites_BH.Rdata")

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.diurnal.R')

save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file="/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_ALLSites_BH.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_ALLSites_BH.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

save( Diurnal.AE.H2O , Diurnal.MBR.H2O,  Diurnal.WP.H2O,
      Diurnal.AE.CO2 , Diurnal.MBR.CO2,  Diurnal.WP.CO2,
      file="/Volumes/MaloneLab/Research/FluxGradient/Diurnal_ALLSites_BH.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/Diurnal_ALLSites_BH.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Carbon Exchange PARMS: ####

load("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_ALLSites_BH.Rdata")

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.cparms.R')

save( SITES_MBR_30min_CPARMS_FG ,
      SITES_MBR_30min_CPARMS_EC ,
      SITES_AE_30min_CPARMS_FG,
      SITES_AE_30min_CPARMS_EC,
      SITES_WP_30min_CPARMS_EC, 
      SITES_WP_30min_CPARMS_FG ,
      MBR.CPARMS,
      AE.CPARMS ,
      WP.CPARMS,
      file= '/Volumes/MaloneLab/Research/FluxGradient/CarbonParms.Rdata')

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/CarbonParms.Rdata')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
