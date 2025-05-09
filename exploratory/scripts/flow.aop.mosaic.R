# Import and Prcess NEON AOP Data t Produce EVI tifs.

#https://www.neonscience.org/resources/learning-hub/tutorials/merge-aop-raster-data

library('neonUtilities')
library('raster')
library('data.table')
library('docstring')
library('gdalUtilities')

source("~/Dropbox (YSE)/Research/FluxGradient/lterwg-flux-gradient/functions/compile_DataAvailability_AOP.R")

VegetationIndice <- 'DP3.30026.001'
LAI <- 'DP3.30012.001'
Canopy_Height <- 'DP3.30015.001' 
#'DP3.30024.001' - give the componenets to get chm?

VI.availabilityDf <- Site.Data.Availability.AOP(VegetationIndice)
LAI.availabilityDf <- Site.Data.Availability.AOP(LAI)
CH.availabilityDf <- Site.Data.Availability.AOP(Canopy_Height)

source("~/Dropbox (YSE)/Research/FluxGradient/lterwg-flux-gradient/exploratory/aop_merge_raster_functions.R")

NEON_TOKEN <-'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzcGFya2xlbG1hbG9uZUBnbWFpbC5jb20iLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4NjgxNTUzODQsImlhdCI6MTcxMDQ3NTM4NCwiZW1haWwiOiJzcGFya2xlbG1hbG9uZUBnbWFpbC5jb20ifQ.A9PxSOT-3FxbAbxV7xqkM1Ps3OqMnzZcTe14PK3Vi16BaCdz_ClmPGqzLRxD8K61Mv-6XouIf8ToaqnP-NXxnQ'

setwd('/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic')

# Where to download data to:
download_folder <-'/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic'

# LAI Data

for( i in 1:length(LAI.availabilityDf$site )){
  print(i)
  site <- LAI.availabilityDf$site[i]
  year <- LAI.availabilityDf$year[i]
  
  message(paste('Working on', site, "-", year))
  
  chm_output_folder <- paste("Output/",site, sep="")
  
  makeFullSiteMosaics('DP3.30012.001',year,site,download_folder,chm_output_folder,NEON_TOKEN)
  
  message(paste('Done with', site, "-", year))
}

# 20, 32, 40,47
for( i in 47:length(CH.availabilityDf$site )){
  print(i)
  site <- CH.availabilityDf$site[i]
  year <- CH.availabilityDf$year[i]
  
  message(paste('Working on', site, "-", year))
  
  chm_output_folder <- paste("Output/",site, sep="")
  
 try( makeFullSiteMosaics(Canopy_Height,year,site,download_folder,chm_output_folder,NEON_TOKEN), silent =T)
  
  message(paste('Done with', site, "-", year))
}

#40
for( i in 65:length(VI.availabilityDf$site )){
  print(i)
  site <- VI.availabilityDf$site[i]
  year <- VI.availabilityDf$year[i]
  
  message(paste('Working on', site, "-", year))
  
  chm_output_folder <- paste("Output/",site, sep="")
  
  try(makeFullSiteMosaics(VegetationIndice,year,site,download_folder,chm_output_folder,NEON_TOKEN), silent =T)
  
  message(paste('Done with', site, "-", year))
}


dpID =VegetationIndice
year =  VI.availabilityDf$year[64]
siteCode = VI.availabilityDf$site[64]
dataRootDir = download_folder
outFileDir = chm_output_folder
apiToken=NEON_TOKEN



