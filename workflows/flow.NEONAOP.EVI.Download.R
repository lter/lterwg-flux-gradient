# Import and Prcess NEON AOP Data t Produce EVI tifs.

#https://www.neonscience.org/resources/learning-hub/tutorials/merge-aop-raster-data

library('neonUtilities')
library('raster')
library('data.table')
library('docstring')
library('gdalUtilities')

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")

source("aop_merge_raster_functions.R")

NEON_TOKEN <-'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzcGFya2xlbG1hbG9uZUBnbWFpbC5jb20iLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4NjgxNTUzODQsImlhdCI6MTcxMDQ3NTM4NCwiZW1haWwiOiJzcGFya2xlbG1hbG9uZUBnbWFpbC5jb20ifQ.A9PxSOT-3FxbAbxV7xqkM1Ps3OqMnzZcTe14PK3Vi16BaCdz_ClmPGqzLRxD8K61Mv-6XouIf8ToaqnP-NXxnQ'

wd <- "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic"

download_folder <-getwd()

chm_output_folder <- "Output/KONZ"
makeFullSiteMosaics('DP3.30026.001','2020','KONZ',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/BONA"
makeFullSiteMosaics('DP3.30026.001','2021','BONA',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/CPER"
makeFullSiteMosaics('DP3.30026.001','2021','CPER',download_folder,chm_output_folder,NEON_TOKEN)

# Try the DP2.30026.001 since DP3.30026.001 is bogus.
setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/GUAN"
makeFullSiteMosaics('DP3.30026.001','2018','GUAN',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/HARV"
makeFullSiteMosaics('DP3.30026.001','2019','HARV',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/NIWO"
makeFullSiteMosaics('DP3.30026.001','2020','NIWO',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/TOOL"
makeFullSiteMosaics('DP3.30026.001','2019','TOOL',download_folder,chm_output_folder,NEON_TOKEN)

setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic")
chm_output_folder <- "Output/JORN"
makeFullSiteMosaics('DP3.30026.001','2021','JORN',download_folder,chm_output_folder,NEON_TOKEN)

# Need to download a different date for GUAN. the data doesnt 
# Load EVI tiffs for NEON sites:
BONA.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/BONA/neon-aop-products_EVI.tif")
CPER.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/CPER/neon-aop-products_EVI.tif")
GUAN.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/GUAN/neon-aop-products_EVI.tif")
HARV.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/HARV/neon-aop-products_EVI.tif")
JORN.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/JORN/neon-aop-products_EVI.tif")
KONZ.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/KONZ/neon-aop-products_EVI.tif")
NIWO.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/NIWO/neon-aop-products_EVI.tif")
TOOL.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/TOOL/neon-aop-products_EVI.tif")

# Transform the raster layers to the same CRS as the shapefile:
BONA.evi.proj <- terra::project( BONA.evi, "epsg:4087")
CPER.evi.proj <- terra::project( CPER.evi, "epsg:4087")
GUAN.evi.proj <- terra::project( GUAN.evi, "epsg:4087")
HARV.evi.proj <- terra::project( HARV.evi, "epsg:4087")
JORN.evi.proj <- terra::project( JORN.evi, "epsg:4087")
KONZ.evi.proj <- terra::project( KONZ.evi, "epsg:4087")
NIWO.evi.proj <- terra::project( NIWO.evi, "epsg:4087")
TOOL.evi.proj <- terra::project( TOOL.evi, "epsg:4087")

setwd('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI')

writeRaster(BONA.evi.proj, "BONA_evi_2021.tif" , overwrite=T)
writeRaster(CPER.evi.proj, "CPER_evi_2021.tif", overwrite=T )
writeRaster(GUAN.evi.proj, "GUAN_evi_2018.tif" , overwrite=T)
writeRaster(HARV.evi.proj, "HARV_evi_2019.tif" , overwrite=T)
writeRaster(JORN.evi.proj, "JORN_evi_2021.tif" , overwrite=T)
writeRaster(KONZ.evi.proj, "KONZ_evi_2020.tif" , overwrite=T)
writeRaster(NIWO.evi.proj, "NIWO_evi_2019.tif" , overwrite=T)
writeRaster(TOOL.evi.proj, "TOOL_evi_2020.tif" , overwrite=T)

# Final Mosaic files for each site where uploaded to googledrive here: https://drive.google.com/drive/folders/1jDgnK12z6c18G9d6oUszzA_7z4GUtvPW?usp=drive_link
