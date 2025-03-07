 # Import the AOP mosaics, lable them ...

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
