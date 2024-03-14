# Spatial Homogeneity:

library(googlesheets4)
library( sf)
library(terra)
library(AOI)
library(ggplot2)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = email) 

Sites <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1VkqT-PSdV8Ak_Ji4cdTDs7SZTNBOId4bZDJGMcXqrAI/edit?usp=drive_link')

Sites.shp <- st_as_sf(x = Sites,                         
                            coords = c("Longitude",  "Latitude"),
                            crs = "+init=epsg:4326")



aoi.usa <- AOI::aoi_get(country= c("USA", "PR") )

ggplot( ) + geom_sf(data=aoi.usa) + geom_sf(data=Sites.shp) +
  geom_spatrat( )

# Create Buffers
Sites.shp.proj <- sf::st_transform(Sites.shp ,crs = "+init=epsg:4087" )
Sites.shp.30 <-sf::st_buffer(x=Sites.shp.proj, dist=30)
Sites.shp.90 <-sf::st_buffer(x=Sites.shp.proj, dist=90)
Sites.shp.450 <-sf::st_buffer(x=Sites.shp.proj, dist=450)
Sites.shp.950 <-sf::st_buffer(x=Sites.shp.proj, dist=950)
Sites.shp.1800 <-sf::st_buffer(x=Sites.shp.proj, dist=1800)

plot(Sites.shp.1800$geometry[1])
plot(Sites.shp.950$geometry[1], add=T)
plot(Sites.shp.450$geometry[1], add=T)
plot(Sites.shp.90$geometry[1], add=T)
plot(Sites.shp.30$geometry[1], add=T)

# Calculate zonal statistics for the buffers:

# Import 

konz <- rast('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/NEON.D06.KONZ.DP3.30026.001.2016-07.basic.20240314T163213Z.RELEASE-2024/NEON_D06_KONZ_DP3_700000_4328000_VegIndices/NEON_D06_KONZ_DP3_700000_4328000_EVI.tif')
konz <-terra::project()
Sites.shp.1800 <- st_transform(Sites.shp.1800, crs='epsg:32614'  )

plot(Sites.shp.1800$geometry[Sites.shp.1800$Site.Id.AF == "US-xKZ"] )
plot(konz, add=T )

Sites.shp.1800 <- zonal(x = EVI, 
                            z= vect(Sites.shp.1800) , 
                        fun = "var", as.polygons=TRUE,  
                        na.rm=TRUE)

