
# Create Square Buffers for LTER-NEON co located sites using this file from google drive:

# Load libraries:
library( sf)
library(ggplot2)

# Import the csv of sites and their locations. 
Sites <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv')

# Make dataframe a shapfile:
Sites.shp <- st_as_sf(x = Sites,                         
                      coords = c("Longitude..degrees.",  "Latitude..degrees."),
                      crs = "epsg:4326")

# Create a USA AOI:
aoi.usa <- AOI::aoi_get(country= c("USA", "PR") )

# Visualize the point locations within the USA:
ggplot( ) + geom_sf(data=aoi.usa) + geom_sf(data=Sites.shp)

# Add projection to the shape file crs:
Sites.shp.proj <- sf::st_transform(Sites.shp ,crs = "epsg:4087" )

# Create square buffers:
Sites.shp.30 <-sf::st_buffer(x=Sites.shp.proj, dist= 30, endCapStyle = "SQUARE")
Sites.shp.30$dist_m <- 30
Sites.shp.90 <-sf::st_buffer(x=Sites.shp.proj, dist= 90, endCapStyle = "SQUARE")
Sites.shp.90$dist_m <- 90
Sites.shp.450 <-sf::st_buffer(x=Sites.shp.proj, dist= 450, endCapStyle = "SQUARE")
Sites.shp.450$dist_m <- 450
Sites.shp.900 <-sf::st_buffer(x=Sites.shp.proj, dist= 900, endCapStyle = "SQUARE")
Sites.shp.900$dist_m <- 900
Sites.shp.1800 <-sf::st_buffer(x=Sites.shp.proj, dist= 1800, endCapStyle = "SQUARE")
Sites.shp.1800$dist_m <- 1800

# merge all buffer sizes into a single shapefile:
Site.Buffers <- rbind( Sites.shp.30, Sites.shp.90, Sites.shp.450, Sites.shp.900, Sites.shp.1800)

Site.Buffers$site <- Site.Buffers$Site_Id.NEON

# Write shapefiles for use in Site.Spatial.Homo
save(Site.Buffers, file='/Volumes/MaloneLab/Research/FluxGradient/NEONLTERsiteBuffers.Rdata')


message("Next run flow.neon.site.simplefeatures.R")
