# Canopy Diversity Indices:

library(terra)
library(sf)
library(tidyverse)
library(gtools)

load(file='/Volumes/MaloneLab/Research/FluxGradient/FG_Site_Wdges.RDATA')

st_crs(site.Buffers)

# 
diversity.dir <- "/Volumes/MaloneLab/Research/FluxGradient/StructuralDiversity_rasters"

folders <- list.files(diversity.dir )

# Files to save information:
Structural.Diversity.mean <- data.frame()
Structural.Diversity.sd <- data.frame()

for( i in folders[90:243]){
  
  print(paste("Working in folder: ",i))
  
  site.year.dir <- paste(diversity.dir,"/", i , sep="")
  files.site <- list.files(site.year.dir, patter=".tif$")
  
  site.rasters <- terra::rast(paste(site.year.dir, "/",files.site , sep="")) %>% terra::project('epsg:4326')
  
  names.raster <- names(site.rasters)
  site.names <- names.raster %>% substr(start = 10, stop = 13)%>% unique
  site.year <- names.raster %>% substr(start = 5, stop =8 ) %>% unique
  names(site.rasters) <- names.raster %>% substr(start = 17, stop =40 )
  
  site.Buffers.sub <- site.Buffers %>% dplyr::filter(site == site.names)

  if( length(site.Buffers.sub$geometry) > 0  ) {
  # Loop through the shapefiles for the site to extract the mean and SD.
  for( a in 1:length( site.Buffers.sub$geometry )){
    print(paste("Working in folder: ",i, "buffer =",a))
    
    raster.sub <- terra::extract(site.rasters, site.Buffers.sub[a,], 
                                touches=TRUE) 
    
    summary.mean <- summarise_all(raster.sub , .funs= "mean", na.rm=T)
    summary.sd <-summarise_all(raster.sub , .funs= "sd", na.rm=T)
    
    summary.sd$Site <-summary.mean$Site <- Sites.wedges.sub[a,]$site
    summary.sd$dist.m <-summary.mean$dist.m <- Sites.wedges.sub[a,]$dist.m
    summary.sd$wedge <-summary.mean$wedge <- Sites.wedges.sub[a,]$wedge 
    
    Structural.Diversity.mean <- smartbind(Structural.Diversity.mean, summary.mean)
    Structural.Diversity.sd <- smartbind(Structural.Diversity.sd,  summary.sd)
    
  }
    } else{ print("There are no buffers for the site")}
  
}


Structural.Diversity.mean <- Structural.Diversity.mean %>% distinct
Structural.Diversity.sd <- Structural.Diversity.sd %>% distinct

save( Structural.Diversity.mean, Structural.Diversity.sd, 
      file='/Volumes/MaloneLab/Research/FluxGradient/Structral_Diversity.Rdata' )




