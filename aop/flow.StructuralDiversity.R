# Canopy Diversity Indices:
## https://www.nature.com/articles/s41597-024-04018-0#Sec7

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

for( i in folders){
  
  print(paste("Working in folder: ",i))
  
  site.year.dir <- paste(diversity.dir,"/", i , sep="")
  files.site <- list.files(site.year.dir, patter=".tif$")
  
  site.rasters <- terra::rast(paste(site.year.dir, "/",files.site , sep="")) %>% terra::project('epsg:4326')
  
  names.raster <- names(site.rasters)
  site.names <- names.raster %>% substr(start = 10, stop = 13) %>% unique
  site.year <- names.raster %>% substr(start = 5, stop =8 ) %>% unique
  names(site.rasters) <- names.raster %>% substr(start = 17, stop =40 )
  
  site.Buffers.sub <- site.buffers.wedges %>% dplyr::filter(site == site.names) %>% distinct

  if( length(site.Buffers.sub$geometry) > 0  ) {
  # Loop through the shapefiles for the site to extract the mean and SD.
  for( a in 1:length( site.Buffers.sub$geometry )){
    
    Sites.wedges.sub <- site.Buffers.sub[a,]

    print(paste("Working in folder: ",i, "buffer =",a))
    
    raster.sub <- terra::extract(site.rasters, site.Buffers.sub[a,], 
                                touches=TRUE) 
    
    summary.mean <- summarise_all(raster.sub , .funs= "mean", na.rm=T)
    summary.sd <-summarise_all(raster.sub , .funs= "sd", na.rm=T)
    

    summary.sd$Site <-summary.mean$Site <- Sites.wedges.sub$site
    summary.sd$dist.m <-summary.mean$dist.m <- Sites.wedges.sub$dist_m
    summary.sd$wedge <-summary.mean$wedge <- Sites.wedges.sub$wedge 
    summary.sd$Year <-summary.mean$Year <-site.year
    
    Structural.Diversity.mean <- smartbind(Structural.Diversity.mean, summary.mean)
    Structural.Diversity.sd <- smartbind(Structural.Diversity.sd,  summary.sd)
    rm( summary.sd)
  }
    } else{ print("There are no buffers for the site")}
  
}


Structural.Diversity.mean <- Structural.Diversity.mean %>% distinct
Structural.Diversity.sd <- Structural.Diversity.sd %>% distinct


# Summarise the data before writing it outL

Structural.Diversity.mean.summary <- Structural.Diversity.mean %>% filter (wedge == 8) %>%
  group_by(Site) %>% summarise_all('mean', na.rm = T)

Structural.Diversity.mean.summary.sd <- Structural.Diversity.mean %>% filter (wedge == 8) %>%
  group_by(Site) %>% summarise_all('sd', na.rm = T)

save( Structural.Diversity.mean, Structural.Diversity.sd, Structural.Diversity.mean.summary,Structural.Diversity.mean.summary.sd,
      file='/Volumes/MaloneLab/Research/FluxGradient/Structral_Diversity.Rdata' )

message("Next run flow.canopy to bring attribute, AOP, and structural diversity data together.")