
library(terra)
library(sf)
library(tidyverse)

load(file='/Volumes/MaloneLab/Research/FluxGradient/FG_Site_Wdges.RDATA')

filterOutofBounds <- function(raster, min, max){
  raster[raster > max]<- NA
  raster[raster < min]<- NA
  return(raster)
}

# Make a list of folders of interest:
setwd('/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output')
folders <- list.files(path='/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output')

# EVI
for(s in folders){
  print(paste("Calculateing EVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  evi.files <- list.files(path=path, pattern="*EVI.tif$")
  print(paste("The following files are available",evi.files))
  
for(raster in evi.files){
  
  print(paste("Working on ",raster))
  # Subset the simple feature:
  Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
 
  if(length(Sites.wedges.sub$Name ) > 0){
   try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
    try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
    
    year <- substr(raster, start = 1, stop = 4)
    
    for( w in 1:length(Sites.wedges.sub$geometry )){
      print(w)
      
      try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
      try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
      
      try(Sites.wedges.sub$EVI.mean[w] <-  test.mean$mean, silent =T)
      try(Sites.wedges.sub$EVI.sd[w] <-  test.sd$sd, silent =T)
      try(Sites.wedges.sub$EVI.year[w] <-  year, silent =T)
      rm(r1, r2, test.mean, test.sd)
    }
    
    if(exists('Sites.EVI') == TRUE ){
      try(Sites.EVI <- rbind(Sites.EVI , Sites.wedges.sub), silent =T)
    } else{Sites.EVI <- Sites.wedges.sub }
  }
 
  
  print(paste("Done with",raster))
}
}

write.csv( Sites.EVI , '/Volumes/MaloneLab/Research/FluxGradient/Sites_EVI.csv')

# NDVI
for(s in folders){
  print(paste("Calculateing NDVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  NDVI.files <- list.files(path=path, pattern="*NDVI.tif$")
  print(paste("The following files are available",NDVI.files))
  
  for(raster in NDVI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
    
    if(length(Sites.wedges.sub$Name ) > 0){
      try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
      try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
      
      year <- substr(raster, start = 1, stop = 4)
      
      for( w in 1:length(Sites.wedges.sub$geometry )){
        print(w)
        
        try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
        try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
        
        try(Sites.wedges.sub$NDVI.mean[w] <-  test.mean$mean, silent =T)
        try(Sites.wedges.sub$NDVI.sd[w] <-  test.sd$sd, silent =T)
        try(Sites.wedges.sub$NDVI.year[w] <-  year, silent =T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.NDVI') == TRUE ){
        try(Sites.NDVI <- rbind(Sites.NDVI , Sites.wedges.sub), silent =T)
      } else{Sites.NDVI <- Sites.wedges.sub }
    }
    
    
    print(paste("Done with",raster))
  }
}

write.csv( Sites.NDVI , '/Volumes/MaloneLab/Research/FluxGradient/Sites_NDVI.csv')

# LAI
for(s in folders){
  print(paste("Calculateing LAI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  LAI.files <- list.files(path=path, pattern="*LAI.tif$")
  print(paste("The following files are available",LAI.files))
  
  for(raster in LAI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
    
    if(length(Sites.wedges.sub$Name ) > 0){
      try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
      try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
      
      year <- substr(raster, start = 1, stop = 4)
      
      for( w in 1:length(Sites.wedges.sub$geometry )){
        print(w)
        
        try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
        try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
        
        try(Sites.wedges.sub$LAI.mean[w] <-  test.mean$mean, silent =T)
        try(Sites.wedges.sub$LAI.sd[w] <-  test.sd$sd, silent =T)
        try(Sites.wedges.sub$LAI.year[w] <-  year, silent =T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.LAI') == TRUE ){
        try(Sites.LAI <- rbind(Sites.LAI , Sites.wedges.sub), silent =T)
      } else{Sites.LAI <- Sites.wedges.sub }
    }
    
    
    print(paste("Done with",raster))
  }
}

write.csv( Sites.LAI , '/Volumes/MaloneLab/Research/FluxGradient/Sites_LAI.csv')

# PRI
for(s in folders){
  print(paste("Calculateing PRI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  PRI.files <- list.files(path=path, pattern="*PRI.tif$")
  print(paste("The following files are available",PRI.files))
  
  for(raster in PRI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
    
    if(length(Sites.wedges.sub$Name ) > 0){
      try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
      try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
      
      year <- substr(raster, start = 1, stop = 4)
      
      for( w in 1:length(Sites.wedges.sub$geometry )){
        print(w)
        
        try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
        try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
        
        try(Sites.wedges.sub$PRI.mean[w] <-  test.mean$mean, silent =T)
        try(Sites.wedges.sub$PRI.sd[w] <-  test.sd$sd, silent =T)
        try(Sites.wedges.sub$PRI.year[w] <-  year, silent =T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.PRI') == TRUE ){
        try(Sites.PRI <- rbind(Sites.PRI , Sites.wedges.sub), silent =T)
      } else{Sites.PRI <- Sites.wedges.sub }
    }
    
    
    print(paste("Done with",raster))
  }
}

write.csv( Sites.PRI , '/Volumes/MaloneLab/Research/FluxGradient/Sites_PRI.csv')

# CHM
for(s in folders){
  print(paste("Calculateing CHM summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  CHM.files <- list.files(path=path, pattern="*CHM.tif$")
  print(paste("The following files are available",CHM.files))
  
  for(raster in CHM.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
    
    if(length(Sites.wedges.sub$Name ) > 0){
      try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
      try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
      
      year <- substr(raster, start = 1, stop = 4)
      
      for( w in 1:length(Sites.wedges.sub$geometry )){
        print(w)
        
        try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
        try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
        
        try(Sites.wedges.sub$CHM.mean[w] <-  test.mean$mean, silent =T)
        try(Sites.wedges.sub$CHM.sd[w] <-  test.sd$sd, silent =T)
        try(Sites.wedges.sub$CHM.year[w] <-  year, silent =T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.CHM') == TRUE ){
        try(Sites.CHM <- rbind(Sites.CHM , Sites.wedges.sub), silent =T)
      } else{Sites.CHM <- Sites.wedges.sub }
    }
    
    
    print(paste("Done with",raster))
  }
}

write.csv( Sites.CHM , '/Volumes/MaloneLab/Research/FluxGradient/Sites_CHM.csv')

# SAVI
for(s in folders){
  print(paste("Calculateing SAVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  SAVI.files <- list.files(path=path, pattern="*SAVI.tif$")
  print(paste("The following files are available",SAVI.files))
  
  for(raster in SAVI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter( site == site.id)
    
    if(length(Sites.wedges.sub$Name ) > 0){
      try( r1 <- terra::rast( paste(path,raster, sep="/")) %>% terra::project( Sites.wedges.sub), silent =T)
      try( r2 <- r1 %>% filterOutofBounds(min=0, max=1 ), silent =T)
      
      year <- substr(raster, start = 1, stop = 4)
      
      for( w in 1:length(Sites.wedges.sub$geometry )){
        print(w)
        
        try(test.mean <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T) , silent =T)
        try(test.sd <- crop( r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T) , silent =T)
        
        try(Sites.wedges.sub$SAVI.mean[w] <-  test.mean$mean, silent =T)
        try(Sites.wedges.sub$SAVI.sd[w] <-  test.sd$sd, silent =T)
        try(Sites.wedges.sub$SAVI.year[w] <-  year, silent =T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.SAVI') == TRUE ){
        try(Sites.SAVI <- rbind(Sites.SAVI , Sites.wedges.sub), silent =T)
      } else{Sites.SAVI <- Sites.wedges.sub }
    }
    
    
    print(paste("Done with",raster))
  }
}

write.csv( Sites.SAVI , '/Volumes/MaloneLab/Research/FluxGradient/Sites_SAVI.csv')

