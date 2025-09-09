library(terra)
library(sf)
library(tidyverse)

load(file='/Volumes/MaloneLab/Research/FluxGradient/FG_Site_Wdges.RDATA')

# Only modify the filterOutofBounds function to accept variable min/max
filterOutofBounds <- function(raster, min, max){
  raster[raster > max] <- NA
  raster[raster < min] <- NA
  return(raster)
}

# Make a list of folders of interest:
setwd('/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output')
folders <- list.files(path='/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output')

# EVI
for(s in folders){
  print(paste("Calculating EVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  evi.files <- list.files(path=path, pattern="*EVI.tif$")
  print(paste("The following files are available",evi.files))
  
  for(raster in evi.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=0, max=1), silent=T)  # Keep EVI range as 0-1
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$EVI.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$EVI.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$EVI.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.EVI') == TRUE){
        try(Sites.EVI <- rbind(Sites.EVI, Sites.wedges.sub), silent=T)
      } else{Sites.EVI <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.EVI, '/Volumes/MaloneLab/Research/FluxGradient/Sites_EVI.csv')

# NDVI
for(s in folders){
  print(paste("Calculating NDVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  NDVI.files <- list.files(path=path, pattern="*NDVI.tif$")
  print(paste("The following files are available",NDVI.files))
  
  for(raster in NDVI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=0, max=1), silent=T)  # Keep NDVI range as 0-1
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$NDVI.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$NDVI.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$NDVI.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.NDVI') == TRUE){
        try(Sites.NDVI <- rbind(Sites.NDVI, Sites.wedges.sub), silent=T)
      } else{Sites.NDVI <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.NDVI, '/Volumes/MaloneLab/Research/FluxGradient/Sites_NDVI.csv')

# LAI
for(s in folders){
  print(paste("Calculating LAI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  LAI.files <- list.files(path=path, pattern="*LAI.tif$")
  print(paste("The following files are available",LAI.files))
  
  for(raster in LAI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=0, max=15), silent=T)  # Changed LAI range to 0-12
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$LAI.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$LAI.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$LAI.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.LAI') == TRUE){
        try(Sites.LAI <- rbind(Sites.LAI, Sites.wedges.sub), silent=T)
      } else{Sites.LAI <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.LAI, '/Volumes/MaloneLab/Research/FluxGradient/Sites_LAI.csv')

# PRI
for(s in folders){
  print(paste("Calculating PRI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  PRI.files <- list.files(path=path, pattern="*PRI.tif$")
  print(paste("The following files are available",PRI.files))
  
  for(raster in PRI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=-1, max=1), silent=T)  # Changed PRI range to -0.2 to 0.2
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$PRI.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$PRI.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$PRI.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.PRI') == TRUE){
        try(Sites.PRI <- rbind(Sites.PRI, Sites.wedges.sub), silent=T)
      } else{Sites.PRI <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.PRI, '/Volumes/MaloneLab/Research/FluxGradient/Sites_PRI.csv')

# CHM
for(s in folders){
  print(paste("Calculating CHM summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  CHM.files <- list.files(path=path, pattern="*CHM.tif$")
  print(paste("The following files are available",CHM.files))
  
  for(raster in CHM.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=0, max=50), silent=T)  # Changed CHM range to 0-60
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$CHM.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$CHM.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$CHM.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.CHM') == TRUE){
        try(Sites.CHM <- rbind(Sites.CHM, Sites.wedges.sub), silent=T)
      } else{Sites.CHM <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.CHM, '/Volumes/MaloneLab/Research/FluxGradient/Sites_CHM.csv')

# SAVI
for(s in folders){
  print(paste("Calculating SAVI summary for",s))
  path <- paste("./",s, sep="" )
  site.id <- s
  
  # Import specific files
  SAVI.files <- list.files(path=path, pattern="*SAVI.tif$")
  print(paste("The following files are available",SAVI.files))
  
  for(raster in SAVI.files){
    
    print(paste("Working on ",raster))
    # Subset the simple feature:
    Sites.wedges.sub <- site.buffers.wedges %>% dplyr::filter(site == site.id)
    
    if(length(Sites.wedges.sub$Name) > 0){
      try(r1 <- terra::rast(paste(path,raster, sep="/")) %>% terra::project(Sites.wedges.sub), silent=T)
      try(r2 <- r1 %>% filterOutofBounds(min=-0.6, max=1.0), silent=T)  # Changed SAVI range to -0.6 to 1.0
      
      year <- substr(raster, start = 1, stop = 4)
      
      for(w in 1:length(Sites.wedges.sub$geometry)){
        print(w)
        
        try(test.mean <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('mean', na.rm=T), silent=T)
        try(test.sd <- crop(r2, Sites.wedges.sub$geometry[w]) %>% terra::global('sd', na.rm=T), silent=T)
        
        try(Sites.wedges.sub$SAVI.mean[w] <- test.mean$mean, silent=T)
        try(Sites.wedges.sub$SAVI.sd[w] <- test.sd$sd, silent=T)
        try(Sites.wedges.sub$SAVI.year[w] <- year, silent=T)
        rm(r1, r2, test.mean, test.sd)
      }
      
      if(exists('Sites.SAVI') == TRUE){
        try(Sites.SAVI <- rbind(Sites.SAVI, Sites.wedges.sub), silent=T)
      } else{Sites.SAVI <- Sites.wedges.sub}
    }
    
    print(paste("Done with",raster))
  }
}

write.csv(Sites.SAVI, '/Volumes/MaloneLab/Research/FluxGradient/Sites_SAVI.csv')

# Import and integrate all data files into one:
Sites.EVI <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_EVI.csv')
Sites.NDVI <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_NDVI.csv')
Sites.LAI <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_LAI.csv')
Sites.PRI <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_PRI.csv')
Sites.CHM <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_CHM.csv')
Sites.SAVI <- read_csv('/Volumes/MaloneLab/Research/FluxGradient/Sites_SAVI.csv')

Sites.EVI.summary <- Sites.EVI %>% select(site, EVI.mean, EVI.sd, EVI.year) %>% 
  reframe(.by= site,
          EVI.mean = mean(EVI.mean, na.rm=T), 
          EVI.sd = mean(EVI.sd, na.rm=T), 
          EVI.years = length(EVI.year %>% unique))

Sites.NDVI.summary <- Sites.NDVI %>% select(site, NDVI.mean, NDVI.sd, NDVI.year) %>% 
  reframe(.by= site,
          NDVI.mean = mean(NDVI.mean, na.rm=T), 
          NDVI.sd = mean(NDVI.sd, na.rm=T), 
          NDVI.years = length(NDVI.year %>% unique))

Sites.LAI.summary <- Sites.LAI %>% select(site, LAI.mean, LAI.sd, LAI.year) %>% 
  reframe(.by= site,
          LAI.mean = mean(LAI.mean, na.rm=T), 
          LAI.sd = mean(LAI.sd, na.rm=T), 
          LAI.years = length(LAI.year %>% unique))

Sites.PRI.summary <- Sites.PRI %>% select(site, PRI.mean, PRI.sd, PRI.year) %>% 
  reframe(.by= site,
          PRI.mean = mean(PRI.mean, na.rm=T), 
          PRI.sd = mean(PRI.sd, na.rm=T), 
          PRI.years = length(PRI.year %>% unique))

Sites.CHM.summary <- Sites.CHM %>% select(site, CHM.mean, CHM.sd, CHM.year) %>% 
  reframe(.by= site,
          CHM.mean = mean(CHM.mean, na.rm=T), 
          CHM.sd = mean(CHM.sd, na.rm=T), 
          CHM.years = length(CHM.year %>% unique))

Sites.SAVI.summary <- Sites.SAVI %>% select(site, SAVI.mean, SAVI.sd, SAVI.year) %>% 
  reframe(.by= site,
          SAVI.mean = mean(SAVI.mean, na.rm=T), 
          SAVI.sd = mean(SAVI.sd, na.rm=T), 
          SAVI.years = length(SAVI.year %>% unique))

Sites.Summary <- Sites.EVI.summary %>% full_join(Sites.NDVI.summary, by = 'site') %>% 
  full_join(Sites.LAI.summary, by = 'site') %>%
  full_join(Sites.PRI.summary, by = 'site') %>%
  full_join(Sites.CHM.summary, by = 'site') %>%
  full_join(Sites.SAVI.summary, by = 'site')

save(Sites.Summary, file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

message("Next run flow.structuralDiversity.R")
