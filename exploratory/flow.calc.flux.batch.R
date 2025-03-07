# flow.batch.flux

library(fs)
library(googledrive)
library(dplyr)
library(stringr)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")


# Add all sites here:
site.list <- c('HARV' ,'KONZ', 'GUAN', 'JORN')

# Add local directory for downloaded data here:
localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data'

# Gradient Flux Calculations: ####
for( site in site.list ){
  sitecode <- site
  print(sitecode)
  
  # Load Data:
  load(fs::path(paste(localdir, site,sep="/"), paste0(site,'_aligned_conc_flux_30min.RData')))
  #load(fs::path(paste(localdir, site,sep="/"), paste0(site,'_aligned_conc_flux_9min.RData')))

  dirTmp <- paste(localdir, site,sep="/")
    
  print('Data Loaded')
  
 # print( 'Running MBR')
  #source(file.path("exploratory/flow.calc.flag.mbr.batch.R"))
  #print('MBR Done')
  
  print( 'Running AE')
  source(file.path("exploratory/flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  print( 'Running WP')
  source(file.path("exploratory/flow.calc.flag.windprof.batch.R"))
  print('WP Done')
  
  print('done')

}

# Validation Dataframe: ####

# Compile fluxes downloaded with flow.Download.GoogleDriveData

rm(list=ls())
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")


# Add all sites here:
site.list <- c('HARV' ,'KONZ', 'GUAN', 'JORN')

# Add local directory for downloaded data here:
localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data'

for( site in site.list){
  print(site)
  
  site <- site
  
  # Create a list to store all data in:
  min9.FG.WP.list <- list()
  min30.FG.WP.list <- list()
  min9.FG.AE.list <- list()
  min30.FG.AE.list <- list()
  MBRflux_align <- list()
  MBRflux_align_30min <- list()
  
  setwd(paste( localdir,"/", site,"/", sep=""))
  load( paste(site, "_WP_9min.Rdata", sep=""))
  load( paste(site, "_AE_9min.Rdata", sep=""))
  load( paste(site, "_MBR_9min.Rdata", sep=""))
  
 
  load( paste(site, "_WP_30min.Rdata", sep=""))
  load( paste(site, "_AE_30min.Rdata", sep=""))
  load( paste(site, "_MBR_30min.Rdata", sep=""))
  
  # Add information to the files to make one large dataframe
  min9.FG.WP.list$H2O$gas <- "H2O"
  min9.FG.WP.list$H2O$site <- paste0(site)
  min9.FG.WP.list$CO2$gas <- "CO2"
  min9.FG.WP.list$CO2$site <- paste0(site)
  min9.FG.WP.list$CH4$gas <- "CH4"
  min9.FG.WP.list$CH4$site <- paste0(site)
  
  min9.FG.AE.list$H2O$gas <- "H2O"
  min9.FG.AE.list$H2O$site <- paste0(site)
  min9.FG.AE.list$CO2$gas <- "CO2"
  min9.FG.AE.list$CO2$site <- paste0(site)
  min9.FG.AE.list$CH4$gas <- "CH4"
  min9.FG.AE.list$CH4$site <- paste0(site)
  
  min30.FG.WP.list$H2O$gas <- "H2O"
  min30.FG.WP.list$H2O$site <- paste0(site)
  min30.FG.WP.list$CO2$gas <- "CO2"
  min30.FG.WP.list$CO2$site <- paste0(site)
  min30.FG.WP.list$CH4$gas <- "CH4"
  min30.FG.WP.list$CH4$site <- paste0(site)
  
  min30.FG.AE.list$H2O$gas <- "H2O"
  min30.FG.AE.list$H2O$site <- paste0(site)
  min30.FG.AE.list$CO2$gas <- "CO2"
  min30.FG.AE.list$CO2$site <- paste0(site)
  min30.FG.AE.list$CH4$gas <- "CH4"
  min30.FG.AE.list$CH4$site <- paste0(site)
  
  MBRflux_align$site <- paste0(site)
  MBRflux_align_30min$site <- paste0(site)
  
  WP_9min.df <- bind_rows(min9.FG.WP.list) #bind list into df
  WP_30min.df <- bind_rows(min30.FG.WP.list) 
  
  AE_9min.df <- bind_rows(min9.FG.AE.list)
  AE_30min.df <- bind_rows(min30.FG.AE.list)
  
  MBR_9min.df <- bind_rows(MBRflux_align)
  MBR_30min.df <- bind_rows(MBRflux_align_30min)
  
  assign(paste(site, "_WP_9min", sep=""),  WP_9min.df)
  assign(paste(site, "_WP_30min", sep=""), WP_30min.df)
  assign(paste(site, "_AE_9min", sep=""),  AE_9min.df)
  assign(paste(site, "_AE_30min", sep=""), AE_30min.df)
  assign(paste(site, "_MBR_9min", sep=""),  MBR_9min.df)
  assign(paste(site, "_MBR_30min", sep=""), MBR_30min.df)
  
  rm( WP_9min.df , WP_30min.df,
      AE_9min.df, AE_30min.df,
      MBR_9min.df, MBR_30min.df,min9.FG.WP.list, min30.FG.WP.list,
      min9.FG.AE.list, min30.FG.AE.list, MBRflux_align, MBRflux_align_30min )

}




SITES_WP_9min <- list(mget(ls(pattern = "WP_9min")))[[1]]
SITES_WP_30min <- list(mget(ls(pattern = "WP_30min")))[[1]]
SITES_AE_9min <- list(mget(ls(pattern = "AE_9min")))[[1]]
SITES_AE_30min <- list(mget(ls(pattern = "AE_30min")))[[1]]
SITES_MBR_9min <- list(mget(ls(pattern = "MBR_9min")))[[1]]
SITES_MBR_30min <- list(mget(ls(pattern = "MBR_30min")))[[1]]

# Extract the site names and replace DF name in the list:
names(SITES_WP_9min) <-substr(names(SITES_WP_9min) , start = 1, stop = 4)
names(SITES_WP_30min) <-substr( names(SITES_WP_30min) , start = 1, stop = 4)
names(SITES_AE_9min) <-substr(names(SITES_AE_9min) , start = 1, stop = 4)
names(SITES_AE_30min) <-substr( names(SITES_AE_30min) , start = 1, stop = 4)
names(SITES_MBR_9min) <-substr(names(SITES_MBR_9min) , start = 1, stop = 4)
names(SITES_MBR_30min) <-substr( names(SITES_MBR_30min) , start = 1, stop = 4)

# Save .Rdata locally:
save(SITES_WP_9min, file = file.path(paste(localdir, "SITES_WP_9min.Rdata", sep="/")) )
save(SITES_WP_30min, file = file.path(paste(localdir, "SITES_WP_30min.Rdata", sep="/")))
save(SITES_AE_9min, file = file.path(paste(localdir, "SITES_AE_9min.Rdata", sep="/")))
save(SITES_AE_30min, file = file.path(paste(localdir, "SITES_AE_30min.Rdata", sep="/")))
save(SITES_MBR_9min, file = file.path(paste(localdir, "SITES_MBR_9min.Rdata", sep="/")))
save(SITES_MBR_30min, file = file.path(paste(localdir, "SITES_MBR_30min.Rdata", sep="/")))

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")

#upload to g drive

googledrive::drive_upload(media =file.path(paste(localdir, "SITES_WP_9min.Rdata", sep="/")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path(paste(localdir, "SITES_WP_30min.Rdata", sep="/")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path(paste(localdir, "SITES_AE_9min.Rdata", sep="/")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path(paste(localdir, "SITES_AE_30min.Rdata", sep="/")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path(paste(localdir, "SITES_MBR_9min.Rdata", sep="/")), overwrite = T, path = drive_url)
googledrive::drive_upload(media = file.path(paste(localdir, "SITES_MBR_30min.Rdata", sep="/")), overwrite = T, path = drive_url)

