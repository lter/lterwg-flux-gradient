# Creates the validation dataframe
rm(list=ls())
# Develops the validation dataframes needed to perform the evaluation
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
all.site.list <- metadata$Site_Id.NEON %>% unique

# There were issues with a few sites: Until addressed remove them:
site.list <- all.site.list[ all.site.list != 'TEAK'&
                          all.site.list !='TOOL'&
                           all.site.list!= 'WREF']

# Add local directory for downloaded data here:
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

# Compile fluxes downloaded with flow.Download.GoogleDriveData
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
  
  
  # Add flags to dataframe:
  source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/functions/flag.all.gas.stability.R' )
  
    WP_9min.df.flag <- flag.all.gas.stability(flux.df = WP_9min.df, L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
    WP_30min.df.flag <- flag.all.gas.stability(flux.df = WP_30min.df, L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
   
    AE_9min.df.flag <- flag.all.gas.stability(flux.df =  AE_9min.df, L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
    AE_30min.df.flag <- flag.all.gas.stability(flux.df =  AE_30min.df, L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
    
    MBR_9min.df.flag <- flag.all.gas.stability(flux.df = MBR_9min.df, L='L_obukhov_CO2', z='z_veg_aero_CO2', d='z_displ_calc_CO2')
    MBR_30min.df.flag <- flag.all.gas.stability(flux.df = MBR_30min.df, L='L_obukhov_CO2', z='z_veg_aero_CO2', d='z_displ_calc_CO2')
    
    # Calculate the difference between EC and gradient FLux:
    AE_30min.df <- AE_30min.df %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
    WP_30min.df <-  WP_30min.df %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
    
    # Cross Gradient Calculations:
    source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Flow.CrossGradient.R' )
    AE_9min.df.CG <- eddy_diff_real(AE_9min.df.flag) %>% cross_grad_flag( Kgas)
    AE_30min.df.CG <- eddy_diff_real(AE_30min.df.flag) %>% cross_grad_flag( Kgas)
    WP_9min.df.CG <- eddy_diff_real(WP_9min.df.flag) %>% cross_grad_flag( Kgas)
    WP_30min.df.CG <- eddy_diff_real(WP_30min.df.flag) %>% cross_grad_flag( Kgas)
    
    # Eddy Diffusivity:
    source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Bad_Eddy.R' )
    
    AE_9min.df.final <- Bad_Eddy(  AE_9min.df.CG, "EddyDiff")
    AE_30min.df.final <- Bad_Eddy(  AE_30min.df.CG, "EddyDiff")
    
    WP_9min.df.final <- Bad_Eddy(  WP_9min.df.CG, "EddyDiff")
    WP_30min.df.final <- Bad_Eddy(  WP_30min.df.CG, "EddyDiff")
    
    # Additional Formatting for the MBR:
    source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Function.Format_MBR.R' )
    MBR_30min.df.final <-format_MBR(MBR_30min.df.flag)
    
  # Save the files
    site.dir <- paste(localdir, "/", site, sep="")
    
    save(WP_9min.df.final,
         WP_30min.df.final,
         AE_9min.df.final,
         AE_30min.df.final,
         MBR_30min.df.final, file = file.path(paste(site.dir ,paste(site,"_Validation.RDATA", sep=""), sep="/"))  )

  rm( WP_9min.df , WP_30min.df,
      AE_9min.df, AE_30min.df,
      MBR_9min.df, MBR_30min.df,
      
      WP_9min.df.flag , WP_30min.df.flag,
      AE_9min.df.flag, AE_30min.df.flag,
      MBR_9min.df.flag, MBR_30min.df.flag,
      
      WP_9min.df.CG , WP_30min.df.CG,
      AE_9min.df.CG, AE_30min.df.CG,
      
      WP_9min.df.final , WP_30min.df.final,
      AE_9min.df.final, AE_30min.df.final,
       MBR_30min.df.final,
      
      min9.FG.WP.list, min30.FG.WP.list,
      min9.FG.AE.list, min30.FG.AE.list, MBRflux_align, MBRflux_align_30min,files2save )
  
  print(paste("Done with", site))
  
}

#Import all validation data of interest 
all.site.list <- metadata$Site_Id.NEON %>% unique

# There were issues with a few sites: Until addressed remove them:
site.list <- all.site.list[ all.site.list != 'TEAK'&
                              all.site.list !='TOOL'&
                              all.site.list!= 'WREF']

# Add local directory for downloaded data here:
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

SITES_WP_30min <- list()
SITES_AE_30min <- list()
SITES_MBR_30min <- list()

for ( site in site.list){
  print(site)
  
  site.dir <- paste(localdir, "/", site, sep="")
  
  load(file = file.path(paste(site.dir ,paste(site,"_Validation.RDATA", sep=""), sep="/"))  )
  
  SITES_MBR_30min[[site]] <- MBR_30min.df.final
  SITES_WP_30min[[site]] <- WP_30min.df.final
  SITES_AE_30min[[site]] <- AE_30min.df.final

  rm( MBR_30min.df.final, WP_30min.df.final, AE_30min.df.final )
  
  print(paste(site, "done", sep=""))
}

# Save files locally and push to google drive:
# Save .Rdata locally:


save(SITES_WP_30min, file = file.path(paste(localdir, "SITES_WP_30min.Rdata", sep="/")))
save(SITES_AE_30min, file = file.path(paste(localdir, "SITES_AE_30min.Rdata", sep="/")))
save(SITES_MBR_30min, file = file.path(paste(localdir, "SITES_MBR_30min.Rdata", sep="/")))

load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_MBR_30min.Rdata")
load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_AE_30min.Rdata")
load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_WP_30min.Rdata")

# Save Files on google drive: 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")

fileSave <- file.path(paste(localdir, "SITES_WP_30min.Rdata", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- file.path(paste(localdir, "SITES_AE_30min.Rdata", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- file.path(paste(localdir, "SITES_MBR_30min.Rdata", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Attribute file

# There were issues with a few sites: Until addressed remove them:
site.list <- metadata$Site_Id.NEON %>% unique

setwd('/Volumes/MaloneLab/Research/FluxGradient/Attributes')

# Import and compile the attribute data!!
site.att <-data.frame()

for(site in site.list){
  print(site)
  
dir <- paste('data/',site,"/",site,"_attr.Rdata", sep="" )

load(dir)

site.att <- site.att %>% rbind(attr.df )
}

write.csv(site.att, '/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv' )

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


message("Next run flow.evaluation.batch")



