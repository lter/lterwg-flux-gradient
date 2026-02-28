# This workflow processes the validation data:

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)

# -------------- Change this stuff -------------

# Add local directory for downloaded data here:
localdir1 <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData' # MaloneLab Server

# Add local directory for your Flux repo here:
localdir2 <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(localdir2)

DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir
email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)
# -------------------------------------------------------

# Create Files in the NEON format for validation sites: ####
# Files saved to the localdir!

setwd(localdir2 )
sitelist <- c('SE-Sto', 'SE-Svb', 'US-Uaf')
dirTmp <- localdir1 
#source(paste(localdir2, '/validation/flow.FI-Hyy.data.format.conc.diffs.R', sep="") )
source(paste(localdir2, '/validation/flow.SE-Sto.data.format.conc.diffs.R', sep="") )
source(paste(localdir2, '/validation/flow.SE-Svb.data.format.conc.diffs.R', sep="") )
source(paste(localdir2, '/validation/flow.US-Uaf.data.format.conc.diffs.R', sep="") )

# Calculate Gradient Fluxes: ####
## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Uses the aligned concentration file combined with the 30min and 9min data files to calculates fluxes and saves locally. 


# Output(s):
# SITE_AE_9min.Rdata (local & Google Drive)
# SITE_AE_9min.zip (local)
# SITE_MBR_9min.RData (local)
# SITE_MBR_9min.zip (local & Google Drive)
# SITE_WP_9min.Rdata (local)
# SITE_WP_9min.zip (local & Google Drive)

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

# GF Calculation:  ####
for(site in site.list){
  
  setwd(localdir2)
  
  sitecode <- site
  print(sitecode)
  
  # Load Data:
  load(fs::path(localdir1, site, paste0(site, '_aligned_conc_flux_9min.RData')))
  
  dirTmp <- file.path(localdir1, site)
  
  print('Data Loaded')
  
  print('Running MBR')
  source(file.path("workflows", "flow.calc.flag.mbr.batch.R"))
  print('MBR Done')

  setwd(localdir2)
  print('Running AE + WP')
 source(file.path("workflows", "flow.calc.flag.aero.batch.R"))
  #print('AE +WP Done')
  
  print('done')
  rm(min9)
}

# Format the data into _Evaluation for Filtering: ####
for(site in site.list){
  print(site)   
  
  site <- site
  
  setwd(file.path(localdir1, site))
  load(paste(site, "_WP_9min.Rdata", sep = ""))
  load(paste(site, "_AE_9min.Rdata", sep = ""))
  load(paste(site, "_MBR_9min.Rdata", sep = ""))
  
  
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
  
  MBRflux_align$site <- paste0(site)
  
  #bind list into df
  
  WP_9min.df <- bind_rows(min9.FG.WP.list) 
  
  AE_9min.df <- bind_rows(min9.FG.AE.list) 
  
  MBR_9min.df <- bind_rows(MBRflux_align) 
    
 
  # Add flags to dataframe:
  WP_9min.df.flag <- flag.all.gas.stability(flux.df = WP_9min.df, 
                                            L = 'L_obukhov', 
                                            z = 'z_veg_aero', 
                                            d = 'z_displ_calc')
  
  AE_9min.df.flag <- flag.all.gas.stability(flux.df = AE_9min.df, 
                                            L = 'L_obukhov', 
                                            z = 'z_veg_aero', 
                                            d = 'z_displ_calc')
  
  MBR_9min.df.flag <- flag.all.gas.stability(flux.df = MBR_9min.df, 
                                             L = 'L_obukhov_CO2', 
                                             z = 'z_veg_aero_CO2', 
                                             d = 'z_displ_calc_CO2')
  
  
  # Calculate the difference between EC and gradient FLux:
  AE_9min.df <- AE_9min.df %>% mutate(Diff_EC_GF = FC_turb_interp - FG_mean)
  WP_9min.df <- WP_9min.df %>% mutate(Diff_EC_GF = FC_turb_interp - FG_mean)
  
  # Cross Gradient Calculations:
  AE_9min.df.CG <- eddy_diff_real(AE_9min.df.flag) %>% cross_grad_flag(Kgas)
  WP_9min.df.CG <- eddy_diff_real(WP_9min.df.flag) %>% cross_grad_flag(Kgas)
  
  # Eddy Diffusivity:
  AE_9min.df.final <- Bad_Eddy(AE_9min.df.CG, "EddyDiff")
  WP_9min.df.final <- Bad_Eddy(WP_9min.df.CG, "EddyDiff")
  
  # Additional Formatting for the MBR:
  MBR_9min.df.final <- format.MBR(MBR_9min.df.flag)
  
  # Ustar Threshold:
  ustar.Threshold <- ustar.neon.sites %>% filter(Site_Id.NEON == site ) %>% select(Threshold.final)

  if( site %in% c("SE-Sto", "SE-Svb", "US-Uaf") == "FALSE" ){
    WP_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
    AE_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
    MBR_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
  } else{
    ustar = 0
    
    WP_9min.df.final$ustar_threshold <- ustar
    AE_9min.df.final$ustar_threshold <- ustar
    MBR_9min.df.final$ustar_threshold <- ustar
  }
  # Save the files
  site.dir <- file.path(localdir1, site)
  
  save(WP_9min.df.final,
       AE_9min.df.final,
       MBR_9min.df.final, file = file.path(site.dir, paste0(site, "_Evaluation.RDATA")))
  
  rm(WP_9min.df, 
     AE_9min.df,
     MBR_9min.df, 
     
     WP_9min.df.flag, 
     AE_9min.df.flag, 
     MBR_9min.df.flag, 
     
     WP_9min.df.CG, 
     AE_9min.df.CG, 
     
     WP_9min.df.final, 
     
     min9.FG.WP.list,
     min9.FG.AE.list, MBRflux_align, files2save )
  
  # Upload to the google drive:
  fileZip <- fs::path(site.dir, paste0(site, "_Evaluation.RDATA"))
  
  googledrive::drive_upload(media = fileZip, 
                            overwrite = T, 
                            path = data_folder$id[data_folder$name==site])
  
  print(paste("Done with", site))
  
}

message("Next filter the data with ....") 
