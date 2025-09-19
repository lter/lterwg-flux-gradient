## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Develops the validation dataframes needed to perform the evaluation

# Ouput(s):
# SITE_Evaluation.RDATA (local & Google Drive)
# Site_Attributes.csv (local & Google Drive)

rm(list=ls())

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

# Has a list of all the sites
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv')

# Get unique sites
site.list <- metadata$Site_Id.NEON %>% unique()

# Ustar Threshold:
ustar.neon.sites <- read.csv("/Volumes/MaloneLab/Research/FluxGradient/UstarNeonSites.csv" )

# Add local directory for downloaded data here:
localdir1 <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

# Add local directory for your Flux repo here:
localdir2 <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(localdir2)

# Add local directory for your attribute data here:
localdir3 <- '/Volumes/MaloneLab/Research/FluxGradient/Attributes'

# Load functions
source(file.path("functions", "flag.all.gas.stability.R"))
source(file.path("functions", "calc.cross.gradient.R"))
source(file.path("functions", "calc.bad.eddy.R"))
source(file.path("functions", "calc.format.MBR.R"))

## --------------------------------------------- ##
#               Authenticate -----
## --------------------------------------------- ##

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 

# Authenticate with Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

# Data on google drive
data_folder <- googledrive::drive_ls(path = drive_url)

## --------------------------------------------- ##
#               Compile Fluxes -----
## --------------------------------------------- ##

# Compile fluxes downloaded with flow.Download.GoogleDriveData
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
  
  WP_9min.df <- bind_rows(min9.FG.WP.list) #bind list into df
  
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
  
  WP_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
  AE_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
  MBR_9min.df.final$ustar_threshold <- ustar.Threshold$Threshold.final
  
  # Save the files
  site.dir <- file.path(localdir1, site)
  
  save(WP_9min.df.final,
       AE_9min.df.final,
       MBR_9min.df.final, file = file.path(site.dir, paste0(site, "_Evaluation.RDATA")))
  
  rm(WP_9min.df, WP_30min.df,
     AE_9min.df, AE_30min.df,
     MBR_9min.df, MBR_30min.df,
     
     WP_9min.df.flag, WP_30min.df.flag,
     AE_9min.df.flag, AE_30min.df.flag,
     MBR_9min.df.flag, MBR_30min.df.flag,
     
     WP_9min.df.CG, WP_30min.df.CG,
     AE_9min.df.CG, AE_30min.df.CG,
     
     WP_9min.df.final, WP_30min.df.final,
     AE_9min.df.final, AE_30min.df.final,
     MBR_30min.df.final,
     
     min9.FG.WP.list, min30.FG.WP.list,
     min9.FG.AE.list, min30.FG.AE.list, MBRflux_align, files2save )
  
  # Upload to the google drive:
  fileZip <- fs::path(site.dir, paste0(site, "_Evaluation.RDATA"))
  
  googledrive::drive_upload(media = fileZip, 
                            overwrite = T, 
                            path = data_folder$id[data_folder$name==site])
  
  print(paste("Done with", site))
  
}

#Import all validation data of interest 
#all.site.list <- metadata$Site_Id.NEON %>% unique

# There were issues with a few sites: Until addressed remove them:
#site.list <- all.site.list[ all.site.list != 'TEAK'&
#                              all.site.list !='TOOL'&
#                              all.site.list!= 'WREF']

# Add local directory for downloaded data here:
#localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

#SITES_WP_9min <- list()
#SITES_AE_9min <- list()
#SITES_MBR_9min <- list()

#for ( site in site.list){
#  print(site)
  
#  site.dir <- paste(localdir, "/", site, sep="")
  
#  load(file = file.path(paste(site.dir ,paste(site,"_Validation.RDATA", sep=""), sep="/"))  )
  
#  SITES_MBR_9min[[site]] <- MBR_9min.df.final
#  SITES_WP_9min[[site]] <- WP_9min.df.final
# SITES_AE_9min[[site]] <- AE_9min.df.final

#  rm( MBR_9min.df.final, WP_9min.df.final, AE_9min.df.final )
  
#  print(paste(site, "done", sep=""))
#}

# Save files locally and push to google drive:
# Save .Rdata locally:


#save(SITES_WP_9min, file = file.path(paste(localdir, "SITES_WP_9min.Rdata", sep="/")))
#save(SITES_AE_9min, file = file.path(paste(localdir, "SITES_AE_9min.Rdata", sep="/")))
#save(SITES_MBR_9min, file = file.path(paste(localdir, "SITES_MBR_9min.Rdata", sep="/")))

#load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_MBR_9min.Rdata")
#load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_AE_9min.Rdata")
#load("/Volumes/MaloneLab/Research/FluxGradient/FluxData/SITES_WP_9min.Rdata")

# Save Files on google drive: 
#drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")

#fileSave <- file.path(paste(localdir, "SITES_WP_9min.Rdata", sep="/"))
#googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

#fileSave <- file.path(paste(localdir, "SITES_AE_9min.Rdata", sep="/"))
#googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

#fileSave <- file.path(paste(localdir, "SITES_MBR_9min.Rdata", sep="/"))
#googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

## --------------------------------------------- ##
#           Compile Attribute Data -----
## --------------------------------------------- ##

# Attribute file

# There were issues with a few sites: Until addressed remove them:
site.list <- metadata$Site_Id.NEON %>% unique()

setwd(localdir3)

# Import and compile the attribute data!!
site.att <- data.frame()

for(site in site.list){
  print(site)
  
  dir <- file.path("data", site, paste0(site, "_attr.Rdata"))
  
  load(dir)
  
  site.att <- site.att %>% rbind(attr.df)
}

write.csv(site.att, '/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv' )

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv')

googledrive::drive_upload(media = fileSave, 
                          overwrite = T, 
                          path = drive_url)

message("Next run flow.evaluation.batch in the lterwg-flux-gradient-eval repo: ") 