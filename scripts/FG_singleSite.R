# Load Libraries and Set Environment Vars ---------------------------------
#load libaries
library(neonUtilities)
library(readr)
library(BiocManager)
library(rhdf5)
library(dplyr)
library(oce)
library(tidyr)
library(lubridate)
library(naniar)
library(ggplot2)
library(R.utils)
library(gtools)
#load in FG specific functions
source("~/fluxGradient/lterwg-flux-gradient/R/SiteAttributes.R")
source("~/fluxGradient/lterwg-flux-gradient/R/SiteDF.R")
source("~/fluxGradient/lterwg-flux-gradient/R/hdf2df.R")
source("~/fluxGradient/lterwg-flux-gradient/R/Flux_Gradient_MBR.R")
source("~/fluxGradient/lterwg-flux-gradient/R/Flux_Gradient_AE.R")
source("~/fluxGradient/lterwg-flux-gradient/R/time_format.R")
#set up dir to store data
Main.Directory <- c('//corellia.environment.yale.edu/MaloneLab/alexis.helgeson/fluxGradient')
#set desired site name and NEON code
sitename <- 'Harvard_Forest'
sitecode <- 'HARV'
#set start and end date for obs
startdate <- "2022-05"
enddate <- "2022-07"
#create site folders and setwd
site.dir <- paste(Main.Directory,"/",sitename, sep="")
#if(!exists(site.dir)){dir.create(site.dir)}
setwd(paste0(site.dir, "/filesToStack00200/"))

# Call Fcns to Calculate CH4 fluxes ---------------------------------------
#grab h5 files to be passed to SiteAttributes and SiteDF
hd.files <-list.files(pattern="\\.h5$")
#grab attribute data
attr.df <- SiteAttributes(hd.files, sitecode)
#save df as csv
write.csv(attr.df, paste0(sitecode, "_", startdate, "_", enddate, "_attr.csv"))
#grab co2, h20, ch4 level 1 data at all 30min resolution tower heights along with level 4 co2, sensible heat, latent heat fluxes, uStar, uBar, air temp, z0
cont.df <- Site.DF(hd.files, sitecode)
#save df as csv
write.csv(cont.df, paste0(sitecode, "_", startdate, "_", enddate, "_cont.csv"))
#calculate flux gradient
#select lower twoer height
z1_height <- attr.df$DistZaxsLvlMeasTow[1]
#select upper tower height
z2_height <- attr.df$DistZaxsLvlMeasTow[6]
#select tallest height for sonic anemometer
z_height <- attr.df$DistZaxsLvlMeasTow[6]
#calculate fluxes using Modified Bowan Ratio
mbr.df <- Flux_Gradient_MBR(cont.df, attr.df, z1_height, z2_height)
#save df as csv
write.csv(mbr.df, paste0(sitecode, "_", startdate, "_", enddate, "_mbr.csv"))
#calculate fluxes using aerodynamic profile and wind profile
ae.df <- Flux_Gradient_AE(cont.df, attr.df, z1_height, z2_height, z_height)
#save df as csv
write.csv(ae.df, paste0(sitecode, "_", startdate, "_", enddate, "_ae.csv"))