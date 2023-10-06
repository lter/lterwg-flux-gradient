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
#grab co2, h20, ch4 level 1 data at all 30min resolution tower heights along with level 4 co2, sensible heat, latent heat fluxes
cont.df <- Site.DF(hd.files, sitecode)
#calculate flux gradient
mbr.df <- Flux_Gradient_MBR(cont.df, attr.df) #what do we want this end df to be used for? do we need to keep all the height concentrations?
