#NOTE IMPORTANT INFORMATION: all of the .flow scripts are written assuming the end user has connect their R studio project to lterwg-flux-gradient GitHub AND that they have created a data folder AND that within that data folder there are site folders named with the NEON sitecode
#load libaries
#TO DO: add namespace call to all library functions
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
#load in data compiling functions
source(file.path("R/SiteDF.R"))
source(file.path("R/SiteAttributes.R"))
source(file.path("R/cont9min.R"))
source(file.path("R/metCont30min.R"))
source(file.path("R/met1min.R"))
source(file.path("R/Compile1min.R"))
source(file.path("R/Compile30min.R"))
source(file.path("R/Compile9min.R"))
#set NEON sitecode
#this sitecode is used to grab existing files and create new ones MAKE SURE IT MATCHES
sitecode <- 'GUAN'
#grab h5 files to be passed to SiteAttributes and SiteDF
h5files <- list.files(path = file.path("data",sitecode), pattern = ".h5", full.names = T)
#grab attribute data
attr.df <- SiteAttributes(hd.files = h5files, sitecode = sitecode)
#grab gas concentration and met data at desired frequency (1min, 9min, 30min)
min9.list <- Site.DF(h5files = h5files, sitecode = sitecode, frequency = "9min")
min1.list <- Site.DF(h5files = h5files, sitecode = sitecode, frequency = "1min")
min30.list <- Site.DF(h5files = h5files, sitecode = sitecode, frequency = "30min")
#load in previously downloaded met (RH, WS2D)
load(paste0("data/", sitecode, "/", sitecode,"_NonEddyMetVars.Rdata"))
RH1min <- DATA$RH1min
WS2D2min <- DATA$WS2D2min
RH30min <- DATA$RH30min
WS2D30min <- DATA$WS2D30min
#match previously dowlonaded met to correct list
min1.list$RH <- RH1min
min30.list$RH <- RH30min
min30.list$WS2D <- WS2D30min
#save as R.data objects
save(min1.list, file = paste0("data/", sitecode, "/", sitecode,"_1min.Rdata"))
save(min9.list, file = paste0("data/", sitecode, "/", sitecode,"_9min.Rdata"))
save(min30.list, file = paste0("data/", sitecode, "/", sitecode,"_30min.Rdata"))
save(attr.df, file = paste0("data/", sitecode, "/", sitecode,"_attr.Rdata"))
save(WS2D2min, file = paste0("data/", sitecode, "/", sitecode,"_WS2D2min.Rdata"))
#zip R.data objects
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_1min.zip"), files = paste0("data/", sitecode, "/", sitecode, "_1min.Rdata"))
#upload to Google Drive
googledrive::drive_upload(media = paste0("data/", sitecode,"_1min.Rdata"), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/folders/1hzWPskSodKkHiempx7HUjLesXAxku2AB?usp=drive_link"))
