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
#Assuming you have connected to the lterwg-flux-gradient github to your R studio
#grab h5 files to be passed to SiteAttributes and SiteDF
folders <- list.files(path = file.path("data",sitecode, "NEON_eddy-flux"), pattern = "KONZ.DP4.00200.001", full.names = T)
#index to trim to h5 files that have CH4 gas concentrations (august 2021 - present)
#the Site.DF functions do not work for time periods that do not include CH4 data -> future fix
folders <- folders[8:10]
#grab one h5 file to pass to SiteAttributes since all attributes are consistent across months
hd.files <-list.files(path = file.path(folders[1]), pattern="\\.h5$", full.names = T)
#grab attribute data
attr.df <- SiteAttributes(hd.files, sitecode)
#grab gas concentration and met data at desired frequency (1min, 9min, 30min)
min9.list <- Site.DF(folder = folders, sitecode = sitecode, frequency = "9min")
min1.list <- Site.DF(folder = folders, sitecode = sitecode, frequency = "1min")
min30.list <- Site.DF(folder = folders, sitecode = sitecode, frequency = "30min")
#load in previously downloaded met (RH, WS2D)
load(paste0("data/", sitecode,"_NonEddyMetVars.Rdata"))
RH1min <- DATA$RH1min
WS2D2min <- DATA$WS2D2min
RH30min <- DATA$RH30min
WS2D30min <- DATA$WS2D30min
#match previously dowlonaded met to correct list
min1.list$RH <- RH1min
in30.list$RH <- RH30min
min30.list$WS2D <- WS2D30min
#save as R.data objects
save(min1.list, file = paste0("data/", sitecode,"_1min.Rdata"))
save(min9.list, file = paste0("data/", sitecode,"_9min.Rdata"))
save(min30.list, file = paste0("data/", sitecode,"_30min.Rdata"))
save(attr.df, file = paste0("data/", sitecode,"_attr.Rdata"))
save(WS2D2min, file = paste0("data/", sitecode,"_WS2D2min.Rdata"))
#zip R.data objects
rdat_prefix <- file.path("data", paste0(sitecode, "_1min"))
zip(zipfile = "data/testZip.zip", files = "data/testZip.csv")
#upload to Google Drive
googledrive::drive_upload(media = paste0("data/", sitecode,"_1min.Rdata"), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/folders/1hzWPskSodKkHiempx7HUjLesXAxku2AB?usp=drive_link"))
