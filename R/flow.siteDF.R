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
library(googledrive)
#load in data compiling functions
source(file.path("SiteDF.R"))
source(file.path("SiteAttributes.R"))
source(file.path("cont9min.R"))
source(file.path("metCont30min.R"))
source(file.path("met1min.R"))
source(file.path("Compile1min.R"))
source(file.path("Compile30min.R"))
#source(file.path("Compile9min.R"))
#set NEON sitecode
#this sitecode is used to grab existing files and create new ones MAKE SURE IT MATCHES
sitecode <- 'TOOL'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1mgqJps4HvjplsE7SXBvjAzm6n3BXC98I")
#grab h5 files to be passed to SiteAttributes and SiteDF
h5files <- list.files(path = paste0("data/", sitecode), pattern = ".h5", full.names = T)
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
PAR1min <- DATA$PAR1min
PAR30min <- DATA$PAR30min
#match previously dowlonaded met to correct list
min1.list$RH <- RH1min
min1.list$PAR <- PAR1min
min30.list$RH <- RH30min
min30.list$WS2D <- WS2D30min
min30.list$PAR <- PAR30min
#save as R.data objects
save(min1.list, file = paste0("data/", sitecode, "/", sitecode,"_1min.Rdata"))
save(min9.list, file = paste0("data/", sitecode, "/", sitecode,"_9min.Rdata"))
save(min30.list, file = paste0("data/", sitecode, "/", sitecode,"_30min.Rdata"))
save(attr.df, file = paste0("data/", sitecode, "/", sitecode,"_attr.Rdata"))
save(WS2D2min, file = paste0("data/", sitecode, "/", sitecode,"_WS2D2min.Rdata"))
#zip R.data objects
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_1min.zip"), files = paste0("data/", sitecode, "/", sitecode, "_1min.Rdata"))
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_9min.zip"), files = paste0("data/", sitecode, "/", sitecode, "_9min.Rdata"))
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_30min.zip"), files = paste0("data/", sitecode, "/", sitecode, "_30min.Rdata"))
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_attr.zip"), files = paste0("data/", sitecode, "/", sitecode, "_attr.Rdata"))
zip(zipfile = paste0("data/", sitecode, "/", sitecode,"_WS2D2min.zip"), files = paste0("data/", sitecode, "/", sitecode, "_WS2D2min.Rdata"))
#upload to Google Drive
#IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
#NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_1min.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_9min.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_30min.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_attr.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_WS2D2min.zip"), overwrite = T, path = drive_url)
