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

# BiocManager::install("rhdf5")

# Add local directory for downloaded data here:

#setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient')

#load in data compiling functions
source(file.path("functions/compile.neon.data.R"))
source(file.path("functions/compile.neon.site.attr.R"))
source(file.path("functions/grab.neon.gas.9min.6min.R"))
source(file.path("functions/grab.neon.met.flux.30min.R"))
source(file.path("functions/grab.neon.met.1min.R"))
source(file.path("functions/compile.neon.data.1min.R"))
source(file.path("functions/compile.neon.data.30min.R"))
source(file.path("functions/compile.neon.data.9min.6min.R"))

#point to the data folder on the shared Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)

# Add all sites here:
# site.list <- c("BONA","CPER","GUAN","HARV","JORN","KONZ","NIWO","TOOL")
# site.list <- c("ABBY","BARR","BART","BLAN","CLBJ","DCFS","DEJU","DELA")
# site.list <- c("DSNY","GRSM","HEAL","JERC","KONA","LAJA","LENO","MLBS")
# site.list <- c("MOAB","NOGP","OAES","ONAQ","ORNL","OSBS","PUUM","RMNP")
# site.list <- c("SCBI","SERC","SJER","SOAP","SRER","STEI","STER","TALL")
# site.list <- c("TEAK","TREE")
# ERROR UKFS
# site.list <- c("UNDE","WOOD")
# ERROR WREF
site.list <- c("YELL")

#set include.provisional = T to get full time series of data up to present, currently provisional covers (2022-07:2023-09) 
#grab relative humidity at 1 min resolution

#localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient'
#setwd(localdir)

for ( sitecode in site.list){
  print(sitecode)

#grab h5 files to be passed to SiteAttributes and SiteDF
h5files <- list.files(path = file.path("data", sitecode, "filesToStack00200"), pattern = ".h5", full.names = T)
#grab attribute data
attr.df <- compile.neon.site.attr(hd.files = h5files, sitecode = sitecode)
#grab gas concentration and met data at desired frequency (1min, 9min, 30min)
min9.list <- compile.neon.data(h5files = h5files, sitecode = sitecode, frequency = "9min")
min1.list <- compile.neon.data(h5files = h5files, sitecode = sitecode, frequency = "1min")
min30.list <- compile.neon.data(h5files = h5files, sitecode = sitecode, frequency = "30min")

#load in previously downloaded met (RH, WS2D)
load(file.path("data", sitecode, paste0(sitecode,"_NonEddyMetVars.Rdata")))
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
save(min1.list, file = file.path("data", sitecode, paste0(sitecode,"_1min.Rdata")))
save(min9.list, file = file.path("data", sitecode, paste0(sitecode,"_9min.Rdata")))
save(min30.list, file = file.path("data", sitecode, paste0(sitecode,"_30min.Rdata")))
save(attr.df, file = file.path("data", sitecode, paste0(sitecode,"_attr.Rdata")))
save(WS2D2min, file = file.path("data", sitecode, paste0(sitecode,"_WS2D2min.Rdata")))


#zip R.data objects
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_1min.zip")), files = file.path("data", sitecode, paste0(sitecode,"_1min.Rdata")))

zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_9min.zip")), files = file.path("data", sitecode, paste0(sitecode,"_9min.Rdata")))
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_30min.zip")), files = file.path("data", sitecode, paste0(sitecode,"_30min.Rdata")))
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_attr.zip")), files = file.path("data", sitecode, paste0(sitecode,"_attr.Rdata")))
zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_WS2D2min.zip")), files = file.path("data", sitecode, paste0(sitecode,"_WS2D2min.Rdata")))


#upload to Google Drive
#IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://lter.github.io/scicomp/tutorial_googledrive-pkg.html
# NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
site_folder <- data_folder$id[data_folder$name==sitecode]

googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_1min.zip"), overwrite = T, path = site_folder)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_9min.zip"), overwrite = T, path = site_folder)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_30min.zip"), overwrite = T, path = site_folder)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_attr.zip"), overwrite = T, path = site_folder)
googledrive::drive_upload(media = paste0("data/", sitecode, "/", sitecode,"_WS2D2min.zip"), overwrite = T, path = site_folder)
}
