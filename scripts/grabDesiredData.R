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
source(file.path("R/SiteDF.R"))
source(file.path("R/SiteAttributes.R"))
source(file.path("R/cont9m.R"))
source(file.path("R/metCont30m.R"))
source(file.path("R/met1m.R"))
#set up dir to store data
Main.Directory <- c(file.path("data/Konza"))
#set desired site name and NEON code
sitename <- 'Konza Praire'
sitecode <- 'KONZ'
#set start and end date for obs
startdate <- "2021-08"
enddate <- "2023-09"
# Call Fcns to Calculate CH4 fluxes ---------------------------------------
#grab h5 files to be passed to SiteAttributes and SiteDF
folders <- list.files(path = file.path("data","Konza", "NEON_eddy-flux"), pattern = "KONZ.DP4.00200.001", full.names = T)

folders <- folders[8:33]
# hd.files <-list.files(path = file.path(folders[1]), pattern="\\.h5$", full.names = T)

#select for 2021 july forward
#grab attribute data
#attr.df <- SiteAttributes(hd.files, sitecode)
#save df as csv
# write.csv(attr.df, paste0(sitecode, "_", startdate, "_", enddate, "_attr.csv"))
#grab co2, h20, ch4 level 1 data at all 30min resolution tower heights along with level 4 co2, sensible heat, latent heat fluxes, uStar, uBar, air temp, z0
m1.list <- Site.DF(folder = folders, sitecode = sitecode, startdate = startdate, enddate = enddate)

# WS2D <- m30.list$WS2D
# WS2D <- WS2D %>%
#   mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
#   select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
# 
# m30.list$WS2D <- WS2D

# m30.list <- met.cont.30m(hd.file = hd.files, sitecode = sitecode, startdate = startdate, enddate = enddate)
# m9.list <- cont.9m(hd.file = hd.files, sitecode = sitecode)
# m1.list <- met.1m(hd.file = hd.files, sitecode = sitecode, startdate = startdate, enddate = enddate)

#grab 2D horizontal wind speed
# WS2D <- loadByProduct("DP1.00001.001", site="KONZ", 
#                       timeIndex=2, package="basic", 
#                       startdate=startdate, enddate=enddate,
#                       check.size=F)
# WS2D_2min <- WS2D$twoDWSD_2min %>%
#   select(verticalPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)

# save(attr.df, m30.list, m9.list, m1.list, WS2D_2min, file = paste0("data/", sitecode,"_vars.Rdata"))
# load(paste0("data/", sitecode,"_vars.Rdata"))
# save(attr.df, m30.list, m9.list, WS2D_2min, m1.list, file = paste0("data/", sitecode,"_vars.Rdata"))
save(m1.list, file = paste0("data/", sitecode,"_1m.Rdata"))


