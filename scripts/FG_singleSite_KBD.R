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
source("R/SiteAttributes.R")
source("R/SiteDF.R")
source("R/hdf2df.R")
source("R/Flux_Gradient_MBR.R")
source("R/Flux_Gradient_AE.R")
source("R/time_format.R")
source("R/cont.HF.R")
#set up dir to store data
Main.Directory <- file.path("Konza")
#set desired site name and NEON code
sitename <- 'Konza_Praire'
sitecode <- 'KONZ'
#set start and end date for obs
startdate <- "2021-01"
enddate <- "2023-09"
#create site folders and setwd
#site.dir <- paste(Main.Directory,"/",sitename, sep="")
#if(!exists(site.dir)){dir.create(site.dir)}
#setwd(paste0(site.dir, "/filesToStack00200/"))

# Call Fcns to Calculate CH4 fluxes ---------------------------------------
#grab h5 files to be passed to SiteAttributes and SiteDF
hd.files <-list.files(path = Main.Directory, pattern="\\.h5$", full.names = TRUE)
#grab attribute data
attr.df <- SiteAttributes(hd.files, sitecode)
#save df as csv
# write.csv(attr.df, paste0(sitecode, "_", startdate, "_", enddate, "_attr.csv"))
#grab co2, h20, ch4 level 1 data at all 30min resolution tower heights along with level 4 co2, sensible heat, latent heat fluxes, uStar, uBar, air temp, z0
cont.df <- Site.DF(hd.files, sitecode, frequency = "high")
#filter for good data
h2o.qfqm.df <- cont.df %>% filter(h2o_qfqm.000_040_30m == "0") %>% filter(h2o_qfqm.000_060_30m == "0") %>% filter(F_co2_qfqm == "0")

co2.qfqm.df <- cont.df %>% filter(co2_qfqm.000_040_30m == "0") %>% filter(co2_qfqm.000_060_30m == "0") %>% filter(F_LE_qfqm == "0")
#save df as csv
# write.csv(cont.df, paste0(sitecode, "_", startdate, "_", enddate, "_cont.csv"))
#calculate flux gradient
#select lower twoer height
z1_height <- attr.df$DistZaxsLvlMeasTow[4]
#select upper tower height
z2_height <- attr.df$DistZaxsLvlMeasTow[6]
#select tallest height for sonic anemometer
z_height <- attr.df$DistZaxsLvlMeasTow[6]
#calculate fluxes using Modified Bowan Ratio
mbr.df <- Flux_Gradient_MBR(cont.df = co2.qfqm.df, attr.df, z1_height, z2_height)

#save df as csv
# write.csv(mbr.df, paste0(sitecode, "_", startdate, "_", enddate, "_mbr.csv"))
#calculate fluxes using aerodynamic profile and wind profile
ae.df <- Flux_Gradient_AE(cont.df = co2.qfqm.df, attr.df, z1_height, z2_height, z_height)
#save df as csv
write.csv(ae.df, paste0(sitecode, "_", startdate, "_", enddate, "_ae.csv"))
