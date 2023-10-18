
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

#load in FG specific functions * UPDATE
#setwd("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/lterwg-flux-gradient")
source("R/SiteAttributes.R")
source("R/SiteDF.R")
source("R/hdf2df.R")
source("R/Flux_Gradient_MBR.R")
source("R/Flux_Gradient_AE.R")


#set desired site name and NEON code
# sitename <- 'HarvardForest'
# sitecode <- 'HARV'
# startdate <- "2017-11"
# enddate <- "2023-08"

sitename <- 'KonzaPrairie'
sitecode <- 'KONZ'
startdate <- "2017-11"
enddate <- "2023-08"

#set desired site name and NEON code
dir <- "data/raw_data"
#setwd(dir)
dir.create(paste(dir,sitename,sep="/"))
setwd(paste(dir,sitename, sep="/"))

# Download the data for the site
zipsByProduct(dpID="DP4.00200.001", site=sitecode ,startdate="2021-11", enddate="2023-08",package="basic", check.size=T) 
setwd(paste(dir,sitename, sep="/"))


