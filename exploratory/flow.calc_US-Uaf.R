library(tidyverse)

load( 'data/US-Uaf/US-Uaf_aligned_conc_flux_9min.RData')

  sitecode <- 'US-Uaf'
  site <- 'US-Uaf'

  localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient'
  setwd(localdir)
  dirTmp <- paste(localdir,"data", sitecode,sep="/")

  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
  data_folder <- googledrive::drive_ls(path = drive_url)
  
source(file.path("exploratory/flow.calc.flag.mbr.batch.R"))
source(file.path("exploratory/flow.calc.flag.aero.batch.R"))
source(file.path("exploratory/flow.calc.flag.windprof.batch.R"))
  
message('Next run the flow.validation.dataframe.batch.R')