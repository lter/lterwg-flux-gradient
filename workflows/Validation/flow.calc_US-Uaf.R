library(tidyverse)

  sitecode <- 'US-Uaf'
  site <- 'US-Uaf'
  
  load( paste(localdir,'/US-Uaf/US-Uaf_aligned_conc_flux_9min.RData', sep=""))
  
  dirTmp <- paste(localdir,sitecode,sep="/")

  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
  
  data_folder <- googledrive::drive_ls(path = drive_url)
  
  setwd(DirRepo)
source(file.path(paste(DirRepo, "/workflows/flow.calc.flag.mbr.batch.R", sep="")))
  setwd(DirRepo)
  source(file.path(paste(DirRepo, "/workflows/flow.calc.flag.aero.batch.R", sep="")))
  setwd(DirRepo)
  source(file.path(paste(DirRepo, "/workflows/flow.calc.flag.windprof.batch.R", sep="")))
  
  
message('Next run the flow.validation.dataframe.batch.R')
