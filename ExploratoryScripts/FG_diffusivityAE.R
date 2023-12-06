library(dplyr)
source(file.path("R/MO_Length.R"))
source(file.path("R/eddydiffAE.R"))
#function arguments
#desired concentration
cont.desired <- "CH4"
#add code later that pulls zip files off of g drive
sitecode <- 'KONZ'
#load in interpolated 9 min data
load(file.path("data", sitecode, "KONZ_min9Diff.Rdata"))
load(file.path("data", sitecode, "KONZ_attr.Rdata"))
#call function to calculate eddy diffusivity using AE method
min9EddyDiff.list <- eddydiffAE(cont.desired = cont.desired, sitecode = sitecode, min9 = min9Diff.list, attr = attr.df)
ae.check <- min9EddyDiff.list$CH4
