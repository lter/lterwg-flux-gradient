# Load Libraries and Set Environment Vars ---------------------------------
#load libaries
##### Load Libraries #####
Sys.setenv(tz="UTC")
packages <- c("BiocManager", "neonUtilities", "readr", "rhdf5", "dplyr", "oce", "tidyr", "lubridate", "naniar", "ggplot2", "R.utils", "gtools")

# Identify packages that are not already loaded
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Load packages
invisible(lapply(packages, library, character.only = TRUE))

### Check project lterwg-flux-gradient
getwd() #if not in lterwg-flux-gradient, set Working Directory to project directory

#load in FG specific functions
source("R/SiteAttributes.R")
source("R/SiteDF_v2.R")
source("R/hdf2df.R")
source("R/contHF.R")
source("R/Flux_Gradient_MBR.R")
source("R/Flux_Gradient_AE.R")
source("R/time_format.R")

#set desired site name and NEON code
sitename <- 'KonzaPrairie'
sitecode <- 'KONZ'
startdate <- "2017-11"
enddate <- "2023-08"

# sitename <- 'Harvard_Forest'
# sitecode <- 'HARV'
# #set start and end date for obs
# startdate <- "2022-05"
# enddate <- "2022-07"

#create site folders and setwd
site.dir <- file.path("data","raw_data",sitename)
if(!exists(site.dir)){suppressWarnings(dir.create(site.dir))}else{print("raw data folder not there")}

# Call Fcns to Calculate CH4 fluxes ---------------------------------------
#grab h5 files to be passed to SiteAttributes and SiteDF
hd.files <-list.files(pattern="\\.h5$",path=site.dir,full.names = T)
#grab attribute data
attr.df <- SiteAttributes(hd.files, sitecode)
#save df as csv
write.csv(attr.df, paste0(site.dir, "_", startdate, "_", enddate, "_attr.csv"))

### grab co2, h20, ch4 level 1 data at all 30min resolution tower heights 
### along with level 4 co2, sensible heat, latent heat fluxes, uStar, uBar, air temp, z0
cont.df.tib <- Site.DF(hd.files, sitecode,frequency = "high")
cont.df = as.data.frame(cont.df.tib)

  date.time1 = matrix(unlist(strsplit(cont.df[,1], split = "T")),ncol=2,byrow=T)
  cont.df$datetime = as.POSIXct(paste(date.time1[,1], date.time1[,2], sep=" " ), tz='UTC') 
  
  #save df as csv
  write.csv(cont.df, paste0(site.dir, "_", startdate, "_", enddate, "_cont.csv"))

plot(cont.df$datetime,cont.df$mean_ch4.000_040_09m,col=2)

points(cont.df$datetime,cont.df$mean_ch4.000_030_09m,col=3)

points(cont.df$datetime,cont.df$mean_ch4.000_020_09m,col=2)

points(cont.df$datetime,cont.df$mean_ch4.000_010_09m,col=1)

#calculate flux gradient
#select lower tower height
z1_height <- attr.df$DistZaxsLvlMeasTow[1]
#select upper tower height
z2_height <- attr.df$DistZaxsLvlMeasTow[nrow(attr.df)]
#select tallest height for sonic anemometer
z_height <- attr.df$DistZaxsLvlMeasTow[nrow(attr.df)]
#calculate fluxes using Modified Bowan Ratio
mbr.df <- Flux_Gradient_MBR(cont.df, attr.df, z1_height, z2_height)
#save df as csv
write.csv(mbr.df, paste0(sitecode, "_", startdate, "_", enddate, "_mbr.csv"))
#calculate fluxes using aerodynamic profile and wind profile
ae.df <- Flux_Gradient_AE(cont.df, attr.df, z1_height, z2_height, z_height)
#save df as csv
write.csv(ae.df, paste0(sitecode, "_", startdate, "_", enddate, "_ae.csv"))