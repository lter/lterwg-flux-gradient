# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
#the R.data/zipfiles are labeled based on the method used to calculate the fluxes (i.e. AE, WP, MBR)
method <- 'AE'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)
library(ggh4x)

# Load functions in this repo
source(file.path("R/plot.all.sites.1to1.R"))
source(file.path("R/plot.all.sites.bar.R"))
source(file.path("R/light.response.curve.R"))
source(file.path("R/plot.light.response.R"))
source(file.path("R/temp.response.curve.R"))
source(file.path("R/plot.temp.response.R"))
source(file.path("R/plot.all.sites.diurnal.R"))
source(file.path("R/calculate.all.sites.diurnal.avg.R"))

#Pull data from gdrive
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
validation_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir(),method)
dir.create(dirTmp)
for(focal_file in validation_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(validation_folder, name == focal_file)
  
  # Download that file
  pathDnld <- fs::path(dirTmp,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip
  if(grepl(pattern='.zip',focal_file)){
    utils::unzip(pathDnld,exdir=dirTmp)
  }
  
}
#Load validation data frames 
#problem loading in .Rdata objects currently being saved to 2 different directories?
#TO DO: better understand how to control where files are unzipped to when downloaded off of gdrive
fileIn <- fs::path(dirTmp, paste0("data/Validation/SITES_", method, "_val.Rdata"))
load(fileIn)
#if data is already downloaded and saved
#load(file.path("data", "Validation", paste0("SITES_", method, "_val.Rdata")))

#bind all site dataframes together
if(method == "AE"){
  all.sites <- bind_rows(SITES_AE_validation)
}
if(method == "WP"){
  all.sites <- bind_rows(SITES_WP_validation)
}

#FG quality flags are columns IQR.flag (flagged outliers), spike.flag (flagged spikes) for WP method and IQR.flag, spike.flag, Stability_500 (stable, unstable, neutral conditions set using L threshold 500), and Stability_100 (stable, unstable, neutral conditions set using L threshold 100)for AE method

#plot Linear 1:1 for calculated FG CO2 and H2O against EC CO2 and H2O
#axis ranges set using range of FG calculated flux
#CO2 all data
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "CO2"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#CO2 removed outliers
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "CO2" & IQR.flag == "0"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#CO2 removed spikes
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "CO2" & spike.flag == "0"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#H2O all data
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "H2O"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#H2O removed outliers
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "H2O" & IQR.flag == "0"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#H2O removed spikes
plot.all.sites.1to1(all.sites = all.sites %>% filter(gas == "H2O" & spike.flag == "0"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#look at bar plot of atmospheric conditions for each site
#THIS STABILITY COLUMN ONLY APPLIES TO AE METHOD BECASUE ONLY THAT METHOD USES THE OBUKHOV LENGTH (L)
if(method == "AE"){
  #L threshold of 100
  plot.all.sites.bar(all.sites = all.sites, desired.var = "Stability_100", x.lab = "Atmospheric Condition")
  #L threshold of 500
  plot.all.sites.bar(all.sites = all.sites, desired.var = "Stability_500", x.lab = "Atmospheric Condition")
}
#look at bar plot of outliers/spikes for each site
#CO2
plot.all.sites.bar(all.sites = all.sites %>% filter(gas=="CO2"), desired.var = "IQR.flag", x.lab = "IQR Flag")
plot.all.sites.bar(all.sites = all.sites %>% filter(gas=="CO2"), desired.var = "spike.flag", x.lab = "Spike Flag")
#H2O
plot.all.sites.bar(all.sites = all.sites %>% filter(gas=="H2O"), desired.var = "IQR.flag", x.lab = "IQR Flag")
plot.all.sites.bar(all.sites = all.sites %>% filter(gas=="H2O"), desired.var = "spike.flag", x.lab = "Spike Flag")
#plot light response curves daytime CO2 vs daytime PAR for FG and EC calculated fluxes
#calculate light response curves, filter to only daytime flux and PAR data at single site
model.LRC.FG <- light.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FG")
model.LRC.EC <- light.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FC_nee_interp")
#plot light response curve using estimated parameters
plot.light.response(model = model.LRC.FG, site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FG")
plot.light.response(model = model.LRC.EC, site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FC_nee_interp")
#plot temperature response curves for nighttime CO2 vs nighttime air temperature for FG and EC calculated fluxes
#calculate temperature response curves, filter to only nighttime flux and air temperature data
#use top of tower air temperature: column name will vary by site depending on number of levels on the tower
model.TRC.FG <- temp.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FG")
model.TRC.EC <- temp.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FC_nee_interp")
plot.temp.response(model = model.TRC.FG, site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FG")
plot.temp.response(model = model.TRC.EC, site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FC_nee_interp")
#plot diurnal averages for all sites
#calculate diurnal averages across sites
#NOTE TOOL DOES NOT HAVE ENOUGH DATA FOR 24 HR DIURNAL CYCLE, MISSING HOURS 05 AND 00
#OPTION TO FILTER DATA BEFORE TAKING HOURLY AVERAGE BY FILTERING COLUMN IQR.flag == "0" or spike.flag == "0"
all.sites.diurnal <- calculate.all.sites.diurnal.avg(all.sites = all.sites)
#plot dirnual cycle for all sites
plot.all.sites.diurnal(all.sites = all.sites.diurnal, flux.name = "mean_FG_flux", flux.ymin.name = "FG_ymin", flux.ymax.name = "FG_ymax")
