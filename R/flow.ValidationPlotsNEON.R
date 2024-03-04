# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
#add userinfo for saving and uploading the file to G drive
user <- "AH"
#the R.data/zipfiles are labeled based on the method used to calculate the fluxes (i.e. AE, WP, MBR)
method <- 'AE'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)

# Load functions in this repo
source(file.path("R/plot1to1.R"))
source(file.path("R/plot.histogram.R"))
source(file.path("R/LightResponseCurve.R"))
source(file.path("R/plot.light.response.R"))
source(file.path("R/temp.response.curve.R"))
source(file.path("R/plot.temp.response.R"))

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
load(file.path("data", "Validation", paste0("SITES_", method, "_val.Rdata")))

#bind all site dataframes together
all.sites <- bind_rows(SITES_AE_validation)

#FG quality flags are columns IQR.flag (flagged outliers), spike.flag (flagged spikes) for WP method and IQR.flag, spike.flag, Stability_500 (stable, unstable, neutral conditions set using L threshold 500), and Stability_100 (stable, unstable, neutral conditions set using L threshold 100)for AE method

#plot Linear 1:1 for calculated FG CO2 and H2O against EC CO2 and H2O
#CO2 all data
plot.1to1(all.sites = all.sites %>% filter(gas == "CO2"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#CO2 removed outliers
plot.1to1(all.sites = all.sites %>% filter(gas == "CO2" & IQR.flag == "0"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#CO2 removed spikes
plot.1to1(all.sites = all.sites %>% filter(gas == "CO2" & spike.flag == "0"), desired.var = "FC_nee_interp", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))
#H2O all data
plot.1to1(all.sites = all.sites %>% filter(gas == "H2O"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#H2O removed outliers
plot.1to1(all.sites = all.sites %>% filter(gas == "H2O" & IQR.flag == "0"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#H2O removed spikes
plot.1to1(all.sites = all.sites %>% filter(gas == "H2O" & spike.flag == "0"), desired.var = "FH2O_interp", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")))
#look at histogram of atmospheric conditions for each site
#L threshold of 100
plot.histogram(all.sites = all.sites, desired.var = "Stability_100", x.lab = "Atmospheric Condition")
#L threshold of 500
plot.histogram(all.sites = all.sites, desired.var = "Stability_500", x.lab = "Atmospheric Condition")
#look at histogram of outliers/spikes for each site
#CO2
plot.histogram(all.sites = all.sites %>% filter(gas=="CO2"), desired.var = "IQR.flag", x.lab = "IQR Flag")
plot.histogram(all.sites = all.sites %>% filter(gas=="CO2"), desired.var = "spike.flag", x.lab = "Spike Flag")
#H2O
plot.histogram(all.sites = all.sites %>% filter(gas=="H2O"), desired.var = "IQR.flag", x.lab = "IQR Flag")
plot.histogram(all.sites = all.sites %>% filter(gas=="H2O"), desired.var = "spike.flag", x.lab = "Spike Flag")
#plot light response curves daytime CO2 vs daytime PAR
#calculate light response curves
#filter to only daytime flux and PAR data
#site.day <- all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA")
model.LRC <- light.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3)
#plot light response curve
plot.light.response(model = model.LRC, site = all.sites %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"))
#plot temperature response curves for nightime CO2 vs nightime air temperature
#calculate temperature response curves
#filter to only nightime flux and air temperature data
model.TRC <- temp.response.curve(site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1)
plot.temp.response(model = model.TRC, site = all.sites %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5")



