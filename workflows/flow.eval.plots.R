# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)
library(ggh4x)
library(googledrive)

# Load functions in this repo
source(file.path("functions/plot.all.sites.1to1.R"))
source(file.path("functions/plot.single.site.1to1.R"))
source(file.path("functions/plot.all.sites.bar.R"))
source(file.path("functions/light.response.curve.R"))
source(file.path("functions/plot.light.response.R"))
source(file.path("functions/temp.response.curve.R"))
source(file.path("functions/plot.temp.response.R"))
source(file.path("functions/plot.all.sites.diurnal.R"))
source(file.path("functions/calculate.all.sites.diurnal.avg.R"))
source(file.path("functions/all.sites.light.response.curve.R"))
source(file.path("functions/all.sites.temp.response.curve.R"))

#Pull data from gdrive
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
validation_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir())
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
  # if(grepl(pattern='.zip',focal_file)){
  #   utils::unzip(pathDnld,exdir=dirTmp)
  # }
  
}
#Load validation data frames 
#the R.data/zipfiles are labeled based on the method used to calculate the fluxes (i.e. AE, WP, MBR)
#problem loading in .Rdata objects currently being saved to 2 different directories?
#TO DO: better understand how to control where files are unzipped to when downloaded off of gdrive
fileIn <- fs::path(dirTmp, paste0("SITES_AE_val.Rdata"))
load(fileIn)
fileIn <- fs::path(dirTmp, paste0("SITES_WP_Stability.Rdata"))
load(fileIn)
fileIn <- fs::path(dirTmp, paste0("SITES_MBR_Stability.Rdata"))
load(fileIn)
#if data is already downloaded and saved
#load(file.path("data", "Validation", paste0("SITES_AE_val.Rdata")))
#load(file.path("data", "Validation", paste0("SITES_WP_val.Rdata")))
#load(file.path("data", "Validation", paste0("SITES_MBR.Rdata")))

#bind all site dataframes together
all.sites.ae <- bind_rows(SITES_AE_validation)
# all.sites.wp <- bind_rows(SITES_WP_validation)
# all.sites.mbr <- bind_rows(SITES_MBR)
#TO DO ADD MBR, NOT YET AVAILABLE
# all.sites.mbr <- bind_rows(SITES_MBR_validation)

#FG quality flags columns across methods (AE, WP): 
#IQR.flag (flagged outliers)
#spike.flag (flagged spikes)
#ustar flag (flagged ustar values below threshold)
#FG quality flag columns only for AE method:
#Stability_500 (stable, unstable, neutral conditions set using L threshold 500)
#Stability_100 (stable, unstable, neutral conditions set using L threshold 100)
#Other noteworthy columns across methods (AE, WP):
#month (numeric month)
#hour (hour from local time to site corrected for daylight savings)
#day_night (character either "day" or "night" set using PAR threshold 92)
#residual (numeric EC - FG)
#RMSE (numeric sqrt(mean((residual)^2)))

#TAKE NOTE: all of the data frames that are passed to the functions in this script are filtered within the argument for the given function, this is done intentionally to limit the number of data frames appearing in the R environment. If you would like to work with a filtered data frame outside of these functions you will need to pull the argument out and define the object on a separate line. Ex: site = all.sites.wp %>% filter(gas == "H2O" & site == "KONZ")


# PLOT LINEAR 1TO1 --------------------------------------------------------
#plot Linear 1:1 for calculated FG CO2 and H2O against EC CO2 and H2O
#axis ranges set using range of FG calculated flux
#METHOD AE
#ALL SITES
#CO2 all data
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "CO2"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#CO2 filter -50 to 50 
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "CO2") %>% filter(FG >= -50 & FG <= 50), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#CO2 removed outliers
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "CO2" & IQR.flag == "0"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#CO2 removed spikes
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "CO2" & spike.flag == "0"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#H2O all data
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "H2O"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#H2O removed outliers
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "H2O" & IQR.flag == "0"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#H2O removed spikes
plot.all.sites.1to1(all.sites = all.sites.ae %>% filter(gas == "H2O" & spike.flag == "0"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method")
#SINGLE SITE
#CO2
plot.single.site.1to1(site = all.sites.ae %>% filter(gas == "CO2" & site == "KONZ"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method at KONZ")
#H2O
plot.single.site.1to1(site = all.sites.ae %>% filter(gas == "H2O" & site == "KONZ"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Aerodynamic Method at KONZ")
#METHOD WP
#ALL SITES
#CO2 all data
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "CO2"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#CO2 neutral conditions using Stability_100
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "CO2" & Stability_100 == "neutral"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method under Neutral Conditions")
#CO2 filter -50 to 50 
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "CO2") %>% filter(FG >= -50 & FG <= 50), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#CO2 removed outliers
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "CO2" & IQR.flag == "0"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#CO2 removed spikes
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "CO2" & spike.flag == "0"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#H2O all data
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "H2O"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#H2O removed outliers
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "H2O" & IQR.flag == "0"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#H2O removed spikes
plot.all.sites.1to1(all.sites = all.sites.wp %>% filter(gas == "H2O" & spike.flag == "0"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Wind Profile Method")
#SINGLE SITE
#CO2
plot.single.site.1to1(site = all.sites.wp %>% filter(gas == "CO2" & site == "KONZ"), x.flux = "FC_turb_interp", y.flux = "FG", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Wind Profile Method at KONZ")
#H2O
plot.single.site.1to1(site = all.sites.wp %>% filter(gas == "H2O" & site == "KONZ"), x.flux = "FH2O_interp", y.flux = "FG", x.lab = expression(paste("Estimated H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), y.lab = expression(paste("FG H"[2], "O Flux (mmol H"[2], "O m"^-2," s"^-1,")")), plot.title = "Wind Profile Method at KONZ")
#METHOD MBR
#ALL SITES
#CO2
plot.all.sites.1to1(all.sites = all.sites.mbr, x.flux = "FC_turb_interp_H2O", y.flux = "FCO2_MBR_H2Otrace", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Modified Bowen Ratio Method")
#CO2 filter -50 to 50
plot.all.sites.1to1(all.sites = all.sites.mbr %>% filter(FCO2_MBR_H2Otrace >= -50 & FCO2_MBR_H2Otrace <= 50), x.flux = "FC_turb_interp_H2O", y.flux = "FCO2_MBR_H2Otrace", x.lab = expression(paste("EC CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), y.lab = expression(paste("FG CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")), plot.title = "Modified Bowen Ratio Method")

# PLOT BAR PLOTS ----------------------------------------------------------
#NOTE THE FUNCTION plot.all.sites.bar WORKS WITH A DATAFRAME THAT HAS MULTIPLE SITES AND A DATAFRAME THAT HAS ONE SITE
#look at bar plot of atmospheric conditions for each site
#THIS STABILITY COLUMN ONLY APPLIES TO AE METHOD BECASUE ONLY THAT METHOD USES THE OBUKHOV LENGTH (L)
#ALL SITES
#L threshold of 100
plot.all.sites.bar(all.sites = all.sites.ae, desired.var = "Stability_100", x.lab = "Atmospheric Condition", plot.title = "100m threshold")
#L threshold of 500
plot.all.sites.bar(all.sites = all.sites.ae, desired.var = "Stability_500", x.lab = "Atmospheric Condition", plot.title = "500m threshold")
#SINGLE SITE
#L threshold of 100
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(site == "KONZ"), desired.var = "Stability_100", x.lab = "Atmospheric Condition", plot.title = "Aerodynamic Method")
#L threshold of 500
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(site == "KONZ"), desired.var = "Stability_500", x.lab = "Atmospheric Condition", plot.title = "Aerodynamic Method")
#look at bar plot of outliers/spikes for each site
#METHOD AE
#ALL SITES
#CO2
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="CO2"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Aerodynamic Method")
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="CO2"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Aerodynamic Method")
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas == "CO2"), desired.var = "ustar.flag", x.lab = "Ustar Flag", plot.title = "Aerodynamic Method")
#H2O
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="H2O"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Aerodynamic Method")
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="H2O"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Aerodynamic Method")
#SINGLE SITE
#CO2
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="CO2" & site == "KONZ"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Aerodynamic Method")
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="CO2" & site == "KONZ"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Aerodynamic Method")
#H2O
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="H2O" & site == "KONZ"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Aerodynamic Method")
plot.all.sites.bar(all.sites = all.sites.ae %>% filter(gas=="H2O" & site == "KONZ"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Aerodynamic Method")
#METHOD WP
#ALL SITES
#CO2
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="CO2"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Wind Profile Method")
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="CO2"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Wind Profile Method")
#H2O
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="H2O"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Wind Profile Method")
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="H2O"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Wind Profile Method")
#SINGLE SITE
#CO2
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="CO2" & site == "KONZ"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Wind Profile Method")
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="CO2" & site == "KONZ"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Wind Profile Method")
#H2O
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="H2O" & site == "KONZ"), desired.var = "IQR.flag", x.lab = "IQR Flag", plot.title = "Wind Profile Method")
plot.all.sites.bar(all.sites = all.sites.wp %>% filter(gas=="H2O" & site == "KONZ"), desired.var = "spike.flag", x.lab = "Spike Flag", plot.title = "Wind Profile Method")
# LIGHT RESPONSE CURVE ----------------------------------------------------
#plot light response curves daytime CO2 vs daytime PAR for FG and EC calculated fluxes
#calculate light response curves, filter to only daytime flux and PAR data at single site
#using rectangular hyperbolic model
#ALL SITES
#NOTE WE WANT TO CALCULATE EC USING BOTH METHODS BECAUSE THE RESPECTIVE METHODS DO NOT SHARE SAME TIME SERIES
#METHOD AE
all.sites.LRC.FG.AE <- all.sites.light.response.curve(all.sites = all.sites.ae, flux.name = "FG", alpha = 0.001, beta = 6, gama = 0.3, method = "AE")
param.df.LRC.FG.AE <- all.sites.LRC.FG.AE$param.df
model.object.LRC.FG.AE <- all.sites.LRC.FG.AE$model.objects
all.sites.LRC.EC.AE <- all.sites.light.response.curve(all.sites = all.sites.ae, flux.name = "FC_nee_interp", alpha = 0.001, beta = 6, gama = 0.3, method = "AE")
param.df.LRC.EC.AE <- all.sites.LRC.EC.AE$param.df
model.object.LRC.EC.AE <- all.sites.LRC.EC.AE$model.objects
#METHOD WP
all.sites.LRC.FG.WP <- all.sites.light.response.curve(all.sites = all.sites.wp, flux.name = "FG", alpha = 0.001, beta = 6, gama = 0.3, method = "WP")
param.df.LRC.FG.WP <- all.sites.LRC.FG.WP$param.df
model.object.LRC.FG.WP <- all.sites.LRC.FG.WP$model.objects
all.sites.LRC.EC.WP <- all.sites.light.response.curve(all.sites = all.sites.wp, flux.name = "FC_nee_interp", alpha = 0.001, beta = 6, gama = 0.3, method = "WP")
param.df.LRC.EC.WP <- all.sites.LRC.EC.WP$param.df
model.object.LRC.EC.WP <- all.sites.LRC.EC.WP$model.objects
#check out parameters across FG and EC
print(param.df.LRC.FG.AE)
print(param.df.LRC.EC.AE)
print(param.df.LRC.FG.WP)
print(param.df.LRC.EC.WP)
#IF YOU WOULD LIKE TO INVESTIGATE VARING THE INITIAL PARAMETER VALUES FOR LIGHT RESPONSE CURVE AT A SPECIFIC SITE FOR A GIVEN METHOD
#SINGLE SITE
#METHOD AE
model.LRC.FG.AE <- light.response.curve(site = all.sites.ae %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FG")
model.LRC.EC.AE <- light.response.curve(site = all.sites.ae %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FC_nee_interp")
#METHOD WP
model.LRC.FG.WP <- light.response.curve(site = all.sites.wp %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FG")
model.LRC.EC.WP <- light.response.curve(site = all.sites.wp %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), alpha = 0.001, beta = 6, gama = 0.3, flux.name = "FC_nee_interp")
#plot light response curve using estimated parameters
#METHOD AE
plot.light.response(model = model.LRC.FG.AE, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FG", plot.title = "FG AE at BONA")
plot.light.response(model = model.LRC.EC.AE, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FC_nee_interp", plot.title = "EC AE at BONA")
#METHOD WP
plot.light.response(model = model.LRC.FG.WP, site = all.sites.wp %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FG", plot.title = "FG WP at BONA")
plot.light.response(model = model.LRC.EC.WP, site = all.sites.wp %>% filter(gas == "CO2" & day_night == "day" & site == "BONA"), flux.name = "FC_nee_interp", plot.title = "EC WP at BONA")

# TEMPERATURE RESPONSE CURVE ----------------------------------------------
#plot temperature response curves for nighttime CO2 vs nighttime air temperature for FG and EC calculated fluxes
#calculate temperature response curves, filter to only nighttime flux and air temperature data
#use top of tower air temperature: column name will vary by site depending on number of levels on the tower
#using exponential model
#ALL SITES
#NOTE WE WANT TO CALCULATE EC USING BOTH METHODS BECAUSE THE RESPECTIVE METHODS DO NOT SHARE SAME TIME SERIES
#NOTE IGNORE NA WARNING FROM all.sites.temp.response.curve
#METHOD AE
all.sites.TRC.FG.AE <- all.sites.temp.response.curve(all.sites = all.sites.ae, flux.name = "FG", rho = 1, psi = 0.1, method = "AE")
param.df.TRC.FG.AE <- all.sites.TRC.FG.AE$param.df
model.object.TRC.FG.AE <- all.sites.TRC.FG.AE$model.objects
all.sites.TRC.EC.AE <- all.sites.temp.response.curve(all.sites = all.sites.ae, flux.name = "FC_nee_interp", rho = 1, psi = 0.1, method = "AE")
param.df.TRC.EC.AE <- all.sites.TRC.EC.AE$param.df
model.object.TRC.EC.AE <- all.sites.TRC.EC.AE$model.objects
#METHOD WP
all.sites.TRC.FG.WP <- all.sites.temp.response.curve(all.sites = all.sites.wp, flux.name = "FG", rho = 1, psi = 0.1, method = "WP")
param.df.TRC.FG.WP <- all.sites.TRC.FG.WP$param.df
model.object.TRC.FG.WP <- all.sites.TRC.FG.WP$model.objects
all.sites.TRC.EC.WP <- all.sites.temp.response.curve(all.sites = all.sites.wp, flux.name = "FC_nee_interp", rho = 1, psi = 0.1, method = "WP")
param.df.TRC.EC.WP <- all.sites.TRC.EC.WP$param.df
model.object.TRC.EC.WP <- all.sites.TRC.EC.WP$model.objects
#check out parameters across FG and EC
print(param.df.TRC.FG.AE)
print(param.df.TRC.EC.AE)
print(param.df.TRC.FG.WP)
print(param.df.TRC.EC.WP)

#IF YOU WOULD LIKE TO INVESTIGATE VARING THE INITIAL PARAMETER VALUES FOR TEMP RESPONSE CURVE AT A SPECIFIC SITE FOR A GIVEN METHOD
#SINGLE SITE
#METHOD AE
model.TRC.FG.AE <- temp.response.curve(site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FG")
model.TRC.EC.AE <- temp.response.curve(site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FC_nee_interp")
#METHOD WP
model.TRC.FG.WP <- temp.response.curve(site = all.sites.wp %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FG")
model.TRC.EC.WP <- temp.response.curve(site = all.sites.wp %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", rho = 1, psi = 0.1, flux.name = "FC_nee_interp")
#plot temperature response curve using estimated parameters
#METHOD AE
plot.temp.response(model = model.TRC.FG.AE, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FG", plot.title = "FG AE at BONA")
plot.temp.response(model = model.TRC.EC.AE, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FC_nee_interp", plot.title = "EC AE at BONA")
#METHOD WP
plot.temp.response(model = model.TRC.FG.WP, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FG", plot.title = "FG WP at BONA")
plot.temp.response(model = model.TRC.EC.WP, site = all.sites.ae %>% filter(gas == "CO2" & day_night == "night" & site == "BONA"), TA.name = "Tair5", flux.name = "FC_nee_interp", plot.title = "EC WP at BONA")

# PLOT DIURNAL CYCLE ------------------------------------------------------
#plot diurnal averages for all sites
#calculate diurnal averages across sites for EC using turbulent flux
#NOTE TOOL DOES NOT HAVE ENOUGH DATA FOR 24 HR DIURNAL CYCLE, MISSING HOURS 05 AND 00
#OPTION TO FILTER DATA BEFORE TAKING HOURLY AVERAGE BY FILTERING COLUMN IQR.flag == "0" or spike.flag == "0"
#ALL SITES
#METHOD AE
all.sites.diurnal.ae <- calculate.all.sites.diurnal.avg(all.sites = all.sites.ae)
#plot dirnual cycle for all sites
plot.all.sites.diurnal(all.sites = all.sites.diurnal.ae, plot.title = "Aerodynamic Method")
#METHOD WP
all.sites.diurnal.wp <- calculate.all.sites.diurnal.avg(all.sites = all.sites.wp)
#plot dirnual cycle for all sites
plot.all.sites.diurnal(all.sites = all.sites.diurnal.wp, plot.title = "Wind Profile Method")
#SINGLE SITE
#METHOD AE
#plot dirnual cycle for single site
plot.all.sites.diurnal(all.sites = all.sites.diurnal.ae %>% filter(site == "KONZ"), plot.title = "Aerodynamic Method")
#METHOD WP
#plot dirnual cycle for single site
plot.all.sites.diurnal(all.sites = all.sites.diurnal.wp %>% filter(site == "KONZ"), plot.title = "Wind Profile Method")
