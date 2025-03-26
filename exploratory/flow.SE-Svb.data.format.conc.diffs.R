# Formats external site (validation data) into the same format as NEON paired concentration difference data.
#
# Merges together flux, met, and profile concentration data for site SE-Svb . Aligns the profile 
# concentration data (CH4, CO2, and H2O) among adjacent tower levels (and also the bottom-top 
# levels) and computes the difference in mean concentration. Aligns non-concentration data 
# with the mid-point of the paired-level concentration differences. 
# Methods for alignment differ according to the data. 30-min flux data are interpolated. 
# Data at smaller (e.g. 1-min) averaging intervals are aggregated to the combined window of 
# each paired concentration difference. 
#
# Also derives kinematic water flux (LE -> w'q'), heat flux (w'T'), aerodynamic 
# canopy height, displacement height, that are needed for the various methods. 
#
# Saves output as SITE_aligned_conc_flux_9min.RData, where SITE is the site code. 
# Zips and uploads to Google Drive.
# For loading data to/from google drive
#email <- 'alexisrose0525@gmail.com'
#email <- 'jaclyn_matthes@g.harvard.edu'
email <- 'sam.jurado@yale.edu'
site <- 'SE-Svb'
# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: fs, googledrive
options(digits=12)
library(foreach)
library(doParallel)
library(dplyr)
library(lubridate)
# Load functions in this repo
source(file.path("functions/interp.flux.R"))
source(file.path("functions/aggregate_averages.R"))
# Final note: This script takes approx 45 min to run per site. 
# -------------------------------------------------------
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url_extSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1jrOJIu5WfdzmlbL9vMkUNfzBpRC-W0Wd")
data_folder <- googledrive::drive_ls(path = drive_url_extSiteData)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)
focal_files <- site_folder$name # Default - downloads all files to the temp folder
focal_files <- c("CH4_SE_SVB_FLUX+PROFILE_2019.csv","sesvb_attr.csv")
for(focal_file in focal_files){
  
  # Find the file identifier for that file
  file_id <- subset(site_folder, name == focal_file)
  
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
fileIn <- fs::path(dirTmp,"CH4_SE_SVB_FLUX+PROFILE_2019.csv")
dataSESVB <- read.table(fileIn,header=TRUE,sep=",",skip=0)
fileIn <- fs::path(dirTmp,"sesvb_attr.csv")
attr.df1 <- read.csv(fileIn)
# Transform the attr.df data frame into same format as NEON sites
attr.df.transpose <- as.data.frame(t(attr.df1))
nameCol <- attr.df.transpose[1,]
dtype <- attr.df.transpose[2,]
attr.df <- attr.df.transpose[3:nrow(attr.df.transpose),]
names(attr.df) <- nameCol
attr.df[attr.df == "na"] <- NA
for (idxCol in 1:ncol(attr.df)){
  x <- switch(dtype[[idxCol]],
              num=as.numeric(attr.df[,idxCol]),
              chr=attr.df[,idxCol],
              int=as.integer(attr.df[,idxCol]))
  attr.df[[idxCol]] <- x
}
# Parse tower heights
tower.heights <- data.frame(TowerHeight=as.numeric(attr.df$DistZaxsLvlMeasTow),
                            TowerPosition=as.numeric(attr.df$TowerPosition))
if(any(diff(tower.heights$TowerPosition) <= 0)){
  stop("Tower Position must be increasing in the attr.df file")
}
#Datetime and UTC
#Assuming the time is representative of the end of the averaging period
dataSESVB$timeEnd <- as.POSIXct(dataSESVB$timestamp, format="%d-%b-%Y %H:%M:%S", tz="CET")
dataSESVB$timeEnd <- with_tz(dataSESVB$timeEnd, tzone = "UTC")
dataSESVB$timeBgn <- dataSESVB$timeEnd - minutes(30)
#Variable Reassignment
###NOTE: We will only consider levels up to 85m (13th) level, as this is where
#' the highest sonic anemometer is, and is the top ch4 conc level.
########PLACE HOLDER FOR WET TO DRY MOL CONVERSION IF IT IS NEEDED##############
###########################NEED TO ASK PI OF SITE###############################
###Relative Humidity is necessary
dataSESVB$esat_Pa = 611.2*exp(17.67*(dataSESVB$Ta_degC_150m)/(dataSESVB$Ta_degC_150m-29.65)) #[Pa] saturated water vapor pressure
dataSESVB$e_Pa = dataSESVB$h2o_mmolmol_150 #[g water /m^3 of air]
dataSESVB$RH = (dataSESVB$e_Pa/dataSESVB$esat_Pa)*100
# Initialize output. Prepopulate things that won't change with different tower pairs
# Make sure to check metadata for consistent sensor heights among the WS, TA, and concentration profiles
dmmyNum <- as.numeric(NA)
dmmyChr <- as.character(NA)
dmmyOut <- data.frame(timeEnd_A=dataSESVB$timeEnd,
                      timeBgn_A=dataSESVB$timeBgn,
                      TowerPosition_A=dmmyNum,
                      mean_A=dmmyNum,
                      qfFinl_A=dmmyNum,
                      min_A=dmmyNum,
                      max_A=dmmyNum,
                      vari_A=0,
                      numSamp_A=dmmyNum,
                      timeEnd_B=dataSESVB$timeEnd,
                      timeBgn_B=dataSESVB$timeBgn, 
                      TowerPosition_B=dmmyNum,
                      mean_B=dmmyNum,
                      qfFinl_B=dmmyNum,
                      min_B=dmmyNum,
                      max_B=dmmyNum,
                      vari_B=0,
                      numSamp_B=dmmyNum,
                      diffTowerPosition=dmmyChr,
                      dLevelsAminusB=dmmyChr,
                      dConc=dmmyNum, 
                      timeMid=dataSESVB$timeBgn+(dataSESVB$timeEnd-dataSESVB$timeBgn)/2,
                      match_time=dataSESVB$timeBgn+(dataSESVB$timeEnd-dataSESVB$timeBgn)/2,
                      TowerHeight_A=dmmyNum,
                      TowerHeight_B=dmmyNum, 
                      FC_turb_interp=dataSESVB$co2_flux_umolm2s_85m, 
                      FC_stor_interp=dmmyNum,
                      FC_nee_interp=dmmyNum,
                      LE_turb_interp=dataSESVB$LE_Wm2_85m, 
                      LE_stor_interp=dmmyNum,
                      LE_nsae_interp=dmmyNum,
                      H_turb_interp=dataSESVB$H_Wm2_85m,     
                      H_stor_interp=dmmyNum,
                      H_nsae_interp=dmmyNum,
                      ustar_interp=dataSESVB$ustar_ms_85m,     
                      roughLength_interp=dmmyNum,
                      RH=dmmyNum,  # at 85m.
                      P_kPa=dataSESVB$AirPress_hPa_85m/10,
                      PAR=dmmyNum, # No PAR from this site ###################
                      Tair8=dataSESVB$Ta_degC_35m, # Increasing number with height, start at level 11, 1st methane height
                      Tair13=dataSESVB$Ta_degC_85m, # Methane level is in the middle
                      Tair16=dataSESVB$Ta_degC_150m,
                      ubar13=dataSESVB$WS_ms_85m, #only one windspeed height
                      z_veg_aero=dmmyNum,
                      z_displ_calc=dmmyNum,
                      roughLength_calc=dmmyNum,
                      Tair_K=dmmyNum,
                      esat_Pa=dmmyNum,
                      e_Pa=dmmyNum,
                      VPD=dmmyNum,
                      rhoa_kgm3=dmmyNum,
                      rhov_kgm3=dmmyNum,
                      rho_kgm3=dmmyNum,
                      specificHumidity_pct=dmmyNum,
                      Cp_moist=dmmyNum,
                      Ftemp=dmmyNum,
                      lambda=dmmyNum,
                      FH2O_interp=dmmyNum,
                      FCH4_turb_interp=dataSESVB$ch4_flux_nmolm2s_85m, # New variable, since we have measured CH4 fluxes
                      FCH4_stor_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      FCH4_nsae_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      stringsAsFactors=FALSE)
# CH4 concentrations
# Tower levels 16-13
CH4_1613 <- dmmyOut
CH4_1613$TowerPosition_A=tower.heights$TowerPosition[16]
CH4_1613$TowerHeight_A=tower.heights$TowerHeight[16]
CH4_1613$mean_A=dataSESVB$ch4_nmolmol_150m 
CH4_1613$TowerPosition_B=tower.heights$TowerPosition[13]
CH4_1613$TowerHeight_B=tower.heights$TowerHeight[13]
CH4_1613$mean_B=dataSESVB$ch4_nmolmol_85m 
# Tower levels 13-8
CH4_138 <- dmmyOut
CH4_138$TowerPosition_A=tower.heights$TowerPosition[13]
CH4_138$TowerHeight_A=tower.heights$TowerHeight[13]
CH4_138$mean_A=dataSESVB$ch4_nmolmol_85m 
CH4_138$TowerPosition_B=tower.heights$TowerPosition[8]
CH4_138$TowerHeight_B=tower.heights$TowerHeight[8]
CH4_138$mean_B=dataSESVB$ch4_nmolmol_35m 
# Tower levels 16-8
CH4_168 <- dmmyOut
CH4_168$TowerPosition_A=tower.heights$TowerPosition[16]
CH4_168$TowerHeight_A=tower.heights$TowerHeight[16]
CH4_168$mean_A=dataSESVB$ch4_nmolmol_150m
CH4_168$TowerPosition_B=tower.heights$TowerPosition[8]
CH4_168$TowerHeight_B=tower.heights$TowerHeight[8]
CH4_168$mean_B=dataSESVB$ch4_nmolmol_35m 
# Combine all combos of CH4 paired levels
CH4out <- rbind(CH4_1613,CH4_138,CH4_168)
rm(CH4_1613,CH4_138,CH4_168)
# CO2 concentrations
# Tower levels 16-13
CO2_1613 <- dmmyOut
CO2_1613$TowerPosition_A=tower.heights$TowerPosition[16]
CO2_1613$TowerHeight_A=tower.heights$TowerHeight[16]
CO2_1613$mean_A=dataSESVB$co2_umolmol_150m 
CO2_1613$TowerPosition_B=tower.heights$TowerPosition[13]
CO2_1613$TowerHeight_B=tower.heights$TowerHeight[13]
CO2_1613$mean_B=dataSESVB$co2_umolmol_85m 
# Tower levels 13-8
CO2_138 <- dmmyOut
CO2_138$TowerPosition_A=tower.heights$TowerPosition[13]
CO2_138$TowerHeight_A=tower.heights$TowerHeight[13]
CO2_138$mean_A=dataSESVB$co2_umolmol_85m 
CO2_138$TowerPosition_B=tower.heights$TowerPosition[8]
CO2_138$TowerHeight_B=tower.heights$TowerHeight[8]
CO2_138$mean_B=dataSESVB$co2_umolmol_35m 
# Tower levels 16-8
CO2_168 <- dmmyOut
CO2_168$TowerPosition_A=tower.heights$TowerPosition[16]
CO2_168$TowerHeight_A=tower.heights$TowerHeight[16]
CO2_168$mean_A=dataSESVB$co2_umolmol_150m 
CO2_168$TowerPosition_B=tower.heights$TowerPosition[8]
CO2_168$TowerHeight_B=tower.heights$TowerHeight[8]
CO2_168$mean_B=dataSESVB$co2_umolmol_35m 
# Combine all combos of CH4 paired levels
CO2out <- rbind(CO2_1613,CO2_138,CO2_168)
rm(CO2_1613,CO2_138,CO2_168)
# H2O concentrations
# Tower levels 16-13
H2O_1613 <- dmmyOut
H2O_1613$TowerPosition_A=tower.heights$TowerPosition[16]
H2O_1613$TowerHeight_A=tower.heights$TowerHeight[16]
H2O_1613$mean_A=dataSESVB$h2o_mmolmol_150m 
H2O_1613$TowerPosition_B=tower.heights$TowerPosition[13]
H2O_1613$TowerHeight_B=tower.heights$TowerHeight[13]
H2O_1613$mean_B=dataSESVB$h2o_mmolmol_85m
# Tower levels 13-8
H2O_138 <- dmmyOut
H2O_138$TowerPosition_A=tower.heights$TowerPosition[13]
H2O_138$TowerHeight_A=tower.heights$TowerHeight[13]
H2O_138$mean_A=dataSESVB$h2o_mmolmol_85m
H2O_138$TowerPosition_B=tower.heights$TowerPosition[8]
H2O_138$TowerHeight_B=tower.heights$TowerHeight[8]
H2O_138$mean_B=dataSESVB$h2o_mmolmol_35m 
# Tower levels 16-8
H2O_168 <- dmmyOut
H2O_168$TowerPosition_A=tower.heights$TowerPosition[16]
H2O_168$TowerHeight_A=tower.heights$TowerHeight[16]
H2O_168$mean_A=dataSESVB$h2o_mmolmol_150m 
H2O_168$TowerPosition_B=tower.heights$TowerPosition[8]
H2O_168$TowerHeight_B=tower.heights$TowerHeight[8]
H2O_168$mean_B=dataSESVB$h2o_mmolmol_35m 
# Combine all combos of H2O paired levels
H2Oout <- rbind(H2O_1613,H2O_138,H2O_168)
rm(H2O_1613,H2O_138,H2O_168)
# Combine all gases into a single data frame
min9Diff.list <- list(CH4=CH4out,CO2=CO2out,H2O=H2Oout)
rm(CH4out,CO2out,H2Oout)
# ------------------- Get concentration diffs for subsequent tower levels --------------
# For each concentration, compute difference in concentration among tower levels
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  
  # Compute tower level diff A-B
  var$diffTowerPosition<- var$TowerPosition_A-var$TowerPosition_B
  var$dLevelsAminusB <- paste0(var$TowerPosition_A,'_',var$TowerPosition_B)
  
  # Compute concentration diffs
  var$dConc <- var$mean_A-var$mean_B
  
  return(var)
})
# Compute vegetation height based on turbulence measurements
# These equations stem from Eqn. 9.7.1b in Stull
####Not enough ubar measurments to deduce this
lvlTow <- 13 #this is the height at which the wind measurment was obtained
hgtMax <- tower.heights$TowerHeight[tower.heights$TowerPosition == 16]
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  var$z_veg_aero <- 10*as.numeric(hgtMax)/(exp(0.4*var[[paste0('ubar',lvlTow)]]/var$ustar_interp)+6.6) # m - aerodynamic vegetation height
  var$z_displ_calc <- 0.66*var$z_veg_aero # m - zero plane displacement height
  var$roughLength_calc <- 0.1*var$z_veg_aero # m - roughness length
  var$roughLength_interp <- var$roughLength_calc # set them the same, since not provided by PI
  return(var)
})
var = min9Diff.list$CO2
# -------------- Compute water flux from LE --------------------
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  
  # Grab Tair at tower top
  Tair_C <- var[[paste0('Tair', 16)]]
  
  # Variables to calculate air physics vars & convert H and LE to flux
  P_pa = var$P_kPa*1000  #Atmospheric pressure [Pa]
  ma = 28.964/1000    #molar mass of dry air [Kg/mol]
  mv = 18/1000        #molar mass of water vapor [Kg/mol]
  R = 8.314          #Universal gas constant dry air [J/(K mol)]
  Cpa_dry = 1004.67  #J Kg-1 K-1 - specific heat of dry air
  
  # Save these variables for aerodynamic method later on
  var$Tair_K = Tair_C + 273.16
  var$esat_Pa = 611.2*exp(17.67*(var$Tair_K-273.16)/(var$Tair_K-29.65)) #[Pa] saturated water vapor pressure
  
  var$e_Pa = var$RH*var$esat_Pa/100 #[Pa] vapor water pressure
  var$VPD = (var$esat_Pa-var$e_Pa)/1000
  var$rhoa_kgm3 = ma*(P_pa - var$e_Pa)/(R*var$Tair_K) # dry air density  [Kg/m3]
  var$rhov_kgm3 = mv*var$e_Pa/(R*var$Tair_K) # water vapor density  [Kg/m3]
  var$rho_kgm3 = var$rhoa_kgm3 + var$rhov_kgm3 # moist air density  [Kg/m3]
  var$specificHumidity_pct = var$rhov_kgm3/var$rho_kgm3 # Specific humidity
  var$Cp_moist = Cpa_dry*(1+0.84*var$specificHumidity_pct) # Specific heat of moist air [J kg-1 K-1] 
  
  # Convert H (W m-2) to w't' (Ftemp, K m-2 s-1) (Eqn. 40 in WPL, 1980)
  var$Ftemp = var$H_turb_interp / (var$Cp_moist*var$rho_kgm3) 
  
  # Convert LE (W m-2) to w'q' (FH2O, mmol m-2 s-1)
  var$lambda <- (2.501-0.00237*Tair_C)*1E6 # lambda = J kg-1 Eqn in back of Stull pg. 641
  var$FH2O_interp <- var$LE_turb_interp/var$lambda/mv*1000 # mmol m-2 s-1
  
  return(var)
})
