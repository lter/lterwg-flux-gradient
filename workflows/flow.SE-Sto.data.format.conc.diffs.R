# Formats external site (validation data) into the same format as NEON paired concentration difference data.
#
# Step 1.: Rename variables so names are standardized
# Step 2.: Preform site-specific data manipulations to make sure units are correct, datetimes are correct, etc.
# Step 3.: Create tower height data pairs
# Step 4.: Run throughmin9Diff.list

# Merges together flux, met, and profile concentration data for site SE-Sto . Aligns the profile 
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
# email <- 'csturtevant@battelleecology.org'
#email <- 'kyle.delwiche@gmail.com'



email <- 'sam.jurado@yale.edu'
site <- 'SE-Sto'
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
focal_files <- c('SE-Sto_met_30min.csv',
                       'SE-Sto_gas_fluxes_30min.csv',
                      'SE-Sto_concentration_profile_30min.csv',
                      'sesto_attr.csv')
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
fileIn <- fs::path(dirTmp,"SE-Sto_met_30min.csv")
dataMet <- read.table(fileIn,header=TRUE,sep=",",skip=0)

fileIn <- fs::path(dirTmp,"SE-Sto_gas_fluxes_30min.csv")
dataFlux <- read.table(fileIn,header=TRUE,sep=",",skip=0)

fileIn <- fs::path(dirTmp,"SE-Sto_concentration_profile_30min.csv")
dataConc <- read.table(fileIn,header=TRUE,sep=",",skip=0)

fileIn <- fs::path(dirTmp,"sesto_attr.csv")
attr.df <- read.csv(fileIn)

# Transform the attr.df data frame into same format as NEON sites
attr.df.transpose <- as.data.frame(t(attr.df))
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

## ***Manually update column names for necessary variables (tried making this fancy but it wasn't working, so just going to brute force it)
# dataFlux <- dataFlux %>% rename(LE_turb_interp = LEraw_1_1_1)
# dataFlux <- dataFlux %>% rename(H_turb_interp = H_1_1_1)
# dataFlux <- dataFlux %>% rename(FC_turb_interp = Fc_1_1_1)
# dataFlux <- dataFlux %>% rename(ustar_interp = Ustar_1_1_1)
# 
# dataMet <- dataMet %>% rename(P_kPa = Pa_1_1_1)
# dataMet <- dataMet %>% rename(Tair1 = Ta_1_1_1)
# dataMet <- dataMet %>% rename(RH = RH_1_1_1)

## Create timeBgn_A variable from date and time

# for dataFlux the date column changes formats half-way through, so will instead have to create a date from doy and time, knowing that years start in 2014
# Initialize variables
current_year <- 2014
previous_integer_doy <- 0  # Track the integer day of year

# Initialize Year column
dataFlux$year <- NA

for (i in 1:nrow(dataFlux)) {
    doy <- dataFlux$doy[i]# Check for year transition (e.g., 365 -> 1)
  if (doy < previous_integer_doy) {
    current_year <- current_year + 1
  }
  
  dataFlux$year[i] <- current_year
  previous_integer_doy <- doy
}

dataFlux$doy <- floor(dataFlux$doy)
dataFlux$timeEnd_A  <- make_datetime(
  year = dataFlux$year,
  month = 1,
  day = 1,
  hour = hour(hms(dataFlux$time)),
  min = minute(hms(dataFlux$time)),
  sec = second(hms(dataFlux$time))
) + days(dataFlux$doy - 1)

# now do the same for met (code could be cleaned up, for now just repeat it)
current_year <- 2014
previous_integer_doy <- 0  # Track the integer day of year

# Initialize Year column
dataMet$year <- NA
for (i in 1:nrow(dataMet)) {
  doy <- dataMet$doy[i]# Check for year transition (e.g., 365 -> 1)
  if (doy < previous_integer_doy) {
    current_year <- current_year + 1
  }
  
  dataMet$year[i] <- current_year
  previous_integer_doy <- doy
}

#now do concentration data
dataMet$doy <- floor(dataMet$doy)
dataMet$timeEnd_A  <- make_datetime(
  year = dataMet$year,
  month = 1,
  day = 1,
  hour = hour(hms(dataMet$time)),
  min = minute(hms(dataMet$time)),
  sec = second(hms(dataMet$time))
) + days(dataMet$doy - 1)


current_year <- 2014
previous_integer_doy <- 0  # Track the integer day of year

# Initialize Year column
dataConc$year <- NA

for (i in 1:nrow(dataConc)) {
  doy <- dataConc$doy[i]# Check for year transition (e.g., 365 -> 1)
  if (doy < previous_integer_doy) {
    current_year <- current_year + 1
  }
  
  dataConc$year[i] <- current_year
  previous_integer_doy <- doy
}


# data format for dataFlux changes half-way through, so create a year column from DOY, then year use, DOY, and time to create timeEnd_A

dataConc$doy <- floor(dataConc$doy)
dataConc$timeEnd_A  <- make_datetime(
  year = dataFlux$year,
  month = 1,
  day = 1,
  hour = hour(hms(dataConc$time)),
  min = minute(hms(dataConc$time)),
  sec = second(hms(dataConc$time))
) + days(dataConc$doy - 1)

# Parse tower heights
tower.heights <- data.frame(TowerHeight=as.numeric(attr.df$DistZaxsLvlMeasTow),
                            TowerPosition=as.numeric(attr.df$TowerPosition))

# ------------------- Populate output --------------------
# merge the tables based on measurement time
 

data <- left_join(dataFlux, dataConc,  by='timeEnd_A')
data <- left_join(data, dataMet,  by='timeEnd_A')

#remove some duplicate columns
data <- data %>%
  select(timeEnd_A, year = year.x, doy = doy.x, date = date.x, time = time.x,
         everything(), 
         -ends_with(".y"), -ends_with(".x"))
rm('dataFlux','dataConc', 'dataMet')


#update time zones for data
data$timeEnd_A <- as.POSIXct(data$timeEnd_A, format="%d-%b-%Y %H:%M:%S", tz="CET")
data$timeEnd_A <- with_tz(data$timeEnd_A, tzone = "UTC")
data$timeBgn_A <- data$timeEnd_A - minutes(30)


# CONCENTRATIONS ARE IN WET MOLE FRACTION. Convert to dry mole fraction (same as NEON data)
# Sadly there are no water vapor concentrations in the AmeriFlux output
# Use the RH profile at the same height (RH-capac sensor per the Instrument heights spreadsheet)
# Convert RH at each level to compute dry air mole fraction of water vapor

# 

# R = 8.314          #Universal gas constant dry air [J/(K mol)]
# for (i in 1:4){
#   Tair <- data[[paste0('TA_1_',i,'_1')]]
#   RH <- data[[paste0('RH_1_',i,'_1')]]
#   Tair_K <- Tair + 273.16 # K
#   P_pa <- data$PA*1000  #Atmospheric pressure [Pa]
#   esat_Pa <- 611.2*exp(17.67*(Tair_K-273.16)/(Tair_K-29.65)) #[Pa] saturated water vapor pressure
#   e_Pa <- RH*esat_Pa/100 #[Pa] vapor water pressure
#   rhoa <- (P_pa - e_Pa)/(R*Tair_K) # dry air molar density  [mol/m3]
#   rhov <- e_Pa/(R*Tair_K) # water vapor molar density  [Kg/m3]
#   Xwa <- rhov/rhoa # dry air mole fraction of water vapor [mol mol-1]
#   
#   # Now convert each gas constituent at this tower level to dry air mole fraction
#   varCH4 <- paste0('CH4_1_',i,'_1')
#   Xcw <- data[[varCH4]] # moist air mole fraction
#   Xca <- Xcw*(1+Xwa) # dry air mole fraction
#   data[[paste0(varCH4,'_MIXING_RATIO')]] <- Xca
#   
#   varCO2 <- paste0('CO2_1_',i,'_1')
#   Xcw <- data[[varCO2]] # moist air mole fraction
#   Xca <- Xcw*(1+Xwa) # dry air mole fraction
#   data[[paste0(varCO2,'_MIXING_RATIO')]] <- Xca
#   
#   if (i == 1){
#     varH2O <- "H2O.concentration..8m"
#     Xcw <- data[[varH2O]] # moist air mole fraction
#     Xca <- Xcw*(1+Xwa) # dry air mole fraction
#     data[[paste0(varH2O,'_MIXING_RATIO')]] <- Xca
#     
#   } else if (i == 2){
#     varH2O <- "H2O.concentration..4m"
#     Xcw <- data[[varH2O]] # moist air mole fraction
#     Xca <- Xcw*(1+Xwa) # dry air mole fraction
#     data[[paste0(varH2O,'_MIXING_RATIO')]] <- Xca
#     
#   } 
#   
# }

# Filter down to where we have CH4 flux
# data <- data %>% dplyr::filter(!is.na(CH4.flux))
# data$timeBgn <- strptime(data$TIMESTAMP_START,"%Y%m%d%H%M")

# Initialize output. Prepopulate things that won't change with different tower pairs
# Make sure to check metadata for consistent sensor heights among the WS, TA, and concentration profiles
numData <- nrow(data)
# dmmyNum <- as.numeric(NA)
# dmmyChr <- as.character(NA)
dmmyNum <- rep(NA_real_, numData)
dmmyChr <- rep(NA_character_, numData)

dmmyOut <- data.frame(timeEnd_A=data$timeEnd_A,
                      timeBgn_A=data$timeBgn_A,
                      TowerPosition_A=dmmyNum,
                      mean_A=dmmyNum,
                      qfFinl_A=dmmyNum,
                      min_A=dmmyNum,
                      max_A=dmmyNum,
                      vari_A=0,
                      numSamp_A=dmmyNum,
                      timeEnd_B=data$timeEnd_A,
                      timeBgn_B=data$timeBgn_A,
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
                      timeMid=data$timeBgn_A+(data$timeEnd_A-data$timeBgn_A)/2,
                      match_time=data$timeBgn_A+(data$timeEnd_A-data$timeBgn_A)/2,
                      TowerHeight_A=dmmyNum,
                      TowerHeight_B=dmmyNum,
                      FC_turb_interp=data$Fc_1_1_1,
                      FC_stor_interp=dmmyNum,
                      FC_nee_interp=dmmyNum,
                      LE_turb_interp=data$LE_1_1_1,
                      LE_stor_interp=dmmyNum,
                      LE_nsae_interp=dmmyNum,
                      H_turb_interp=data$H_1_1_1,
                      H_stor_interp=dmmyNum,
                      H_nsae_interp=dmmyNum,
                      ustar_interp=data$Ustar_1_1_1,
                      roughLength_interp=dmmyNum,
                      RH=data$RH_1_1_1,  # Tower top
                      P_kPa=data$Pa_1_1_1,
                      PAR=data$PPFD_IN_1_2_1, # Only gap-filled variable available
                      Tair1=dmmyNum, # Tower top, only one temp variable available
                      Tair2=dmmyNum, # Tower top, only one temp variable available
                      Tair3=dmmyNum, # Tower top, only one temp variable available
                      Tair4=dmmyNum, # Tower top, only one temp variable available
                      Tair5=data$Ta_1_1_1, # Tower top, only one temp variable available
                      ubar1=dmmyNum, # Increasing number with height
                      ubar2=dmmyNum, # Increasing number with height
                      ubar3=dmmyNum, # Increasing number with height
                      ubar4=dmmyNum, # Increasing number with height
                      ubar5=data$WS_1_1_1, # Tower top, only one ws variable available
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
                      FCH4_turb_interp=data$Fch4_1_1_1, # New variable, since we have measured CH4 fluxes
                      FCH4_stor_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      FCH4_nsae_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      stringsAsFactors=FALSE)

# Put concentrations into the output. This task is simple for this site since
# concentrations and fluxes have already been aggregated to 30-min intervals 

# temporarily add _MIXING_RATIO to concentration variable names (I need to figure out if these are dry or wet 
# mole fractions.  If wet, go through the calculations above to convert to dry, and that code adds _MIXING_RATIO to variable names)
data$CH4_1_1_1_MIXING_RATIO <- data$CH4_1_1_1
data$CH4_1_2_1_MIXING_RATIO <- data$CH4_1_2_1
data$CH4_1_3_1_MIXING_RATIO <- data$CH4_1_3_1
data$CH4_1_4_1_MIXING_RATIO <- data$CH4_1_4_1
data$CH4_1_5_1_MIXING_RATIO <- data$CH4_1_5_1
data$CO2_1_1_1_MIXING_RATIO <- data$CO2_1_1_1
data$CO2_1_2_1_MIXING_RATIO <- data$CO2_1_2_1
data$CO2_1_3_1_MIXING_RATIO <- data$CO2_1_3_1
data$CO2_1_4_1_MIXING_RATIO <- data$CO2_1_4_1
data$CO2_1_5_1_MIXING_RATIO <- data$CO2_1_5_1
data$H2O_1_1_1_MIXING_RATIO <- data$H2O_1_1_1
data$H2O_1_2_1_MIXING_RATIO <- data$H2O_1_2_1
data$H2O_1_3_1_MIXING_RATIO <- data$H2O_1_3_1
data$H2O_1_4_1_MIXING_RATIO <- data$H2O_1_4_1
data$H2O_1_5_1_MIXING_RATIO <- data$H2O_1_5_1

# CH4 concentrations
# Tower levels 5-1
CH4_51 <- dmmyOut
CH4_51$TowerPosition_A=tower.heights$TowerPosition[5]
CH4_51$TowerHeight_A=tower.heights$TowerHeight[5]
CH4_51$mean_A=data$CH4_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_51$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_51$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_51$mean_B=data$CH4_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-2
CH4_52 <- dmmyOut
CH4_52$TowerPosition_A=tower.heights$TowerPosition[5]
CH4_52$TowerHeight_A=tower.heights$TowerHeight[5]
CH4_52$mean_A=data$CH4_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_52$TowerPosition_B=tower.heights$TowerPosition[2]
CH4_52$TowerHeight_B=tower.heights$TowerHeight[2]
CH4_52$mean_B=data$CH4_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-3
CH4_53 <- dmmyOut
CH4_53$TowerPosition_A=tower.heights$TowerPosition[5]
CH4_53$TowerHeight_A=tower.heights$TowerHeight[5]
CH4_53$mean_A=data$CH4_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_53$TowerPosition_B=tower.heights$TowerPosition[3]
CH4_53$TowerHeight_B=tower.heights$TowerHeight[3]
CH4_53$mean_B=data$CH4_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-4
CH4_54 <- dmmyOut
CH4_54$TowerPosition_A=tower.heights$TowerPosition[5]
CH4_54$TowerHeight_A=tower.heights$TowerHeight[5]
CH4_54$mean_A=data$CH4_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_54$TowerPosition_B=tower.heights$TowerPosition[4]
CH4_54$TowerHeight_B=tower.heights$TowerHeight[4]
CH4_54$mean_B=data$CH4_1_2_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-1
CH4_41 <- dmmyOut
CH4_41$TowerPosition_A=tower.heights$TowerPosition[4]
CH4_41$TowerHeight_A=tower.heights$TowerHeight[4]
CH4_41$mean_A=data$CH4_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_41$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_41$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_41$mean_B=data$CH4_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-2
CH4_42 <- dmmyOut
CH4_42$TowerPosition_A=tower.heights$TowerPosition[4]
CH4_42$TowerHeight_A=tower.heights$TowerHeight[4]
CH4_42$mean_A=data$CH4_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_42$TowerPosition_B=tower.heights$TowerPosition[2]
CH4_42$TowerHeight_B=tower.heights$TowerHeight[2]
CH4_42$mean_B=data$CH4_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-3
CH4_43 <- dmmyOut
CH4_43$TowerPosition_A=tower.heights$TowerPosition[4]
CH4_43$TowerHeight_A=tower.heights$TowerHeight[4]
CH4_43$mean_A=data$CH4_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_43$TowerPosition_B=tower.heights$TowerPosition[3]
CH4_43$TowerHeight_B=tower.heights$TowerHeight[3]
CH4_43$mean_B=data$CH4_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-1
CH4_31 <- dmmyOut
CH4_31$TowerPosition_A=tower.heights$TowerPosition[3]
CH4_31$TowerHeight_A=tower.heights$TowerHeight[3]
CH4_31$mean_A=data$CH4_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_31$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_31$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_31$mean_B=data$CH4_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-2
CH4_32 <- dmmyOut
CH4_32$TowerPosition_A=tower.heights$TowerPosition[3]
CH4_32$TowerHeight_A=tower.heights$TowerHeight[3]
CH4_32$mean_A=data$CH4_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_32$TowerPosition_B=tower.heights$TowerPosition[2]
CH4_32$TowerHeight_B=tower.heights$TowerHeight[2]
CH4_32$mean_B=data$CH4_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 2-1
CH4_21 <- dmmyOut
CH4_21$TowerPosition_A=tower.heights$TowerPosition[2]
CH4_21$TowerHeight_A=tower.heights$TowerHeight[2]
CH4_21$mean_A=data$CH4_1_4_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CH4_21$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_21$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_21$mean_B=data$CH4_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Combine all combos of CH4 paired levels
CH4out <- rbind(CH4_51, CH4_52, CH4_53, CH4_54, CH4_41,CH4_42,CH4_43, CH4_31, CH4_32, CH4_21)
rm(CH4_51, CH4_52, CH4_53, CH4_54, CH4_41,CH4_42,CH4_43, CH4_31, CH4_32, CH4_21)


# CO2 concentrations
CO2_51 <- dmmyOut
CO2_51$TowerPosition_A=tower.heights$TowerPosition[5]
CO2_51$TowerHeight_A=tower.heights$TowerHeight[5]
CO2_51$mean_A=data$CO2_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_51$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_51$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_51$mean_B=data$CO2_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-2
CO2_52 <- dmmyOut
CO2_52$TowerPosition_A=tower.heights$TowerPosition[5]
CO2_52$TowerHeight_A=tower.heights$TowerHeight[5]
CO2_52$mean_A=data$CO2_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_52$TowerPosition_B=tower.heights$TowerPosition[2]
CO2_52$TowerHeight_B=tower.heights$TowerHeight[2]
CO2_52$mean_B=data$CO2_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-3
CO2_53 <- dmmyOut
CO2_53$TowerPosition_A=tower.heights$TowerPosition[5]
CO2_53$TowerHeight_A=tower.heights$TowerHeight[5]
CO2_53$mean_A=data$CO2_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_53$TowerPosition_B=tower.heights$TowerPosition[3]
CO2_53$TowerHeight_B=tower.heights$TowerHeight[3]
CO2_53$mean_B=data$CO2_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-4
CO2_54 <- dmmyOut
CO2_54$TowerPosition_A=tower.heights$TowerPosition[5]
CO2_54$TowerHeight_A=tower.heights$TowerHeight[5]
CO2_54$mean_A=data$CO2_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_54$TowerPosition_B=tower.heights$TowerPosition[4]
CO2_54$TowerHeight_B=tower.heights$TowerHeight[4]
CO2_54$mean_B=data$CO2_1_2_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-1
CO2_41 <- dmmyOut
CO2_41$TowerPosition_A=tower.heights$TowerPosition[4]
CO2_41$TowerHeight_A=tower.heights$TowerHeight[4]
CO2_41$mean_A=data$CO2_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_41$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_41$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_41$mean_B=data$CO2_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-2
CO2_42 <- dmmyOut
CO2_42$TowerPosition_A=tower.heights$TowerPosition[4]
CO2_42$TowerHeight_A=tower.heights$TowerHeight[4]
CO2_42$mean_A=data$CO2_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_42$TowerPosition_B=tower.heights$TowerPosition[2]
CO2_42$TowerHeight_B=tower.heights$TowerHeight[2]
CO2_42$mean_B=data$CO2_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-3
CO2_43 <- dmmyOut
CO2_43$TowerPosition_A=tower.heights$TowerPosition[4]
CO2_43$TowerHeight_A=tower.heights$TowerHeight[4]
CO2_43$mean_A=data$CO2_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_43$TowerPosition_B=tower.heights$TowerPosition[3]
CO2_43$TowerHeight_B=tower.heights$TowerHeight[3]
CO2_43$mean_B=data$CO2_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-1
CO2_31 <- dmmyOut
CO2_31$TowerPosition_A=tower.heights$TowerPosition[3]
CO2_31$TowerHeight_A=tower.heights$TowerHeight[3]
CO2_31$mean_A=data$CO2_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_31$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_31$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_31$mean_B=data$CO2_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-2
CO2_32 <- dmmyOut
CO2_32$TowerPosition_A=tower.heights$TowerPosition[3]
CO2_32$TowerHeight_A=tower.heights$TowerHeight[3]
CO2_32$mean_A=data$CO2_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_32$TowerPosition_B=tower.heights$TowerPosition[2]
CO2_32$TowerHeight_B=tower.heights$TowerHeight[2]
CO2_32$mean_B=data$CO2_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 2-1
CO2_21 <- dmmyOut
CO2_21$TowerPosition_A=tower.heights$TowerPosition[2]
CO2_21$TowerHeight_A=tower.heights$TowerHeight[2]
CO2_21$mean_A=data$CO2_1_4_1_MIXING_RATIO # I believe ICOS convention is starting from top down
CO2_21$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_21$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_21$mean_B=data$CO2_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down


CO2out <- rbind(CO2_51, CO2_52, CO2_53, CO2_54, CO2_41,CO2_42,CO2_43, CO2_31, CO2_32, CO2_21)
rm(CO2_51, CO2_52, CO2_53, CO2_54, CO2_41,CO2_42,CO2_43, CO2_31, CO2_32, CO2_21)

# Combine all combos of H2O paired levels
# H2O concentrations
H2O_51 <- dmmyOut
H2O_51$TowerPosition_A=tower.heights$TowerPosition[5]
H2O_51$TowerHeight_A=tower.heights$TowerHeight[5]
H2O_51$mean_A=data$H2O_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_51$TowerPosition_B=tower.heights$TowerPosition[1]
H2O_51$TowerHeight_B=tower.heights$TowerHeight[1]
H2O_51$mean_B=data$H2O_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-2
H2O_52 <- dmmyOut
H2O_52$TowerPosition_A=tower.heights$TowerPosition[5]
H2O_52$TowerHeight_A=tower.heights$TowerHeight[5]
H2O_52$mean_A=data$H2O_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_52$TowerPosition_B=tower.heights$TowerPosition[2]
H2O_52$TowerHeight_B=tower.heights$TowerHeight[2]
H2O_52$mean_B=data$H2O_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-3
H2O_53 <- dmmyOut
H2O_53$TowerPosition_A=tower.heights$TowerPosition[5]
H2O_53$TowerHeight_A=tower.heights$TowerHeight[5]
H2O_53$mean_A=data$H2O_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_53$TowerPosition_B=tower.heights$TowerPosition[3]
H2O_53$TowerHeight_B=tower.heights$TowerHeight[3]
H2O_53$mean_B=data$H2O_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 5-4
H2O_54 <- dmmyOut
H2O_54$TowerPosition_A=tower.heights$TowerPosition[5]
H2O_54$TowerHeight_A=tower.heights$TowerHeight[5]
H2O_54$mean_A=data$H2O_1_1_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_54$TowerPosition_B=tower.heights$TowerPosition[4]
H2O_54$TowerHeight_B=tower.heights$TowerHeight[4]
H2O_54$mean_B=data$H2O_1_2_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-1
H2O_41 <- dmmyOut
H2O_41$TowerPosition_A=tower.heights$TowerPosition[4]
H2O_41$TowerHeight_A=tower.heights$TowerHeight[4]
H2O_41$mean_A=data$H2O_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_41$TowerPosition_B=tower.heights$TowerPosition[1]
H2O_41$TowerHeight_B=tower.heights$TowerHeight[1]
H2O_41$mean_B=data$H2O_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-2
H2O_42 <- dmmyOut
H2O_42$TowerPosition_A=tower.heights$TowerPosition[4]
H2O_42$TowerHeight_A=tower.heights$TowerHeight[4]
H2O_42$mean_A=data$H2O_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_42$TowerPosition_B=tower.heights$TowerPosition[2]
H2O_42$TowerHeight_B=tower.heights$TowerHeight[2]
H2O_42$mean_B=data$H2O_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 4-3
H2O_43 <- dmmyOut
H2O_43$TowerPosition_A=tower.heights$TowerPosition[4]
H2O_43$TowerHeight_A=tower.heights$TowerHeight[4]
H2O_43$mean_A=data$H2O_1_2_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_43$TowerPosition_B=tower.heights$TowerPosition[3]
H2O_43$TowerHeight_B=tower.heights$TowerHeight[3]
H2O_43$mean_B=data$H2O_1_3_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-1
H2O_31 <- dmmyOut
H2O_31$TowerPosition_A=tower.heights$TowerPosition[3]
H2O_31$TowerHeight_A=tower.heights$TowerHeight[3]
H2O_31$mean_A=data$H2O_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_31$TowerPosition_B=tower.heights$TowerPosition[1]
H2O_31$TowerHeight_B=tower.heights$TowerHeight[1]
H2O_31$mean_B=data$H2O_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 3-2
H2O_32 <- dmmyOut
H2O_32$TowerPosition_A=tower.heights$TowerPosition[3]
H2O_32$TowerHeight_A=tower.heights$TowerHeight[3]
H2O_32$mean_A=data$H2O_1_3_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_32$TowerPosition_B=tower.heights$TowerPosition[2]
H2O_32$TowerHeight_B=tower.heights$TowerHeight[2]
H2O_32$mean_B=data$H2O_1_4_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Tower levels 2-1
H2O_21 <- dmmyOut
H2O_21$TowerPosition_A=tower.heights$TowerPosition[2]
H2O_21$TowerHeight_A=tower.heights$TowerHeight[2]
H2O_21$mean_A=data$H2O_1_4_1_MIXING_RATIO # I believe ICOS convention is starting from top down
H2O_21$TowerPosition_B=tower.heights$TowerPosition[1]
H2O_21$TowerHeight_B=tower.heights$TowerHeight[1]
H2O_21$mean_B=data$H2O_1_5_1_MIXING_RATIO #  I believe ICOS convention is starting from top down

# Combine all combos of H2O paired levels
H2Oout <- rbind(H2O_51, H2O_52, H2O_53, H2O_54, H2O_41,H2O_42,H2O_43, H2O_31, H2O_32, H2O_21)
rm(H2O_51, H2O_52, H2O_53, H2O_54, H2O_41,H2O_42,H2O_43, H2O_31, H2O_32, H2O_21)


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
lvlTow <- 5   #5 heights at SE-Sto
hgtMax <- tower.heights$TowerHeight[tower.heights$TowerPosition == lvlTow]


min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  var$z_veg_aero <- 10*as.numeric(hgtMax)/(exp(0.4*var[[paste0('ubar',lvlTow)]]/var$ustar_interp)+6.6) # m - aerodynamic vegetation height
  var$z_displ_calc <- 0.66*var$z_veg_aero # m - zero plane displacement height
  var$roughLength_calc <- 0.1*var$z_veg_aero # m - roughness length
  var$roughLength_interp <- var$roughLength_calc # set them the same, since not provided by PI
  return(var)
})


# -------------- Compute water flux from LE --------------------
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  
  # Grab Tair at tower top
  Tair_C <- var[[paste0('Tair',lvlTow)]]
  
  # Variables to calculate air physics vars & convert H and LE to flux
  P_pa = var$P_kPa*100  #Atmospheric pressure [Pa] In this file, they were actaully hecto pascals
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



# -------- Save and zip the files to the temp directory. Upload to google drive. -------
# for some reason this code isn't saving and uploading the .zip file, I'm not sure why...
# -------- Save and zip the files to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.zip'))
save(min9Diff.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_aligned_conc_flux_9min.RData'))
setwd(wdPrev)

# Save in same folder as NEON site data
drive_url_NEONSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folderUpld <- googledrive::drive_ls(path = drive_url_NEONSiteData)
site_folderUpld <- googledrive::drive_ls(path = data_folderUpld$id[data_folderUpld$name==site])
googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folderUpld$id[data_folderUpld$name==site]) 

# Do the same for attr.df (attribute file). 
# Zip the file with the same folder structure as those created for NEON files 
pathSaveAttr <- fs::path(dirTmp,"data", site)
dir.create(pathSaveAttr,recursive = TRUE)
fileSaveAttr <- fs::path("data", site, paste0(site,'_attr.RData'))
fileZipAttr <- fs::path("data", site, paste0(site,'_attr.zip'))
wdPrev <- getwd()
setwd(dirTmp)
save(attr.df,file=fileSaveAttr)
utils::zip(zipfile=fileZipAttr,files=fileSaveAttr)
googledrive::drive_upload(media = fileZipAttr, overwrite = T, path = data_folderUpld$id[data_folderUpld$name==site])
setwd(wdPrev)




# fileSave <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
# fileZip <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.zip'))
# save(min9Diff.list,file=fileSave)
# wdPrev <- getwd()
# setwd(dirTmp)
# utils::zip(zipfile=fileZip,files=paste0(site,'_aligned_conc_flux_9min.RData'))
# setwd(wdPrev)
# 
# # Save in same folder as NEON site data
# drive_url_NEONSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
# data_folderUpld <- googledrive::drive_ls(path = drive_url_NEONSiteData)
# site_folderUpld <- googledrive::drive_ls(path = data_folderUpld$id[data_folderUpld$name==site])
# googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folderUpld$id[data_folderUpld$name==site]) 
# 
# # Do the same for attr.df (attribute file). 
# # Zip the file with the same folder structure as those created for NEON files 
# pathSaveAttr <- fs::path(dirTmp,"data", site)
# dir.create(pathSaveAttr,recursive = TRUE)
# fileSaveAttr <- fs::path("data", site, paste0(site,'_attr.RData'))
# fileZipAttr <- fs::path("data", site, paste0(site,'_attr.zip'))
# wdPrev <- getwd()
# setwd(dirTmp)
# save(attr.df,file=fileSaveAttr)
# utils::zip(zipfile=fileZipAttr,files=fileSaveAttr)
# googledrive::drive_upload(media = fileZipAttr, overwrite = T, path = data_folderUpld$id[data_folderUpld$name==site])
# setwd(wdPrev)
