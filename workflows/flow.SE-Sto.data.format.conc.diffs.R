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
email <- 'kyle.delwiche@gmail.com'

site <- 'SE-Sto'


# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: fs, googledrive
options(digits=12)
library(foreach)
library(doParallel)
library(dplyr)

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

gdrive_path <- "https://drive.google.com/drive/u/1/folders/1F1qZkAZywNUq_fyS1OmlG3C9AkGo6fdc"

ch4_files_to_keep <- c(#'SE-Sto_met_30min.csv',
                       #'SE-Sto_gas_fluxes_30min.csv',
                       #'SE-Sto_concentration_profile_30min.csv',
                       'sesto_attr.csv')

# Identify desired files 
ch4_files <- googledrive::drive_ls(
  path = googledrive::as_id(gdrive_path),
) %>%  
  # Filter to keep only specified files
  dplyr::filter(name %in% ch4_files_to_keep) %>% 
  # Add explicit path column for safety
  dplyr::mutate(target_path = file.path("methane", "raw_methane", name))

# Download files with explicit path specification
purrr::walk2(
  .x = ch4_files$id,
  .y = ch4_files$target_path,
  .f = ~ googledrive::drive_download(
    file = googledrive::as_id(.x),
    path = .y,
    overwrite = TRUE
  )
)


# Load in data files
fileIn <- file.path("methane", "raw_methane",'SE-Sto_gas_fluxes_30min.csv')
dataFlux <- read.table(fileIn,header=TRUE,sep=",")

fileIn <- file.path("methane", "raw_methane",'SE-Sto_concentration_profile_30min.csv')
dataConc <- read.csv(fileIn,header=TRUE)

fileIn <- file.path("methane", "raw_methane",'SE-Sto_met_30min.csv')
dataMet <- read.csv(fileIn,header=TRUE)

fileIn <- file.path("methane", "raw_methane",'sesto_attr.csv')
attr.df <- read.csv(fileIn,header=TRUE)


## ***Manually update column names for necessary variables (tried making this fancy but it wasn't working, so just going to brute force it)
dataFlux <- dataFlux %>% rename(LE_turb_interp = LEraw_1_1_1)
dataFlux <- dataFlux %>% rename(H_turb_interp = H_1_1_1)
dataFlux <- dataFlux %>% rename(FC_turb_interp = Fc_1_1_1)
dataFlux <- dataFlux %>% rename(ustar_interp = Ustar_1_1_1)

dataMet <- dataMet %>% rename(P_kPa = Pa_1_1_1)
dataMet <- dataMet %>% rename(Tair1 = Ta_1_1_1)
dataMet <- dataMet %>% rename(RH = RH_1_1_1)

## Create timeBgn_A variable from date and time
dataFlux$date <- as.Date(dataFlux$date, format = "%Y-%m-%d")
dataFlux$timeEnd_A <- as.POSIXct(paste(dataFlux$date, dataFlux$time), format="%Y-%m-%d %H:%M:%S")

dataMet$date <- as.Date(dataMet$date, format = "%Y-%m-%d")
dataMet$timeEnd_A <- as.POSIXct(paste(dataMet$date, dataMet$time), format="%Y-%m-%d %H:%M:%S")

dataConc$date <- as.Date(dataConc$date, format = "%Y-%m-%d")
dataConc$timeEnd_A <- as.POSIXct(paste(dataConc$date, dataConc$time), format="%Y-%m-%d %H:%M:%S")



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

# Parse tower heights
tower.heights <- data.frame(TowerHeight=as.numeric(attr.df$DistZaxsLvlMeasTow),
                            TowerPosition=as.numeric(attr.df$TowerPosition))
if(any(diff(tower.heights$TowerPosition) < 0)){
  stop("Tower Position must be increasing in the attr.df file")
}

# ------------------- Populate output --------------------
# merge the tables based on measurement time
 

data <- left_join(dataFlux, dataConc, by='timeEnd_A')
rm('dataAmf','dataConc')

# CONCENTRATIONS ARE IN WET MOLE FRACTION. Convert to dry mole fraction (same as NEON data)
# Sadly there are no water vapor concentrations in the AmeriFlux output
# Use the RH profile at the same height (RH-capac sensor per the Instrument heights spreadsheet)
# Convert RH at each level to compute dry air mole fraction of water vapor
R = 8.314          #Universal gas constant dry air [J/(K mol)]
for (i in 1:4){
  Tair <- data[[paste0('TA_1_',i,'_1')]]
  RH <- data[[paste0('RH_1_',i,'_1')]]
  Tair_K <- Tair + 273.16 # K
  P_pa <- data$PA*1000  #Atmospheric pressure [Pa]
  esat_Pa <- 611.2*exp(17.67*(Tair_K-273.16)/(Tair_K-29.65)) #[Pa] saturated water vapor pressure
  e_Pa <- RH*esat_Pa/100 #[Pa] vapor water pressure
  rhoa <- (P_pa - e_Pa)/(R*Tair_K) # dry air molar density  [mol/m3]
  rhov <- e_Pa/(R*Tair_K) # water vapor molar density  [Kg/m3]
  Xwa <- rhov/rhoa # dry air mole fraction of water vapor [mol mol-1]
  
  # Now convert each gas constituent at this tower level to dry air mole fraction
  varCH4 <- paste0('CH4_1_',i,'_1')
  Xcw <- data[[varCH4]] # moist air mole fraction
  Xca <- Xcw*(1+Xwa) # dry air mole fraction
  data[[paste0(varCH4,'_MIXING_RATIO')]] <- Xca
  
  varCO2 <- paste0('CO2_1_',i,'_1')
  Xcw <- data[[varCO2]] # moist air mole fraction
  Xca <- Xcw*(1+Xwa) # dry air mole fraction
  data[[paste0(varCO2,'_MIXING_RATIO')]] <- Xca
  
  if (i == 1){
    varH2O <- "H2O.concentration..8m"
    Xcw <- data[[varH2O]] # moist air mole fraction
    Xca <- Xcw*(1+Xwa) # dry air mole fraction
    data[[paste0(varH2O,'_MIXING_RATIO')]] <- Xca
    
  } else if (i == 2){
    varH2O <- "H2O.concentration..4m"
    Xcw <- data[[varH2O]] # moist air mole fraction
    Xca <- Xcw*(1+Xwa) # dry air mole fraction
    data[[paste0(varH2O,'_MIXING_RATIO')]] <- Xca
    
  } 
  
}

# Filter down to where we have CH4 flux
data <- data %>% dplyr::filter(!is.na(CH4.flux))
data$timeBgn <- strptime(data$TIMESTAMP_START,"%Y%m%d%H%M")

# Initialize output. Prepopulate things that won't change with different tower pairs
# Make sure to check metadata for consistent sensor heights among the WS, TA, and concentration profiles
numData <- nrow(data)
dmmyNum <- as.numeric(NA)
dmmyChr <- as.character(NA)
dmmyOut <- data.frame(timeEnd_A=data$timeEnd,
                      timeBgn_A=data$timeBgn,
                      TowerPosition_A=dmmyNum,
                      mean_A=dmmyNum,
                      qfFinl_A=dmmyNum,
                      min_A=dmmyNum,
                      max_A=dmmyNum,
                      vari_A=0,
                      numSamp_A=dmmyNum,
                      timeEnd_B=data$timeEnd,
                      timeBgn_B=data$timeBgn,
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
                      timeMid=data$timeBgn+(data$timeEnd-data$timeBgn)/2,
                      match_time=data$timeBgn+(data$timeEnd-data$timeBgn)/2,
                      TowerHeight_A=dmmyNum,
                      TowerHeight_B=dmmyNum,
                      FC_turb_interp=data$FC,
                      FC_stor_interp=dmmyNum,
                      FC_nee_interp=dmmyNum,
                      LE_turb_interp=data$LE,
                      LE_stor_interp=dmmyNum,
                      LE_nsae_interp=dmmyNum,
                      H_turb_interp=data$H,
                      H_stor_interp=dmmyNum,
                      H_nsae_interp=dmmyNum,
                      ustar_interp=data$USTAR,
                      roughLength_interp=dmmyNum,
                      RH=data$RH_1_1_1,  # Tower top
                      P_kPa=data$PA,
                      PAR=data$PPFD_IN_PI_F, # Only gap-filled variable available
                      Tair1=data$TA_1_4_1, # Increasing number with height
                      Tair2=data$TA_1_3_1,
                      Tair3=data$TA_1_2_1,
                      Tair4=data$TA_1_1_1, # Tower top
                      ubar1=dmmyNum, # Increasing number with height
                      ubar2=data$WS_1_3_1,
                      ubar3=data$WS_1_2_1,
                      ubar4=data$WS_1_1_1, # Tower top
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
                      FCH4_turb_interp=data$FCH4, # New variable, since we have measured CH4 fluxes
                      FCH4_stor_interp=data$SCH4, # New variable, since we have measured CH4 fluxes
                      FCH4_nsae_interp=data$FCH4+data$SCH4, # New variable, since we have measured CH4 fluxes
                      stringsAsFactors=FALSE)

# Put concentrations into the output. This task is simple for this site since
# concentrations and fluxes have already been aggregated to 30-min intervals 

# CH4 concentrations
# Tower levels 4-3
CH4_43 <- dmmyOut
CH4_43$TowerPosition_A=tower.heights$TowerPosition[4]
CH4_43$TowerHeight_A=tower.heights$TowerHeight[4]
CH4_43$mean_A=data$CH4_1_1_1_MIXING_RATIO # AMF convention increments from top down
CH4_43$TowerPosition_B=tower.heights$TowerPosition[3]
CH4_43$TowerHeight_B=tower.heights$TowerHeight[3]
CH4_43$mean_B=data$CH4_1_2_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 3-2
CH4_32 <- dmmyOut
CH4_32$TowerPosition_A=tower.heights$TowerPosition[3]
CH4_32$TowerHeight_A=tower.heights$TowerHeight[3]
CH4_32$mean_A=data$CH4_1_2_1_MIXING_RATIO # AMF convention increments from top down
CH4_32$TowerPosition_B=tower.heights$TowerPosition[2]
CH4_32$TowerHeight_B=tower.heights$TowerHeight[2]
CH4_32$mean_B=data$CH4_1_3_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 2-1
CH4_21 <- dmmyOut
CH4_21$TowerPosition_A=tower.heights$TowerPosition[2]
CH4_21$TowerHeight_A=tower.heights$TowerHeight[2]
CH4_21$mean_A=data$CH4_1_3_1_MIXING_RATIO # AMF convention increments from top down
CH4_21$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_21$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_21$mean_B=data$CH4_1_4_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 4-1
CH4_41 <- dmmyOut
CH4_41$TowerPosition_A=tower.heights$TowerPosition[4]
CH4_41$TowerHeight_A=tower.heights$TowerHeight[4]
CH4_41$mean_A=data$CH4_1_1_1_MIXING_RATIO # AMF convention increments from top down
CH4_41$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_41$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_41$mean_B=data$CH4_1_4_1_MIXING_RATIO # AMF convention increments from top down

# Combine all combos of CH4 paired levels
CH4out <- rbind(CH4_43,CH4_32,CH4_21,CH4_41)
rm(CH4_43,CH4_32,CH4_21,CH4_41)


# CO2 concentrations
# Tower levels 4-3
CO2_43 <- dmmyOut
CO2_43$TowerPosition_A=tower.heights$TowerPosition[4]
CO2_43$TowerHeight_A=tower.heights$TowerHeight[4]
CO2_43$mean_A=data$CO2_1_1_1_MIXING_RATIO # AMF convention increments from top down
CO2_43$TowerPosition_B=tower.heights$TowerPosition[3]
CO2_43$TowerHeight_B=tower.heights$TowerHeight[3]
CO2_43$mean_B=data$CO2_1_2_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 3-2
CO2_32 <- dmmyOut
CO2_32$TowerPosition_A=tower.heights$TowerPosition[3]
CO2_32$TowerHeight_A=tower.heights$TowerHeight[3]
CO2_32$mean_A=data$CO2_1_2_1_MIXING_RATIO # AMF convention increments from top down
CO2_32$TowerPosition_B=tower.heights$TowerPosition[2]
CO2_32$TowerHeight_B=tower.heights$TowerHeight[2]
CO2_32$mean_B=data$CO2_1_3_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 2-1
CO2_21 <- dmmyOut
CO2_21$TowerPosition_A=tower.heights$TowerPosition[2]
CO2_21$TowerHeight_A=tower.heights$TowerHeight[2]
CO2_21$mean_A=data$CO2_1_3_1_MIXING_RATIO # AMF convention increments from top down
CO2_21$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_21$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_21$mean_B=data$CO2_1_4_1_MIXING_RATIO # AMF convention increments from top down

# Tower levels 4-1
CO2_41 <- dmmyOut
CO2_41$TowerPosition_A=tower.heights$TowerPosition[4]
CO2_41$TowerHeight_A=tower.heights$TowerHeight[4]
CO2_41$mean_A=data$CO2_1_1_1_MIXING_RATIO # AMF convention increments from top down
CO2_41$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_41$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_41$mean_B=data$CO2_1_4_1_MIXING_RATIO # AMF convention increments from top down

# Combine all combos of CO2 paired levels
CO2out <- rbind(CO2_43,CO2_32,CO2_21,CO2_41)
rm(CO2_43,CO2_32,CO2_21,CO2_41)

# H2O concentrations - only available for top 2 tower levels
# Tower levels 4-3
H2O_43 <- dmmyOut
H2O_43$TowerPosition_A=tower.heights$TowerPosition[4]
H2O_43$TowerHeight_A=tower.heights$TowerHeight[4]
H2O_43$mean_A=data$H2O.concentration..8m_MIXING_RATIO # H2O concentrations not in AmeriFlux output
H2O_43$TowerPosition_B=tower.heights$TowerPosition[3]
H2O_43$TowerHeight_B=tower.heights$TowerHeight[3]
H2O_43$mean_B=data$H2O.concentration..4m_MIXING_RATIO # H2O concentrations not in AmeriFlux output

# Combine all combos of H2O paired levels
H2Oout <- rbind(H2O_43)
rm(H2O_43)

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
lvlTow <- 4
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
