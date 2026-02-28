# Process FI-Hyy data and add Google Drive integration
# For loading data to/from google drive
#email <- 'jonathan.gewirtzman@yale.edu'  # Use the same email as US-Uaf script
#email <- 'sam.jurado@yale.edu' 
site <- 'FI-Hyy'

# ------ Prerequisites ------
library(dplyr)
library(lubridate)
library(purrr)
library(googledrive)  # For Google Drive integration
library(fs)           # For file path handling

# Set up temporary directory for file downloads
dirTmp <- fs::path(localdir1, site)
dir.create(dirTmp, recursive = TRUE)

# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # May need email=TRUE for interactive auth
drive_url_extSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1jrOJIu5WfdzmlbL9vMkUNfzBpRC-W0Wd")
data_folder <- googledrive::drive_ls(path = drive_url_extSiteData)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Define files to download
focal_files <- c(
  "FI-Hyy_concentration_profile_1min.csv",
  "FI-Hyy_met_1min.csv",
  "FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv"
)

# Download files from Google Drive
for(focal_file in focal_files){
  # Find the file identifier
  file_id <- subset(site_folder, name == focal_file)
  
  # Download the file
  pathDnld <- fs::path(dirTmp, focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip if needed
  if(grepl(pattern='.zip', focal_file)){
    utils::unzip(pathDnld, exdir=dirTmp)
  }
}

# Read the downloaded data files
fihyy <- read.csv(fs::path(dirTmp, 'FI-Hyy_concentration_profile_1min.csv'))
fihyy_met <- read.csv(fs::path(dirTmp, 'FI-Hyy_met_1min.csv'))
fihhy_gas <- read.csv(fs::path(dirTmp, 'FLX_FI-Hyy_FLUXNET-CH4_HH_2016-2016_1-1.csv'))

# ----- Your existing data processing code starts here -----
# Replace -9999 with NA in all dataframes
fihyy[fihyy == -9999] <- NA
fihyy_met[fihyy_met == -9999] <- NA
fihhy_gas[fihhy_gas == -9999] <- NA

# Parse Time columns for the first two datasets
fihyy$Time <- ymd_hms(fihyy$Time)
fihyy_met$Time <- ymd_hms(fihyy_met$Time)

# For fihhy_gas, convert from numeric timestamp format
# Assuming TIMESTAMP_START is in YYYYMMDDHHmm format as numeric
fihhy_gas <- fihhy_gas %>%
  mutate(Time = as.POSIXct(as.character(TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC"))

# Add 30-min time column to all dataframes
fihyy <- fihyy %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))
fihyy_met <- fihyy_met %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))
fihhy_gas <- fihhy_gas %>% mutate(Time_30min = floor_date(Time, unit = "30 minutes"))

# Summarize high-frequency data to 30-min
fihyy_agg <- fihyy %>%
  group_by(Time_30min) %>%
  summarise(across(c(CH4_168, CH4_672, CH4_1250, CO2_168, CO2_672, CO2_1250), 
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")

fihyy_met_agg <- fihyy_met %>%
  group_by(Time_30min) %>%
  summarise(across(c(PA, TA_168, TA_672, TA_1250, VPD_168, VPD_672, VPD_1250), 
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")


#####NEED RELATIVE HUMIDITY###
fihhy_gas$esat_Pa = 611.2*exp(17.67*(fihhy_gas$TA_F)/(fihhy_gas$TA_F+243.51))
fihhy_gas$RH_F = 100*((fihhy_gas$esat_Pa-(fihhy_gas$VPD_F*100))/fihhy_gas$esat_Pa)
fihhy_gas$e_Pa = (fihhy_gas$RH_F/100)*fihhy_gas$esat_Pa

# For fihhy_gas, select only relevant columns before aggregating
fihhy_gas_agg <- fihhy_gas %>%
  group_by(Time_30min) %>%
  summarise(
    NEE = mean(NEE, na.rm = TRUE),
    H = mean(H, na.rm = TRUE),
    LE = mean(LE, na.rm = TRUE),
    FCH4 = mean(FCH4, na.rm = TRUE),
    USTAR = mean(USTAR, na.rm = TRUE),
    TA = mean(TA_F, na.rm = TRUE),  # Using TA_F as it appears to have values
    P_kPa = mean(PA_F, na.rm = TRUE), #met values can be infilled,  probably from another station nearby
    VPD = mean(VPD_F, na.rm = TRUE),  # Using VPD_F as it appears to have values
    PPFD_IN = mean(PPFD_IN, na.rm = TRUE),
    WS = mean(WS_F,na.rm=TRUE),
    RH = mean(RH_F,na.rm=TRUE),
    esat_Pa = mean(esat_Pa ,na.rm=TRUE),
    e_Pa = mean(e_Pa,na.rm=TRUE)
    )

# Now join everything by 30-min interval
all_data <- fihyy_agg %>%
  full_join(fihyy_met_agg, by = "Time_30min") %>%
  full_join(fihhy_gas_agg, by = "Time_30min")

# Add suffix to columns from gas data to avoid name conflicts
names(all_data)[names(all_data) == "PA.y"] <- "PA_flux"
names(all_data)[names(all_data) == "PA.x"] <- "PA_met"

# Create a simple attribute data frame with tower heights for this site
# This is a minimal version just to maintain the expected format
tower.heights <- data.frame(
  TowerPosition = c(1, 2, 3),
  TowerHeight = c(16.8, 67.2, 125.0)
)

# Create a minimal attribute data frame
attr.df <- data.frame(
  DistZaxsLvlMeasTow = c(16.8, 67.2, 125.0),
  TowerPosition = c(1, 2, 3),
  SiteID = rep("FI-Hyy", 3)
)


#######################ARE THE CONCS IN WET MOLE FRACTION?######################
#############CODE FOR CONVERTING TO DRY MOLE FRACTION GOES HERE#################


# Filter down to where we have CH4 flux
all_data <- all_data  %>% dplyr::filter(!is.na(FCH4))


#Assuming the time is representative of the end of the averaging period
all_data$timeEnd <- as.POSIXct(all_data$Time_30min, format="%Y-%m-%d %H:%M:%S", tz="EET")
all_data$timeEnd <- with_tz(all_data$timeEnd, tzone = "UTC")
all_data$timeBgn <- all_data$timeEnd - minutes(30)



# Initialize output. Prepopulate things that won't change with different tower pairs
# Make sure to check metadata for consistent sensor heights among the WS, TA, and concentration profiles
numData <- nrow(all_data)
dmmyNum <- as.numeric(NA)
dmmyChr <- as.character(NA)
dmmyOut <- data.frame(timeEnd_A=all_data$timeEnd,
                      timeBgn_A=all_data$timeBgn,
                      TowerPosition_A=dmmyNum,
                      mean_A=dmmyNum,
                      qfFinl_A=dmmyNum,
                      min_A=dmmyNum,
                      max_A=dmmyNum,
                      vari_A=0,
                      numSamp_A=dmmyNum,
                      timeEnd_B=all_data$timeEnd,
                      timeBgn_B=all_data$timeBgn,
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
                      timeMid=all_data$timeBgn+(all_data$timeEnd-all_data$timeBgn)/2,
                      match_time=all_data$timeBgn+(all_data$timeEnd-all_data$timeBgn)/2,
                      TowerHeight_A=dmmyNum,
                      TowerHeight_B=dmmyNum,
                      FC_turb_interp=all_data$NEE, #Does NEE include storage of is it turbulent?
                      FC_stor_interp=dmmyNum,
                      FC_nee_interp=dmmyNum,
                      LE_turb_interp=all_data$LE,
                      LE_stor_interp=dmmyNum,
                      LE_nsae_interp=dmmyNum,
                      H_turb_interp=all_data$H,
                      H_stor_interp=dmmyNum,
                      H_nsae_interp=dmmyNum,
                      ustar_interp=all_data$USTAR,
                      roughLength_interp=dmmyNum,
                      RH=all_data$RH, 
                      P_kPa=all_data$PA,
                      PAR=all_data$PPFD_IN,
                      Tair1=all_data$TA_168,
                      Tair2=all_data$TA_672,
                      Tair3=all_data$TA_1250, # Assuming Tower top
                      ubar3=all_data$WS, # Wind speed presumably at tower top, need to ask for multiple
                      z_veg_aero=dmmyNum,
                      z_displ_calc=dmmyNum,
                      roughLength_calc=dmmyNum,
                      Tair_K=dmmyNum,
                      esat_Pa=all_data$esat_Pa,
                      e_Pa=all_data$e_Pa,
                      VPD=all_data$VPD,
                      rhoa_kgm3=dmmyNum,
                      rhov_kgm3=dmmyNum,
                      rho_kgm3=dmmyNum,
                      specificHumidity_pct=dmmyNum,
                      Cp_moist=dmmyNum,
                      Ftemp=dmmyNum,
                      lambda=dmmyNum,
                      FH2O_interp=dmmyNum,
                      FCH4_turb_interp=all_data$FCH4, # New variable, since we have measured CH4 fluxes
                      FCH4_stor_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      FCH4_nsae_interp=dmmyNum, # New variable, since we have measured CH4 fluxes
                      stringsAsFactors=FALSE)

# Put concentrations into the output. This task is simple for this site since
# concentrations and fluxes have already been aggregated to 30-min intervals 


# Put concentrations into the output. This task is simple for this site since
# concentrations and fluxes have already been aggregated to 30-min intervals 


# CH4 concentrations
# Tower levels 3-2
CH4_32 <- dmmyOut
CH4_32$TowerPosition_A=tower.heights$TowerPosition[3]
CH4_32$TowerHeight_A=tower.heights$TowerHeight[3]
CH4_32$mean_A=all_data$CH4_1250 # AMF convention increments from top down
CH4_32$TowerPosition_B=tower.heights$TowerPosition[2]
CH4_32$TowerHeight_B=tower.heights$TowerHeight[2]
CH4_32$mean_B=all_data$CH4_672 # AMF convention increments from top down

# Tower levels 2-1
CH4_21 <- dmmyOut
CH4_21$TowerPosition_A=tower.heights$TowerPosition[2]
CH4_21$TowerHeight_A=tower.heights$TowerHeight[2]
CH4_21$mean_A=all_data$CH4_672 # AMF convention increments from top down
CH4_21$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_21$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_21$mean_B=all_data$CH4_168 # AMF convention increments from top down

# Tower levels 3-1
CH4_31 <- dmmyOut
CH4_31$TowerPosition_A=tower.heights$TowerPosition[3]
CH4_31$TowerHeight_A=tower.heights$TowerHeight[3]
CH4_31$mean_A=all_data$CH4_1250 # AMF convention increments from top down
CH4_31$TowerPosition_B=tower.heights$TowerPosition[1]
CH4_31$TowerHeight_B=tower.heights$TowerHeight[1]
CH4_31$mean_B=all_data$CH4_168 # AMF convention increments from top down

# Combine all combos of CH4 paired levels
CH4out <- rbind(CH4_32,CH4_21,CH4_31)
rm(CH4_32,CH4_21,CH4_31)


# CO2 concentrations
# Tower levels 3-2
CO2_32 <- dmmyOut
CO2_32$TowerPosition_A=tower.heights$TowerPosition[3]
CO2_32$TowerHeight_A=tower.heights$TowerHeight[3]
CO2_32$mean_A=all_data$CO2_1250 # AMF convention increments from top down
CO2_32$TowerPosition_B=tower.heights$TowerPosition[2]
CO2_32$TowerHeight_B=tower.heights$TowerHeight[2]
CO2_32$mean_B=all_data$CO2_672 # AMF convention increments from top down

# Tower levels 2-1
CO2_21 <- dmmyOut
CO2_21$TowerPosition_A=tower.heights$TowerPosition[2]
CO2_21$TowerHeight_A=tower.heights$TowerHeight[2]
CO2_21$mean_A=all_data$CO2_672 # AMF convention increments from top down
CO2_21$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_21$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_21$mean_B=all_data$CO2_168 # AMF convention increments from top down

# Tower levels 3-1
CO2_31 <- dmmyOut
CO2_31$TowerPosition_A=tower.heights$TowerPosition[3]
CO2_31$TowerHeight_A=tower.heights$TowerHeight[3]
CO2_31$mean_A=all_data$CO2_1250 # AMF convention increments from top down
CO2_31$TowerPosition_B=tower.heights$TowerPosition[1]
CO2_31$TowerHeight_B=tower.heights$TowerHeight[1]
CO2_31$mean_B=all_data$CO2_168 # AMF convention increments from top down

# Combine all combos of CO2 paired levels
CO2out <- rbind(CO2_32,CO2_21,CO2_31)
rm(CO2_32,CO2_21,CO2_31)

#No h2O concentrations

# Combine all gases into a single data frame
min9Diff.list <- list(CH4=CH4out,CO2=CO2out)
rm(CH4out,CO2out)

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
lvlTow <- 3
hgtMax <- tower.heights$TowerHeight[tower.heights$TowerPosition == lvlTow]
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  var$z_veg_aero <- 10*as.numeric(hgtMax)/(exp(0.4*var[[paste0('ubar',lvlTow)]]/var$ustar_interp)+6.6) # m - aerodynamic vegetation height
  var$z_displ_calc <- 0.66*var$z_veg_aero # m - zero plane displacement height
  var$roughLength_calc <- 0.1*var$z_veg_aero # m - roughness length
  var$roughLength_interp <- var$roughLength_calc # set them the same, since not provided by PI
  return(var)
})
#####The wind speeds might cause a problem here, is WS actually at tower top? 
#Is it too high?



# -------------- Compute water flux from LE --------------------
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  
  # Grab Tair at tower top
  Tair_C <- var[[paste0('Tair',lvlTow)]]
  
  # Variables to calculate air physics vars & convert H and LE to flux
  P_pa = var$P_kPa*100  #Atmospheric pressure [Pa] was actually hectopascal
  ma = 28.964/1000    #molar mass of dry air [Kg/mol]
  mv = 18/1000        #molar mass of water vapor [Kg/mol]
  R = 8.314          #Universal gas constant dry air [J/(K mol)]
  Cpa_dry = 1004.67  #J Kg-1 K-1 - specific heat of dry air
  
  # Save these variables for aerodynamic method later on
  var$Tair_K = Tair_C + 273.16
  #var$esat_Pa = 611.2*exp(17.67*(var$Tair_K-273.16)/(var$Tair_K-29.65)) #[Pa] saturated water vapor pressure
  #var$e_Pa = var$RH*var$esat_Pa/100 #[Pa] vapor water pressure
  #var$VPD = (var$esat_Pa-var$e_Pa)/1000
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
  # Monin-Obukhov length & stability parameter (z/L)
  var$L_obukhov <- calc.MO.length(var$P_kPa, Tair_C, var$H_turb_interp, var$LE_turb_interp, var$ustar_interp)$L
  var$zoL <- as.numeric(attr.df$DistZaxsTow[1])/var$L_obukhov
  return(var)
})



# ----- Your existing data processing code ends here -----

# Save results to Google Drive
# Save the processed data
fileSave <- fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.RData'))
fileZip <- fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.zip'))
save(all_data, file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip, files=paste0(site, '_aligned_conc_flux_9min.RData'))
setwd(wdPrev)

# Save in same folder as NEON site data
drive_url_NEONSiteData <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folderUpld <- googledrive::drive_ls(path = drive_url_NEONSiteData)

# Check if folder for this site exists, create if not
site_exists <- any(data_folderUpld$name == site)
if(!site_exists) {
  site_folder <- googledrive::drive_mkdir(site, path = drive_url_NEONSiteData)
  site_folder_id <- site_folder$id
} else {
  site_folder_id <- data_folderUpld$id[data_folderUpld$name == site]
}

# Upload the data file
googledrive::drive_upload(
  media = fileZip, 
  path = site_folder_id, 
  name = paste0(site, '_aligned_conc_flux_9min.zip'),
  overwrite = TRUE
) 

# Save the attribute data
pathSaveAttr <- fs::path(dirTmp, "data", site)
dir.create(pathSaveAttr, recursive = TRUE)
fileSaveAttr <- fs::path(pathSaveAttr, paste0(site, '_attr.RData'))
fileZipAttr <- fs::path(dirTmp, paste0(site, '_attr.zip'))
save(attr.df, file=fileSaveAttr)

# Zip and upload the attribute file
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZipAttr, files=fs::path("data", site, paste0(site, '_attr.RData')))
setwd(wdPrev)

googledrive::drive_upload(
  media = fileZipAttr, 
  path = site_folder_id,
  name = paste0(site, '_attr.zip'),
  overwrite = TRUE
)

# Print confirmation
cat("Processing complete. Files uploaded to Google Drive:\n")
cat(paste0("1. ", site, "_aligned_conc_flux_9min.zip\n"))
cat(paste0("2. ", site, "_attr.zip\n"))