# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
site <- 'KONZ'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: fs, googledrive
library(foreach)
library(doParallel)

# Load functions in this repo
source('./R/interp_fluxes.R')
source('./R/aggregate_averages.R')

# Final note: This script takes approx 45 min to run per site. 
# -------------------------------------------------------

# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email)
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)
#focal_file = "KONZ_30m.zip"
for(focal_file in site_folder$name){
  
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

# Extract 9 minute data
fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_9min.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_30min.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_1min.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_WS2D2min.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_attr.Rdata'))
load(fileIn)

# ------------------- Get concentration diffs for subsequent tower levels --------------
message(paste0(Sys.time(),': Computing concentration profile differences among subsequent levels...'))

# For each concentration, compute difference in concentration among tower levels
list.idx = seq_len(length(min9.list))
min9Diff.list <- lapply(list.idx,FUN=function(idx){
  var = min9.list[[idx]]
  scalar = names(min9.list)[idx]
  var$timeBgn <- as.POSIXct(strptime(var$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  var$timeEnd <- as.POSIXct(strptime(var$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  var <- dplyr::arrange(var,timeBgn)
  var$TowerPosition <- as.numeric(var$TowerPosition)
  
  # Without any filtering, this provides concentration diff for each adjacent tower level AND the top/bottom. 
  # Do any filtering desired to restrict to particular tower levels (e.g. 1 & 3 to get that diff)
  
  # Create output data frame where conc diffs are in the same row
  OUT <- var
  var <- names(OUT)
  ncol <- ncol(OUT)
  nrow <- nrow(OUT)
  OUT[1:(nrow-1),(ncol+1):(ncol*2)] <- OUT[2:(nrow),]
  OUT <- OUT[1:(nrow-1),]
  
  names(OUT)[1:ncol] <- paste0(var,'_A')
  names(OUT)[(ncol+1):(ncol*2)] <- paste0(var,'_B')
  
  # Apply quality control
  timeChk <- OUT$timeBgn_B-OUT$timeEnd_A # looking for short positive lag (flush time is 1 min)
  if(scalar == "CH4"){
    bad <- timeChk < 45 | timeChk > 100 | OUT$qfFinl_A == 1 | OUT$qfFinl_B == 1
  } else {
    bad <- timeChk < 200 | timeChk > 300 | OUT$qfFinl_A == 1 | OUT$qfFinl_B == 1
  }
  OUT <- OUT[!bad,]
  
  # Compute tower level diff A-B
  # Swap any in which the diff is negative bc we always want higher level minus lower level
  diffTowerPosition<- OUT$TowerPosition_A-OUT$TowerPosition_B
  idxNeg <- diffTowerPosition < 0
  A <- OUT[idxNeg,1:ncol]
  B <- OUT[idxNeg,(ncol+1):(ncol*2)]
  OUT[idxNeg,1:ncol] <- B
  OUT[idxNeg,(ncol+1):(ncol*2)] <- A
  OUT$diffTowerPosition<- OUT$TowerPosition_A-OUT$TowerPosition_B
  OUT$dLevelsAminusB <- paste0(OUT$TowerPosition_A,'_',OUT$TowerPosition_B)
  
  # Compute concentration diffs
  OUT$dConc <- OUT$mean_A-OUT$mean_B
  OUT$timeMid <- OUT$timeEnd_A+0.5*(OUT$timeBgn_B-OUT$timeEnd_A)
  
  # Make match column for CH4 & CO2/H2O
  if(scalar == "CH4"){
    #####CHECK THIS WITH DIFFERENT TOWERS
    OUT$match_time <- OUT$timeMid + 1.5*60 # CO2 & H2O starts 1.5 min past CH4
  } else {
    OUT$match_time <- OUT$timeMid 
  }
  return(OUT)
})

# Reassign names from original list
names(min9Diff.list) = names(min9.list)

# ----- Interpolate the 30-min flux data to 9 min concentration midpoints ------
message(paste0(Sys.time(),': Interpolating fluxes to midpoint of each paired profile window...'))

# FC
timeBgn <- as.POSIXct(strptime(min30.list$F_co2$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(min30.list$F_co2$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- min30.list$F_co2$turb
qf <- min30.list$F_co2$turb.qfFinl # filter
flux[qf == 1] <- NA
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$FC_interp <- fluxPred
  return(var)
})

# LE
timeBgn <- as.POSIXct(strptime(min30.list$F_LE$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(min30.list$F_LE$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- min30.list$F_LE$turb
qf <- min30.list$F_LE$turb.qfFinl # filter
flux[qf == 1] <- NA
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$LE_interp <- fluxPred
  return(var)
})

# H
timeBgn <- as.POSIXct(strptime(min30.list$F_H$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(min30.list$F_H$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- min30.list$F_H$turb
qf <- min30.list$F_H$turb.qfFinl # filter
flux[qf == 1] <- NA
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$H_interp <- fluxPred
  return(var)
})

# ustar & roughness length
timeBgn <- as.POSIXct(strptime(min30.list$Ufric$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(min30.list$Ufric$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
ustar <- min30.list$Ufric$veloFric
roughLength <- min30.list$MomRough$distZaxsRgh
qf <- min30.list$Ufric$qfFinl # filter
ustar[qf == 1] <- NA
roughLength[qf == 1] <- NA
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  ustarPred <- interp_fluxes(timeBgn,timeEnd,ustar,timePred)
  var$ustar_interp <- ustarPred
  roughLengthPred <- interp_fluxes(timeBgn,timeEnd,roughLength,timePred)
  var$roughLength_interp <- roughLengthPred
  return(var)
})

# ------ Aggregate the 1-min MET and 2-min ubar data to each window of paired concentrations -----
numCoreAvail <- parallel::detectCores()
numCoreUse <- numCoreAvail # Adjust as desired
message(paste0(Sys.time(),': ', numCoreUse, ' of ',numCoreAvail, ' available cores will be used for parallelization...'))
doParallel::registerDoParallel(numCoreUse)

# --- First, merge together the 1-min RH, air pressure, and 3D wind data so we can input a single data frame (MUCH faster than doing each individually) 
message(paste0(Sys.time(),': Combining the data frames of the 1-min MET data. This should take 5-10 min...'))

# RH (round times to minute mark)
timeBgn <- round(as.POSIXct(min1.list$RH$startDateTime),units='mins')
timeEnd <- round(as.POSIXct(min1.list$RH$endDateTime),units='mins')
RH <- min1.list$RH$RHMean
qf <- min1.list$RH$RHFinalQF # quality flag
RH[qf == 1] <- NA # filter
RH_1min <- data.frame(timeBgn=timeBgn,
                      timeEnd=timeEnd,
                      RH=RH)

# Air Pressure (round times to minute mark)
timeBgn <- round(as.POSIXct(strptime(min1.list$Press$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
timeEnd <- round(as.POSIXct(strptime(min1.list$Press$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
P_kPa <- min1.list$Press$mean
qf <- min1.list$Press$qfFinl # quality flag
P_kPa[qf == 1] <- NA # filter
P_kPa_1min <- data.frame(timeBgn=timeBgn,
                         timeEnd=timeEnd,
                         P_kPa=P_kPa)

# Merge the 1-min MET data frames and clear space
MET_1min <- base::merge(RH_1min,P_kPa_1min,all=TRUE)
rm('RH_1min','RH','qf','P_kPa_1min','P_kPa')

# 3D wind (round times to minute mark)
lvlTow <- attr.df$LvlMeasTow[1] # how many measurement levels
nameWS3D <- paste0('ubar',lvlTow) # This is what we will eventually name the 3D wind
timeBgn <- round(as.POSIXct(strptime(min1.list$WS3D$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
timeEnd <- round(as.POSIXct(strptime(min1.list$WS3D$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
WS3D <- min1.list$WS3D$mean
qf <- min1.list$WS3D$qfFinl # quality flag
WS3D[qf == 1] <- NA # filter
WS3D_1min <- data.frame(timeBgn=timeBgn,
                        timeEnd=timeEnd,
                        WS3D=WS3D)
names(WS3D_1min)[3] <- nameWS3D

# Merge the 1-min MET data frames and clear space. 
MET_1min <- base::merge(MET_1min,WS3D_1min,all=TRUE)
rm('WS3D_1min','WS3D','qf')

# Tair profile 
timeBgn <- round(as.POSIXct(strptime(min1.list$Tair$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
timeEnd <- round(as.POSIXct(strptime(min1.list$Tair$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')),units='mins')
Tair <- min1.list$Tair$mean
qf <- min1.list$Tair$qfFinl # quality flag
Tair[qf == 1] <- NA # filter
for(idxLvl in 1:(lvlTow)){
  setLvl <- min1.list$Tair$TowerPosition == idxLvl
  Tair_df <- data.frame(timeBgn = timeBgn[setLvl],
                        timeEnd = timeEnd[setLvl],
                        Tair = Tair[setLvl])
  names(Tair_df)[3] <- paste0('Tair',idxLvl)
  
  if(idxLvl == 1){
    Tair_1min <- Tair_df
  } else {
    Tair_1min <- base::merge(Tair_1min,Tair_df,all=TRUE)
  }
}

# Merge the 1-min MET data frames and clear space
MET_1min <- base::merge(MET_1min,Tair_1min,all=TRUE)
rm('Tair_1min','Tair_df','Tair','qf')

# 2-min ubar profile 
timeBgn <- round(as.POSIXct(WS2D2min$startDateTime),units='mins')
timeEnd <- round(as.POSIXct(WS2D2min$endDateTime),units='mins')
ubar <- WS2D2min$windSpeedMean
qf <- WS2D2min$windSpeedFinalQF # quality flag
ubar[qf == 1] <- NA # filter
for(idxLvl in 1:(lvlTow-1)){
  setLvl <- WS2D2min$TowerPosition == idxLvl
  ubar_df <- data.frame(timeBgn = timeBgn[setLvl],
                        timeEnd = timeEnd[setLvl],
                        ubar = ubar[setLvl])
  names(ubar_df)[3] <- paste0('ubar',idxLvl)
  
  if(idxLvl == 1){
    ubar_2min <- ubar_df
  } else {
    ubar_2min <- base::merge(ubar_2min,ubar_df,all=TRUE)
  }
}


# Aggregate!
message(paste0(Sys.time(),': Aggregating 1-min MET data to each paired profile window. This should take 5-10 min...'))

MET_agr_list <- foreach::foreach(idxDf = names(min9Diff.list)) %dopar% {
  message(paste0(Sys.time(),': Aggregating 1-min MET data to paired profile windows for ',idxDf,' data frame. This will take a while...'))

  # Find which level is measured first to get the overall start and end window of the profile pair
  AbeforeB <- min9Diff.list[[idxDf]]$timeBgn_A < min9Diff.list[[idxDf]]$timeBgn_B
  timeAgrBgn <- min9Diff.list[[idxDf]]$timeBgn_B 
  timeAgrBgn[AbeforeB] <- min9Diff.list[[idxDf]]$timeBgn_A[AbeforeB]
  timeAgrEnd <- min9Diff.list[[idxDf]]$timeEnd_A 
  timeAgrEnd[AbeforeB] <- min9Diff.list[[idxDf]]$timeEnd_B[AbeforeB]
  
  # Aggregate the 1-min data to the combined window of the profile pair
  MET_agr <- aggregate_averages(timeBgn=MET_1min$timeBgn,
                                timeEnd=MET_1min$timeEnd,
                                meas=MET_1min[,-c(1,2)],
                                timeAgrBgn=timeAgrBgn,
                                timeAgrEnd=timeAgrEnd,
                                na.rm=TRUE)
  
}

message(paste0(Sys.time(),': Aggregating 2-min ubar data to each paired profile window. This should take 5-10 min...'))

ubar_agr_list <- foreach::foreach(idxDf = names(min9Diff.list)) %dopar% {
  message(paste0(Sys.time(),': Aggregating 2-min ubar data to paired profile windows for ',idxDf,' data frame. This will take a while...'))
  
  # Find which level is measured first to get the overall start and end window of the profile pair
  AbeforeB <- min9Diff.list[[idxDf]]$timeBgn_A < min9Diff.list[[idxDf]]$timeBgn_B
  timeAgrBgn <- min9Diff.list[[idxDf]]$timeBgn_B 
  timeAgrBgn[AbeforeB] <- min9Diff.list[[idxDf]]$timeBgn_A[AbeforeB]
  timeAgrEnd <- min9Diff.list[[idxDf]]$timeEnd_A 
  timeAgrEnd[AbeforeB] <- min9Diff.list[[idxDf]]$timeEnd_B[AbeforeB]
  
  # Aggregate the 1-min data to the combined window of the profile pair
  ubar_agr <- aggregate_averages(timeBgn=ubar_2min$timeBgn,
                                timeEnd=ubar_2min$timeEnd,
                                meas=ubar_2min[,-c(1,2)],
                                timeAgrBgn=timeAgrBgn,
                                timeAgrEnd=timeAgrEnd,
                                na.rm=TRUE)
  
}

# Add aggregated output to the concentration diffs
nameGas <- names(min9Diff.list)
for (idxGas in 1:length(nameGas)){
  min9Diff.list[[nameGas[idxGas]]] <- cbind(min9Diff.list[[nameGas[idxGas]]],MET_agr_list[[idxGas]],ubar_agr_list[[idxGas]])
}
rm('MET_agr_list','ubar_agr_list')

# Compute vegetation height based on turbulence measurements
# These equations stem from Eqn. 9.7.1b in Stull
min9Diff.list <- lapply(min9Diff.list,FUN=function(var){
  var$z_veg_aero <- 10*as.numeric(attr.df$DistZaxsLvlMeasTow[attr.df$TowerPosition == lvlTow])/(exp(0.4*var[[paste0('ubar',lvlTow)]]/var$ustar_interp)+6.6) # m - aerodynamic vegetation height
  var$z_displ_calc <- 0.66*var$z_veg_aero # m - zero plane displacement height
  var$roughLength_calc <- 0.1*var$z_veg_aero # m - roughness length
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
  var$Ftemp = var$H_interp / (var$Cp_moist*var$rho_kgm3) 
  
  # Convert LE (W m-2) to w'q' (FH2O, mmol m-2 s-1)
  # lambda = latent heat of vaporiz. [J/kg] - Eqn in back of Stull pg. 641
  #var$lambda <- (2.501-(2.361*1e-3)*Tair_C_avg)/1e6 # lambda = J kg-1
  var$lambda <- (3149000-2370*(Tair_C+273.16))*1e-6 # J g-1
  var$FH2O_interp <- var$LE_interp*(1/var$lambda) 
  
  return(var)
})







# -------- Save and zip the file to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.zip'))
save(min9Diff.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_aligned_conc_flux_9min.RData'))
setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work