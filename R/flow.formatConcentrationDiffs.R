# Pull data from google drive
#email <- 'csturtevant@battelleecology.org'
email <- 'jaclyn_matthes@g.harvard.edu'
site <- 'KONZ'

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
fileIn <- fs::path(dirTmp,paste0(site,'_9m.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,paste0(site,'_30m.Rdata'))
load(fileIn)

fileIn <- fs::path(dirTmp,paste0(site,'_1m.Rdata'))
load(fileIn)

# ------------------- Get concentration diffs for subsequent tower levels --------------
# For each concentration, compute difference in concentation among tower levels
# m9.list <- list(Cont=list(CH4=CH4))
list.idx = seq_len(length(m9.list))
m9Diff.list <- lapply(list.idx,FUN=function(idx){
  var = m9.list[[idx]]
  scalar = names(m9.list)[idx]
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
names(m9Diff.list) = names(m9.list)

# ----- Interpolate the 30-min flux data to 9 min concentration midpoints ------
source('./R/interp_fluxes.R')

# FC
timeBgn <- as.POSIXct(strptime(m30.list$F_co2$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(m30.list$F_co2$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- m30.list$F_co2$turb
qf <- m30.list$F_co2$turb.qfFinl # filter
flux[qf == 1] <- NA
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$FC_interp <- fluxPred
  return(var)
})

# LE
timeBgn <- as.POSIXct(strptime(m30.list$F_LE$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(m30.list$F_LE$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- m30.list$F_LE$turb
qf <- m30.list$F_LE$turb.qfFinl # filter
flux[qf == 1] <- NA
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$LE_interp <- fluxPred
  return(var)
})

# H
timeBgn <- as.POSIXct(strptime(m30.list$F_H$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(m30.list$F_H$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- m30.list$F_H$turb
qf <- m30.list$F_H$turb.qfFinl # filter
flux[qf == 1] <- NA
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$H_interp <- fluxPred
  return(var)
})

# ustar & roughness length
timeBgn <- as.POSIXct(strptime(m30.list$Ufric$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(m30.list$Ufric$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
ustar <- m30.list$Ufric$veloFric
roughLength <- m30.list$MomRough$distZaxsRgh
qf <- m30.list$Ufric$qfFinl # filter
ustar[qf == 1] <- NA
roughLength[qf == 1] <- NA
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  ustarPred <- interp_fluxes(timeBgn,timeEnd,ustar,timePred)
  roughLengthPred <- interp_fluxes(timeBgn,timeEnd,roughLength,timePred)
  var$roughLength_interp <- roughLengthPred
  return(var)
})

# # Aggregate the 1-min RH & pressure on the tower top to the window of paired concentrations
# timeBgn <- as.POSIXct(strptime(m1.list$RH$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
# timeEnd <- as.POSIXct(strptime(m1.list$RH$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
# RH <- m1.list$RH$RHmean
# qf <- m1.list$RH$RHFinalQF # quality flag
# RH[qf == 1] <- NA # filter
# 
# # Aggregate the 1-min RH & pressure on the tower top to the window of paired concentrations
# #timeBgn <- as.POSIXct(strptime(m1.list$RH$startDateTime,format='%Y-%m-%dT%H:%M:%OZS',tz='GMT'))
# timeBgn <- m1.list$RH$startDateTime
# timeEnd <- m1.list$RH$endDateTime

RH <- m1.list$RH$RHMean
qf <- m1.list$RH$RHFinalQF # quality flag
RH[qf == 1] <- NA # filter

P_kPa <- m1.list$Press$mean
qf <- m1.list$Press$qfFinl # quality flag
P_kPa[qf == 1] <- NA # filter

Tair_C <- m1.list$Tair$mean
qf <- m1.list$Tair$qfFinl # quality flag
Tair_C[qf == 1] <- NA # filter

# REVISIT WHETHER THERE'S A BETTER WAY TO DO THIS: takes a very long time to run
# Average 1-minute met data into the 20-minute intervals that match 
# the concentration differences at adjacent (or top_bottom) levels
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  
  timeAvg_A <- var$timeBgn_A #averaging start time Level A
  timeAvg_B <- var$timeEnd_B #averaging end time Level B
  timeAvg_A41 <- var$timeBgn_B #averaging start time Level A when 4_1
  timeAvg_B41 <- var$timeEnd_A #averaging start time Level B when 4_1
  
  RH_avg <- P_kPa_avg <- Tair_C_avg <- vector()
  for(i in 1:length(timeAvg_A)){
    print(i)
    if(var$dLevelsAminusB[i] != "4_1"){ # times seq for all non-4_1 level diff
      RH_avg[i] <- mean(RH[timeBgn >= timeAvg_A[i] & timeEnd <= timeAvg_B[i]], na.rm=T)
      P_kPa_avg[i] <- mean(P_kPa[timeBgn >= timeAvg_A[i] & timeEnd <= timeAvg_B[i]], na.rm=T)
      Tair_C_avg[i] <- mean(Tair_C[timeBgn >= timeAvg_A[i] & timeEnd <= timeAvg_B[i]], na.rm=T)
    } else { # times swapped (B then A) for the 4_1 level diff
      RH_avg[i] <- mean(RH[timeBgn >= timeAvg_A41[i] & timeEnd <= timeAvg_B41[i]], na.rm=T)
      P_kPa_avg[i] <- mean(P_kPa[timeBgn >= timeAvg_A41[i] & timeEnd <= timeAvg_B41[i]], na.rm=T)
      Tair_C_avg[i] <- mean(Tair_C[timeBgn >= timeAvg_A41[i] & timeEnd <= timeAvg_B41[i]], na.rm=T)
    }
  }
  # Store 1 min averaged RH, P (kPa), Tair (C) in 9 min table
  #m9Diff.list[["CO2"]]$RH = RH_avg
  var$RH <- RH_avg
  var$P_kPa <- P_kPa_avg
  var$Tair_C <- Tair_C_avg
  
  # Variables to calculate air physics vars & convert H and LE to flux
  P_pa = P_kPa_avg*1000  #Atmospheric pressure [Pa]
  ma = 28.964/1000    #molar mass of dry air [Kg/mol]
  mv = 18/1000        #molar mass of water vapor [Kg/mol]
  R = 8.314          #Universal gas constant dry air [J/(K mol)]
  Cpa_dry = 1004.67  #J Kg-1 K-1 - specific heat of dry air
  
  # Save these variables for aerodynamic method later on
  var$Tair_K = Tair_C_avg + 273.16
  var$esat_Pa = 611.2*exp(17.67*(var$Tair_K-273.16)/(var$Tair_K-29.65)) #[Pa] saturated water vapor pressure
  var$e_Pa = RH_avg*var$esat_Pa/100 #[Pa] vapor water pressure
  var$VPD = (var$esat_Pa-var$e_Pa)/1000
  var$rhoa_kgm3 = ma*(P_pa - var$e_Pa)/(R*var$Tair_K) # dry air density  [Kg/m3]
  var$rhov_kgm3 = mv*var$e_Pa/(R*var$Tair_K) # water vapor density  [Kg/m3]
  var$rho_kgm3 = var$rhoa_kgm3 + var$rhov_kgm3 # moist air density  [Kg/m3]
  var$specificHumidity_pct = var$rhov_kgm3/var$rho_kgm3 # Specific humidity
  var$Cp_moist = Cpa_dry*(1+0.84*var$specificHumidity_pct) # Specific heat of moist air [J kg-1 K-1] 
  
  # Convert H (W m-2) to w't' (Ftemp, K m-2 s-1) (Eqn. 40 in WPL, 1980)
  var$Ftemp = var$H / (var$Cp_moist*var$rho_kgm3) 
  
  # Convert LE (W m-2) to w'q' (FH2O, mmol m-2 s-1)
  # lambda = latent heat of vaporiz. [J/kg] - Eqn in back of Stull pg. 641
  #var$lambda <- (2.501-(2.361*1e-3)*Tair_C_avg)/1e6 # lambda = J kg-1
  var$lambda <- (3149000-2370*(var$Tair_C+273.16))*1e-6 # J g-1
  var$FH2O_interp <- var$LE*(1/var$lambda) 
  
  return(var)
})

#save(m9Diff.list, "KONZ_9minfluxmet.Rdata")

# Aggregate the 1-min ubar profile and tair profile at the window of paired concentations
