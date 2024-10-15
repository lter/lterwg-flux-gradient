# Uses output from flow.neon.data.extract.R from Google Drive. 
#
# Merges together NEON flux, met, and profile concentration data. Aligns the profile 
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
# Based on code from original flow.neon.data.format.conc.diffs.R file 
# that saved output in 9 min format.
#
# Saves output as SITE_aligned_conc_flux_30min.RData, where SITE is the NEON site code. 
# Zips and uploads to Google Drive.

# Pull data from google drive
#email <- 'alexisrose0525@gmail.com'
email <- 'csturtevant@battelleecology.org'
# email <- 'jaclyn_matthes@g.harvard.edu'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: fs, googledrive
library(foreach)
library(doParallel)

# Load functions in this repo
source(file.path("functions/interp.flux.R"))
source(file.path("functions/aggregate_averages.R"))

# Final note: This script takes approx 45 min to run per site. 
# -------------------------------------------------------
sites <- c("BONA","CPER","GUAN","HARV","JORN","KONZ","NIWO","TOOL")
for(site in sites){
  rm(c('min9.list','min30.list','attr.df','min1.list','min9Diff.list'))
  
  # Authenticate with Google Drive and get site data
  googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
  data_folder <- googledrive::drive_ls(path = drive_url)
  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  
  # Only grab the files with the 1- 2- 9- or 30-minute data needed 
  site_minfiles <- site_folder[grep("min", site_folder$name),]
  
  dirTmp <- fs::path(tempdir(),site)
  dir.create(dirTmp)
  
  #for(focal_file in site_folder$name){
  for(focal_file in site_minfiles$name){
    # # Find the file identifier for that file
    # file_id <- subset(site_folder, name == focal_file)
    file_id <- subset(site_minfiles, name == focal_file)
    # 
    # Download that file
    pathDnld <- fs::path(dirTmp,focal_file)
    # googledrive::drive_download(file = file_id$id, 
    #                             path = pathDnld,
    #                             overwrite = T)
    googledrive::drive_download(file = file_id$id, 
                                path = pathDnld,
                                overwrite = T)
    # Unzip
    if(grepl(pattern='.zip',focal_file)){
      utils::unzip(pathDnld,exdir=dirTmp)
    }
    
  }
  
  # Download site attribute table
  site_attrfiles <- site_folder[grep("attr", site_folder$name),]
  pathDnld <- fs::path(dirTmp,site_attrfiles$name)
  googledrive::drive_download(file = site_attrfiles$id, 
                              path = pathDnld, overwrite = T)
  if(grepl(pattern='.zip',site_attrfiles$name)){
    utils::unzip(pathDnld,exdir=dirTmp)
  }
  
  # Extract data in 1, 2, 9, and 30 min & attribute files
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
  min30Diff.list <- lapply(list.idx,FUN=function(idx){
    var = min9.list[[idx]]
    scalar = names(min9.list)[idx]
    var$timeBgn <- as.POSIXct(strptime(var$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
    var$timeEnd <- as.POSIXct(strptime(var$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
    var <- dplyr::arrange(var,timeBgn)
    var$TowerPosition <- as.numeric(var$TowerPosition)
    
    # Without any filtering, this provides concentration diff for each adjacent tower level AND the top/bottom. 
    # Do any filtering desired to restrict to particular tower levels (e.g. 1 & 3 to get that diff)
    
    # Create output data frame where conc diffs are in the same row
    # Loop through the combinations of levels. This is accomplished by incrementing the 
    # row difference up to N/2 (rounding down), where N is the number of tower levels.
    # This ensures that we have every combination of tower levels, while restricting the
    # data to when those two tower levels are measured closest in time
    for (diffRowIdx in seq_len(floor(max(attr.df$LvlMeasTow,na.rm=TRUE)/2))){
      OUTidx <- var
      vars <- names(OUTidx)
      ncol <- ncol(OUTidx)
      nrow <- nrow(OUTidx)
      OUTidx[1:(nrow-diffRowIdx),(ncol+1):(ncol*2)] <- OUTidx[(1+diffRowIdx):(nrow),]
      OUTidx <- OUTidx[1:(nrow-diffRowIdx),] #last row removed because becomes NA
      
      names(OUTidx)[1:ncol] <- paste0(vars,'_A')
      names(OUTidx)[(ncol+1):(ncol*2)] <- paste0(vars,'_B')
      
      # Combine results
      if(diffRowIdx == 1){
        OUT <- OUTidx
      } else {
        OUT <- rbind(OUT,OUTidx)
      }
    }

    #YOU MUST RUN THIS BEFORE CHANGING THE TOWER POSITIONS OTHERWISE YOU WILL END UP REMOVING COMBINATION OF top-bottom levels
    # Apply quality control
    timeChk <- difftime(OUT$timeBgn_B, OUT$timeEnd_A, units = "secs") # looking for short positive lag (flush time is 1 min for CH4 but 3 min for CO2 and H2O)
    bad <- timeChk < 45 | timeChk > 3600 | OUT$qfFinl_A == 1 | OUT$qfFinl_B == 1
    OUT <- OUT[!bad,]
    
    # Compute tower level diff A-B
    # Swap any in which the diff is negative bc we always want higher level minus lower level
    #Using A as "top" and B as "bottom"
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
    
    OUT$timeMid <- OUT$timeEnd_A+(0.5*difftime(OUT$timeBgn_B, OUT$timeEnd_A, units = "secs"))
    
    # Make match column for CH4 & CO2/H2O
    if(scalar == "CH4"){
      #####CHECK THIS WITH DIFFERENT TOWERS
      OUT$match_time <- OUT$timeMid + (1.5*60) # CO2 & H2O starts 3 min past CH4 so need to add 1.5 min so that timeMid aligns across gas concentrations, multiply by 60 to convert min to sec
    } else {
      OUT$match_time <- OUT$timeMid 
    }
    
    #Add in tower heights from attribute data frame to gas concentrations data frames
    #grab only tower heights and positions for matching
    tower.heights <- as.data.frame(cbind(attr.df[,4], attr.df[,17]))
    names(tower.heights) <- c("TowerHeight", "TowerPosition")
    #add tower height to data frame
    for(height in 1:dim(tower.heights)[1]){
      #loop over position A
      OUT[which(OUT$TowerPosition_A == height),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == height),1]
      #loop over position B
      OUT[which(OUT$TowerPosition_B == height),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == height),1]
    }
    
    return(OUT)
  })
  
  # Reassign names from original list
  names(min30Diff.list) = names(min9.list)
  
  # ----- Align the 9-min conc diff data to 30 min eddy flux values ------
  message(paste0(Sys.time(),': Interpolating fluxes to midpoint of each paired profile window...'))
  
  # carbon flux
  turb <- min30.list$F_co2$turb
  qf <- min30.list$F_co2$turb.qfFinl ; turb[qf == 1] <- NA
  stor <- min30.list$F_co2$stor
  qf <- min30.list$F_co2$stor.qfFinl ; stor[qf == 1] <- NA
  nee <- min30.list$F_co2$nsae
  qf <- min30.list$F_co2$nsae.qfFinl ; nee[qf == 1] <- NA
  FCdf = data.frame(datetime = as.POSIXct((as.numeric(as.POSIXct(strptime(min30.list$F_co2$timeBgn, 
                                                                          format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))) +
                                             as.numeric(as.POSIXct(strptime(min30.list$F_co2$timeEnd, 
                                                                            format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))))/2,
                                          origin = '1970-01-01',tz='GMT'),
                    FC_turb_interp = turb,FC_stor_interp = stor, FC_nee_interp = nee)
  
  # water flux
  turb <- min30.list$F_LE$turb
  qf <- min30.list$F_LE$turb.qfFinl ; turb[qf == 1] <- NA
  stor <- min30.list$F_LE$stor
  qf <- min30.list$F_LE$stor.qfFinl ; stor[qf == 1] <- NA
  nsae <- min30.list$F_LE$nsae
  qf <- min30.list$F_LE$nsae.qfFinl ; nsae[qf == 1] <- NA
  FLEdf = data.frame(datetime = as.POSIXct((as.numeric(as.POSIXct(strptime(min30.list$F_LE$timeBgn, 
                                                                           format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))) +
                                              as.numeric(as.POSIXct(strptime(min30.list$F_LE$timeEnd, 
                                                                             format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))))/2,
                                           origin = '1970-01-01',tz='GMT'),
                     LE_turb_interp = turb,LE_stor_interp = stor, LE_nsae_interp = nsae)
  
  # heat flux
  turb <- min30.list$F_H$turb
  qf <- min30.list$F_H$turb.qfFinl ; turb[qf == 1] <- NA
  stor <- min30.list$F_H$stor
  qf <- min30.list$F_H$stor.qfFinl ; stor[qf == 1] <- NA
  nsae <- min30.list$F_H$nsae
  qf <- min30.list$F_H$nsae.qfFinl ; nsae[qf == 1] <- NA
  FHdf = data.frame(datetime = as.POSIXct((as.numeric(as.POSIXct(strptime(min30.list$F_H$timeBgn, 
                                                                          format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))) +
                                             as.numeric(as.POSIXct(strptime(min30.list$F_H$timeEnd, 
                                                                            format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))))/2,
                                          origin = '1970-01-01',tz='GMT'),
                    H_turb_interp = turb, H_stor_interp = stor, H_nsae_interp = nsae)
  
  # turbulence vars
  ustar <- min30.list$Ufric$veloFric
  roughLength <- min30.list$FluxFoot$distZaxsRgh
  qf <- min30.list$Ufric$qfFinl # filter
  ustar[qf == 1] <- NA
  roughLength[qf==1] <- NA
  turbdf = data.frame(datetime = as.POSIXct((as.numeric(as.POSIXct(strptime(min30.list$Ufric$timeBgn, 
                                                                            format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))) +
                                               as.numeric(as.POSIXct(strptime(min30.list$Ufric$timeEnd, 
                                                                              format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))))/2,
                                            origin = '1970-01-01',tz='GMT'),
                      ustar_interp = ustar, roughLength_interp = roughLength)
  
  # Join data.table objects with datetime key
  data.table::setDT(turbdf); data.table::setDT(FHdf)
  data.table::setDT(FLEdf); data.table::setDT(FCdf)
  data.table::setkey(turbdf, datetime); data.table::setkey(FHdf, datetime)
  data.table::setkey(FLEdf, datetime); data.table::setkey(FCdf, datetime)
  
  FC_LE <- FCdf[FLEdf, roll = "nearest" ]
  FH_turb <- FHdf[turbdf, roll = "nearest" ]
  flux_30 <- FC_LE[FH_turb, roll = "nearest"]
  
  # Align two top tower dConc values to 30-min flux data
  # by assigning to the nearest flux half-hour interval
  min30Diff.list <- lapply(min30Diff.list,FUN=function(var){
    var$datetime <- var$match_time
    var <- var[as.numeric(var$TowerHeight_A) == max(as.numeric(var$TowerHeight_A)) &
                 as.numeric(var$TowerHeight_B) != min(as.numeric(var$TowerHeight_B)),]
    data.table::setDT(var)
    data.table::setkey(var, datetime)
    var <- var[flux_30, roll = "nearest"]
    tdiff = abs(var$datetime - var$match_time)/60 # time diff between flux & dConc
    var$dConc[tdiff > 30] = NA # remove if diff between flux & dConc is > 30 min
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
  
  # PAR (round times to minute mark)
  timeBgn <- round(as.POSIXct(min1.list$PAR$startDateTime),units='mins')
  timeEnd <- round(as.POSIXct(min1.list$PAR$endDateTime),units='mins')
  PAR <- min1.list$PAR$PARMean
  qf <- min1.list$PAR$PARFinalQF # quality flag
  PAR[qf == 1] <- NA # filter
  PAR_1min <- data.frame(timeBgn=timeBgn,
                         timeEnd=timeEnd,
                         PAR=PAR)
  
  # Merge the 1-min MET data frames and clear space
  MET_1min <- base::merge(RH_1min,P_kPa_1min,all=TRUE)
  MET_1min <- base::merge(MET_1min,PAR_1min,all=TRUE)
  rm('RH_1min','RH','qf','P_kPa_1min','P_kPa','PAR_1min','PAR')
  
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
  
  #for debugging PAR NAs; currently showing up in MET_agr for all 3 gas concentrations
  #idxDf = "CH4"
  
  MET_agr_list <- foreach::foreach(idxDf = names(min30Diff.list)) %dopar% {
    message(paste0(Sys.time(),': Aggregating 1-min MET data to 30 min flux windows for ',idxDf,' data frame. This will take a while...'))
    
    # use the 30-min flux window to anchor the met aggregation
    timeAgrBgn <- min30Diff.list[[idxDf]]$datetime-(15*60-1)
    timeAgrEnd <- min30Diff.list[[idxDf]]$datetime+(15*60+1)
    
    # Aggregate the 1-min data to the combined window of the profile pair
    MET_agr <- aggregate_averages(timeBgn=MET_1min$timeBgn,
                                  timeEnd=MET_1min$timeEnd,
                                  meas=MET_1min[,-c(1,2)],
                                  timeAgrBgn=timeAgrBgn,
                                  timeAgrEnd=timeAgrEnd,
                                  na.rm=TRUE)
    
  }
  
  message(paste0(Sys.time(),': Aggregating 2-min ubar data to each paired profile window. This should take 5-10 min...'))
  
  ubar_agr_list <- foreach::foreach(idxDf = names(min30Diff.list)) %dopar% {
    message(paste0(Sys.time(),': Aggregating 2-min ubar data to paired profile windows for ',idxDf,' data frame. This will take a while...'))
    
    # use the 30-min flux window to anchor the met aggregation
    timeAgrBgn <- min30Diff.list[[idxDf]]$datetime-(15*60-1)
    timeAgrEnd <- min30Diff.list[[idxDf]]$datetime+(15*60+1)
    
    # Aggregate the 1-min data to the combined window of the profile pair
    ubar_agr <- aggregate_averages(timeBgn=ubar_2min$timeBgn,
                                   timeEnd=ubar_2min$timeEnd,
                                   meas=ubar_2min[,-c(1,2)],
                                   timeAgrBgn=timeAgrBgn,
                                   timeAgrEnd=timeAgrEnd,
                                   na.rm=TRUE)
    
  }
  
  # Add aggregated output to the concentration diffs
  nameGas <- names(min30Diff.list)
  for (idxGas in 1:length(nameGas)){
    min30Diff.list[[nameGas[idxGas]]] <- cbind(min30Diff.list[[nameGas[idxGas]]],MET_agr_list[[idxGas]],ubar_agr_list[[idxGas]])
  }
  rm('MET_agr_list','ubar_agr_list')
  
  # Compute vegetation height based on turbulence measurements
  # These equations stem from Eqn. 9.7.1b in Stull
  min30Diff.list <- lapply(min30Diff.list,FUN=function(var){
    var$z_veg_aero <- 10*as.numeric(attr.df$DistZaxsLvlMeasTow[attr.df$TowerPosition == lvlTow])/(exp(0.4*var[[paste0('ubar',lvlTow)]]/var$ustar_interp)+6.6) # m - aerodynamic vegetation height
    var$z_displ_calc <- 0.66*var$z_veg_aero # m - zero plane displacement height
    var$roughLength_calc <- 0.1*var$z_veg_aero # m - roughness length
    return(var)
  })
  
  
  # -------------- Compute water flux from LE --------------------
  min30Diff.list <- lapply(min30Diff.list,FUN=function(var){
    
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
    # lambda = latent heat of vaporiz. [J/kg] - Eqn in back of Stull pg. 641
    #var$lambda <- (3149000-2370*(Tair_C+273.16))*1e-6 # J g-1
    #var$FH2O_interp <- var$LE_interp*(1/var$lambda) 
    var$lambda <- (2.501-0.00237*Tair_C)*1E6 # lambda = J kg-1 Eqn in back of Stull pg. 641
    var$FH2O_interp <- var$LE_turb_interp/var$lambda/mv*1000 # mmol m-2 s-1
    var = as.data.frame(var) 
    return(var)
  })
  
  min30Diff.list$CH4 = as.data.frame(min30Diff.list$CH4)
  min30Diff.list$CO2 = as.data.frame(min30Diff.list$CO2)
  min30Diff.list$H2O = as.data.frame(min30Diff.list$H2O)
  
  # -------- Save and zip the file to the temp directory. Upload to google drive. -------
  fileSave <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_30min.RData'))
  fileZip <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_30min.zip'))
  save(min30Diff.list,file=fileSave)
  wdPrev <- getwd()
  setwd(dirTmp)
  utils::zip(zipfile=fileZip,files=paste0(site,'_aligned_conc_flux_30min.RData'))
  setwd(wdPrev)
  googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
}
