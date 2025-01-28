#' grab.neon.met.flux.30min
#'
#' @param sitecode NEON site code
#' @param hd.file file type h5 containg NEON site specific data
#' @param startdate data start date
#' @param enddate data end date
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
grab.neon.met.flux.30min <- function(hd.file, sitecode, startdate, enddate){
  
  #lists the contents of hdf5 file and returns a df with file contents
  test.df <- h5ls(hd.file,
                  recursive = TRUE,
                  all =TRUE, 
                  datasetinfo = TRUE,
                  index_type = h5default("H5_INDEX"),
                  order = h5default("H5_ITER"), 
                  s3 = FALSE, 
                  s3credentials = NULL,
                  native = FALSE)
  
  #grabs the all unique heights of ch4Conc (name column in test.df = heights)
  #NOTE: getVarEddy fcn returns df where name column = test.df$name, this column identifies which vars can be set in stackEddy fcn. I theorize this fcn might be replaced by stackEddy? Or at the very least improved?
  heights.list <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/ch4Conc", sep="")),]$name)
  #subsets heights list grabbing only those at 30m
  heights <- as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.list, '000')),'30m'),6,6))
  
  #Grab NEON level 1 CH4 cont, CO2 stor, H2O stor for each height
  df_CH4 =data.frame()
  df_CO2 =data.frame()
  df_H2O  =data.frame()
  df_CO2.850 =data.frame()
  df_H2O.850  =data.frame()

  #loop over heights
  for(i in 1:length(heights)){
    #set the gas concentration dataset name to be used by h5read
    ch4.dir <- paste("/", sitecode, '/dp01/data/ch4Conc/000_0',heights[i],'0_30m/rtioMoleDryCh4', sep="")
    co2.dir <- paste("/", sitecode, '/dp01/data/isoCo2/000_0',heights[i], '0_30m/rtioMoleDryCo2', sep="")
    h2o.dir <- paste( "/", sitecode, '/dp01/data/isoCo2/000_0',heights[i], '0_30m/rtioMoleDryH2o', sep="")
    #set the qfqm dataset name to be used by h5read
    ch4.qfqm <- paste("/", sitecode, '/dp01/qfqm/ch4Conc/000_0',heights[i],'0_30m/rtioMoleDryCh4', sep="")
    co2.qfqm <- paste("/", sitecode, '/dp01/qfqm/isoCo2/000_0',heights[i], '0_30m/rtioMoleDryCo2', sep="")
    h2o.qfqm <- paste( "/", sitecode, '/dp01/qfqm/isoCo2/000_0',heights[i], '0_30m/rtioMoleDryH2o', sep="")
    #grabbing 30min from 850 sensor
    co2.850.dir <- paste("/", sitecode, '/dp01/data/co2Stor/000_0',heights[i], '0_30m/rtioMoleDryCo2', sep="")
    h2o.850.dir <- paste("/", sitecode, '/dp01/data/h2oStor/000_0',heights[i], '0_30m/rtioMoleDryH2o', sep="")
    co2.850.qfqm <- paste("/", sitecode, '/dp01/qfqm/co2Stor/000_0',heights[i], '0_30m/rtioMoleDryCo2', sep="")
    h2o.850.qfqm <- paste("/", sitecode, '/dp01/qfqm/h2oStor/000_0',heights[i], '0_30m/rtioMoleDryH2o', sep="")
    #h5read fcn reads a dataset in h5 file and returns an array containing data read
    CH4 <- h5read(hd.file, ch4.dir) %>%
      #select(timeEnd, mean) %>%
      group_by(timeEnd, timeBgn) %>%
      summarize(mean = mean(mean, na.rm = T),
                # value_paste = paste(mean, collapse= "; "),
                # sd_val = sd(mean, na.rm = T),
                duplicate_ct = n(),
                min = mean(min, na.rm = T),
                max = mean(max, na.rm = T),
                vari = mean(vari, na.rm = T),
                numSamp = mean(numSamp, na.rm = T)) %>%
      ungroup()
    #grab qfqm identifier, select desired columns
    CH4.qfqm <- h5read(hd.file, ch4.qfqm) %>%
      #select(timeEnd, qfFinl) %>%
      distinct()
    #join datasets
    CH4.all <- left_join(CH4, CH4.qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights[i]))
    #add to populated df
    df_CH4 <- bind_rows(df_CH4, CH4.all)
    
    #grab CO2 data from g2131
    CO2 <- h5read(hd.file, co2.dir) %>%
      #select(timeEnd, mean) %>%
      group_by(timeEnd, timeBgn) %>%
      summarize(mean = mean(mean, na.rm = T),
                # value_paste = paste(mean, collapse= "; "),
                # sd_val = sd(mean, na.rm = T),
                duplicate_ct = n(),
                min = mean(min, na.rm = T),
                max = mean(max, na.rm = T),
                vari = mean(vari, na.rm = T),
                numSamp = mean(numSamp, na.rm = T)) %>%
      ungroup()
    #grad qfqm identifier
    CO2.qfqm <- h5read(hd.file, co2.qfqm) %>%
      #select(timeEnd, qfFinl) %>%
      distinct()
    
    CO2.all <- left_join(CO2, CO2.qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights[i]))
    
    df_CO2 <- bind_rows(df_CO2, CO2.all)
    
    #grab CO2 data from 850
    CO2.850 <- h5read(hd.file, co2.850.dir) %>%
      #select(timeEnd, mean) %>%
      group_by(timeEnd, timeBgn) %>%
      summarize(mean = mean(mean, na.rm = T),
                # value_paste = paste(mean, collapse= "; "),
                # sd_val = sd(mean, na.rm = T),
                duplicate_ct = n(),
                min = mean(min, na.rm = T),
                max = mean(max, na.rm = T),
                vari = mean(vari, na.rm = T),
                numSamp = mean(numSamp, na.rm = T)) %>%
      ungroup()
    #grad qfqm identifier
    CO2.850.qfqm <- h5read(hd.file, co2.850.qfqm) %>%
      #select(timeEnd, qfFinl) %>%
      distinct()
    
    CO2.850.all <- left_join(CO2.850, CO2.850.qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights[i]))
    
    df_CO2.850 <- bind_rows(df_CO2.850, CO2.850.all)
    
    #grab H2O data from g2131
    H2O <- h5read(hd.file, h2o.dir) %>%
      #select(timeEnd, mean) %>%
      group_by(timeEnd, timeBgn) %>%
      summarize(mean = mean(mean, na.rm = T),
                # value_paste = paste(mean, collapse= "; "),
                # sd_val = sd(mean, na.rm = T),
                duplicate_ct = n(),
                min = mean(min, na.rm = T),
                max = mean(max, na.rm = T),
                vari = mean(vari, na.rm = T),
                numSamp = mean(numSamp, na.rm = T)) %>%
      ungroup()
    #grad qfqm identifier
    H2O.qfqm <- h5read(hd.file, h2o.qfqm) %>%
      #select(timeEnd, qfFinl) %>%
      distinct()
    
    H2O.all <- left_join(H2O, H2O.qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights[i]))
    
    df_H2O <- bind_rows(df_H2O, H2O.all)
    
    #grab H2O data from 850
    H2O.850 <- h5read(hd.file, h2o.850.dir) %>%
      #select(timeEnd, mean) %>%
      group_by(timeEnd, timeBgn) %>%
      summarize(mean = mean(mean, na.rm = T),
                # value_paste = paste(mean, collapse= "; "),
                # sd_val = sd(mean, na.rm = T),
                duplicate_ct = n(),
                min = mean(min, na.rm = T),
                max = mean(max, na.rm = T),
                vari = mean(vari, na.rm = T),
                numSamp = mean(numSamp, na.rm = T)) %>%
      ungroup()
    #grad qfqm identifier
    H2O.850.qfqm <- h5read(hd.file, h2o.850.qfqm) %>%
      #select(timeEnd, qfFinl) %>%
      distinct()
    
    H2O.850.all <- left_join(H2O.850, H2O.850.qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights[i]))
    
    df_H2O.850 <- bind_rows(df_H2O.850, H2O.850.all)
    
  }
  
  #remove looping var and flux df
  rm(CH4, CO2, H2O)
  
  #I am not sure that the location of these files are the same across sites!
  #grab NEON level 4 friction velocity for a given site
  Ufric  <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxMome/turb", sep=""))
  #grab NEON level 4 qfqm for friction velocity for a given site
  Ufric.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxMome/turb", sep=""))
  Ufric.all <- left_join(Ufric, Ufric.qfqm, by = c("timeEnd", "timeBgn"))
  
  #grab NEON level 1 air pressure at 2nd tower position 30min resolution for a given site
  heights.press <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/presBaro", sep="")),]$name)
  heights.press <- as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.press, '000')),'30m'),6,7))
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/000_0", heights.press,"_30m/presAtm", sep=""))
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/000_0", heights.press,"_30m/presAtm", sep=""))
  P.all <- left_join(P, P.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = as.numeric(paste0(heights.press))/10)
  
  #grabs the all unique heights of air temp (name column in test.df = heights)
  heights.temp <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/tempAirLvl", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.temp <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.temp, '000')),'30m'),6,6))
  
  #loop over all heights for air temperature
  df_temp <- data.frame()
  
  for (j in 1:length(heights.temp)) {
    # progress message
    message("Processing height ", j, " (", length(heights.temp) - j, " remaining)")
    
    #set the air temp dataset name to be used by h5read
    temp.dir <- paste0("/", sitecode, '/dp01/data/tempAirLvl/000_0',heights.temp[j],'0_30m/temp', sep="")
    #set the qfqm dataset name to be used by h5read
    temp.qfqm <- paste0("/", sitecode, '/dp01/qfqm/tempAirLvl/000_0',heights.temp[j],'0_30m/temp', sep="")
    
    #grab NEON level 1 air temp at lowest tower position (assuming this is surface air temp) 1min resolution for a given site
    temp  <- h5read(hd.file, temp.dir)
    #grab NEON level 1 qfqm for surface air temperature
    qfqm  <- h5read(hd.file, temp.qfqm)
    
    #join datasets
    temp.all <- left_join(temp, qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(TowerPosition = paste0(heights.temp[j]))
    #add to populated df
    df_temp <- bind_rows(df_temp, temp.all)
    
  }
  
  #remove looping variable
  rm(temp, qfqm)
  
  #grab top of tower air temp
  heights.temp.top <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/tempAirTop", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.temp.top <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.temp.top, '000')),'30m'),6,6))
  
  temp.top <- h5read(hd.file, paste0("/", sitecode, '/dp01/data/tempAirTop/000_0', heights.temp.top,'0_30m/temp', sep=""))
  temp.top.qfqm <- h5read(hd.file, paste0("/", sitecode, '/dp01/qfqm/tempAirTop/000_0', heights.temp.top,'0_30m/temp', sep=""))
  temp.top.all <- left_join(temp.top, temp.top.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.temp.top))
  #combine all temp into one df
  df_temp <- rbind(df_temp, temp.top.all)
  
  #grab NEON level 1 horizontal wind speed data from top of tower at 30min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/000_0",heights[ length(heights)], "0_30m/veloXaxsYaxsErth", sep=""))
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/000_0",heights[ length(heights)], "0_30m/veloXaxsYaxsErth", sep=""))
  Sonic.all <- left_join(SoniWind, SoniWind.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights[length(heights)]))
  #grab all flux footprint variables
  #which of these is momentum roughness? Assuming veloZaxsHorSd for now based on range and mean 
  fluxFoot <- h5read(hd.file, paste("/", sitecode, "/dp04/data/foot/stat", sep=""))
  fluxFoot.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/foot/turb", sep=""))
  fluxFoot.all <- left_join(fluxFoot, fluxFoot.qfqm, by = c("timeEnd", "timeBgn"))
  #NEON only provides general qfqm for footprint not variable specific
  #grab NEON top of tower incoming solar radiation to be used in uStar filtering
  heights.solar <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/radiNet", sep="")),]$name)
  heights.solar <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.solar, '000')),'30m'),6,6))
  
  SWin <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_30m/radiSwIn", sep=""))
  SWin.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_30m/radiSwIn", sep=""))
  SWout <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_30m/radiSwOut", sep=""))
  SWout.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_30m/radiSwOut", sep=""))
  LWin <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_30m/radiLwIn", sep=""))
  LWin.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_30m/radiLwIn", sep=""))
  LWout <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_30m/radiLwOut", sep=""))
  LWout.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_30m/radiLwOut", sep=""))
  
  # Solar.all <- SWin %>%
  #   left_join(SWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(SWout, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(SWout.qfqm, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(LWout, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(LWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(LWout, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(LWout.qfqm, by = c("timeEnd", "timeBgn")) %>%
  #   mutate(TowerPosition = paste0(heights.solar))
  
  SWin.all <- SWin %>%
    left_join(SWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.solar))
  SWout.all <- SWout %>%
    left_join(SWout.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.solar))
  LWin.all <- LWin %>%
    left_join(LWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.solar))
  LWout.all <- LWout %>%
    left_join(LWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.solar))
  
  # #grab relative humidity
  # RH <- loadByProduct("DP1.00098.001", site="KONZ", 
  #               timeIndex=30, package="basic", 
  #               startdate=startdate, enddate=enddate,
  #               check.size=F)
  # RH <- RH$RH_30min %>% 
  #   filter(horizontalPosition == "000") %>%
  #   select(verticalPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  # 
  # #grab 2D horizontal wind speed
  # WS2D <- loadByProduct("DP1.00001.001", site="KONZ", 
  #                     timeIndex=30, package="basic", 
  #                     startdate=startdate, enddate=enddate,
  #                     check.size=F)
  # WS2D <- WS2D$twoDWSD_30min %>%
  #   select(verticalPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
  
  #soil heat flux
  plots.soil <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/fluxHeatSoil", sep="")),]$name)
  plots.soil <- unique(substr(plots.soil,1,3))
  
  #loop over all heights for soil heat flux
  df_soil <- data.frame()
  
  for (k in 1:length(plots.soil)) {
    # progress message
    message("Processing height ", k, " (", length(plots.soil) - k, " remaining)")
    
    #set the air temp dataset name to be used by h5read
    soil.dir <- paste("/", sitecode, "/dp01/data/fluxHeatSoil/", plots.soil[k], "_501_30m/fluxHeatSoil", sep="")
    #set the qfqm dataset name to be used by h5read
    soil.qfqm <- paste("/", sitecode, "/dp01/qfqm/fluxHeatSoil/", plots.soil[k], "_501_30m/fluxHeatSoil", sep="")
    
    #grab NEON level 1 air temp at lowest tower position (assuming this is surface air temp) 1min resolution for a given site
    soil  <- h5read(hd.file, soil.dir)
    #grab NEON level 1 qfqm for surface air temperature
    qfqm  <- h5read(hd.file, soil.qfqm)
    
    #join datasets
    soil.all <- left_join(soil, qfqm, by = c("timeEnd", "timeBgn")) %>%
      mutate(Plot = paste0(plots.soil[k]))
    #add to populated df
    df_soil <- bind_rows(df_soil, soil.all)
    
  }
  
  #remove looping variable
  rm(soil, qfqm)
  
  #remove timesteps with multiple measurements, select desired columns, make heights columns
  CH4.clean <- df_CH4 %>% 
    filter(duplicate_ct == 1) %>% 
    select(timeEnd, timeBgn, TowerPosition, mean, qfFinl, min, max, vari, numSamp)
  
  CO2.clean <- df_CO2 %>% 
    filter(duplicate_ct == 1) %>% 
    select(timeEnd, timeBgn, TowerPosition, mean, qfFinl, min, max, vari, numSamp)
  
  H2O.clean <- df_H2O %>% 
    filter(duplicate_ct == 1) %>% 
    select(timeEnd, timeBgn, TowerPosition, mean, qfFinl, min, max, vari, numSamp)
  
  CO2.850.clean <- df_CO2.850 %>% 
    filter(duplicate_ct == 1) %>% 
    select(timeEnd, timeBgn, TowerPosition, mean, qfFinl, min, max, vari, numSamp)
  
  H2O.850.clean <- df_H2O.850 %>% 
    filter(duplicate_ct == 1) %>% 
    select(timeEnd, timeBgn, TowerPosition, mean, qfFinl, min, max, vari, numSamp)
  
  #build fluxes df
  #Add NEON level 4 CO2, sensible heat, latent heat fluxes to df
  F_co2.nsae <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxCo2/nsae", sep=""))
  F_co2.nsae.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxCo2/nsae", sep=""))
  F_co2.all <- cbind(F_co2.nsae, select(F_co2.nsae.qfqm, qfFinl)) %>% 
    rename(nsae = flux, nsae.qfFinl = qfFinl)
  F_co2.stor <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxCo2/stor", sep="")) %>% select(flux)
  F_co2.stor.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxCo2/stor", sep="")) %>% select(qfFinl)
  F_co2.all <- cbind(F_co2.all, F_co2.stor, select(F_co2.stor.qfqm, qfFinl)) %>% 
    rename(stor = flux, stor.qfFinl = qfFinl)
  F_co2.turb <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxCo2/turb", sep="")) %>% select(flux)
  F_co2.turb.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxCo2/turb", sep="")) %>% select(qfFinl)
  F_co2.all <- cbind(F_co2.all, F_co2.turb, select(F_co2.turb.qfqm, qfFinl)) %>% 
    rename(turb = flux, turb.qfFinl = qfFinl)
  
  
  F_H.nsae <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxTemp/nsae", sep=""))
  F_H.nsae.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxTemp/nsae", sep=""))
  F_H.all <- cbind(F_H.nsae, select(F_H.nsae.qfqm, qfFinl)) %>% 
    rename(nsae = flux, nsae.qfFinl = qfFinl)
  F_H.stor <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxTemp/stor", sep="")) %>% select(flux)
  F_H.stor.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxTemp/stor", sep="")) %>% select(qfFinl)
  F_H.all <- cbind(F_H.all, F_H.stor, select(F_H.stor.qfqm, qfFinl)) %>% 
    rename(stor = flux, stor.qfFinl = qfFinl)
  F_H.turb <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxTemp/turb", sep="")) %>% select(flux)
  F_H.turb.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxTemp/turb", sep="")) %>% select(qfFinl)
  F_H.all <- cbind(F_H.all, F_H.turb, select(F_H.turb.qfqm, qfFinl)) %>% 
    rename(turb = flux, turb.qfFinl = qfFinl)
  
  
  F_LE.nsae <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxH2o/nsae", sep=""))
  F_LE.nsae.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxH2o/nsae", sep=""))
  F_LE.all <- cbind(F_LE.nsae, select(F_LE.nsae.qfqm, qfFinl)) %>% 
    rename(nsae = flux, nsae.qfFinl = qfFinl)
  F_LE.stor <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxH2o/stor", sep="")) %>% select(flux)
  F_LE.stor.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxH2o/stor", sep="")) %>% select(qfFinl)
  F_LE.all <- cbind(F_LE.all, F_LE.stor, select(F_LE.stor.qfqm, qfFinl)) %>% 
    rename(stor = flux, stor.qfFinl = qfFinl)
  F_LE.turb <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxH2o/turb", sep="")) %>% select(flux)
  F_LE.turb.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxH2o/turb", sep="")) %>% select(qfFinl)
  F_LE.all <- cbind(F_LE.all, F_LE.turb, select(F_LE.turb.qfqm, qfFinl)) %>% 
    rename(turb = flux, turb.qfFinl = qfFinl)
  
  
 
  
  
var = list(TAir = df_temp, Press = P.all, WS3D = Sonic.all, SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, SoilHF = df_soil, FluxFoot = fluxFoot.all, Ufric = Ufric.all, CH4 = CH4.clean, CO2 = CO2.clean, H2O = H2O.clean, H2O.850 = H2O.850.clean, CO2.850 = CO2.850.clean, F_co2 = F_co2.all, F_H = F_H.all, F_LE = F_LE.all)
  
  return(var)
}

#DEPRECIATED CODE
# # Merge all data:
# totF <- df_H2O %>% left_join(df_CO2, by= 'timeEnd') %>% left_join(df_CH4 , by= 'timeEnd')
# totF$timeEnd1 <- time.format(totF$timeEnd)  # Reformat the time and round it by one second
# 
# Ufric$uStar <- Ufric$veloFric
# Ufric.qfqm$uStar_qfqm <- Ufric.qfqm$qfFinl
# P$airpress <- P$mean
# P.qfqm$airpress_qfqm <- P.qfqm$qfFinl
# #surface air temperature used in aerodynamic profile method estimation of eddy diffusivity
# temp$airtemp <-temp$mean
# temp.qfqm$airtemp_qfqm <- temp.qfqm$qfFinl
# #mean wind speed at measurement height used for wind profile method
# SoniWind$uBar <- SoniWind$mean
# SoniWind.qfqm$uBar_qfqm <- SoniWind.qfqm$qfFinl
# #roughness length used for wind profile method
# MomRough$z0 <- MomRough$veloZaxsHorSd
# #incoming solar radiation to be used in uStar filtering
# Solar$radiSwIn <- Solar$mean
# Solar.qfqm$radiSwIn_qfqm <- Solar.qfqm$qfFinl
# 
# # Format the time for the merge:
# Ufric$timeEnd1 <- time.format(Ufric$timeEnd)
# Ufric.qfqm$timeEnd1 <- time.format(Ufric.qfqm$timeEnd)
# P$timeEnd1 <- time.format(P$timeEnd)
# P.qfqm$timeEnd1 <- time.format(P.qfqm$timeEnd)
# temp$timeEnd1 <- time.format(temp$timeEnd)
# temp.qfqm$timeEnd1 <- time.format(temp.qfqm$timeEnd)
# SoniWind$timeEnd1 <- time.format(SoniWind$timeEnd)
# SoniWind.qfqm$timeEnd1 <- time.format(SoniWind.qfqm$timeEnd)
# MomRough$timeEnd1 <- time.format(MomRough$timeEnd)
# Solar$timeEnd1 <- time.format(Solar$timeEnd)
# Solar.qfqm$timeEnd1 <- time.format(Solar.qfqm$timeEnd)
# 
# 
#  totF <- totF %>% left_join( Ufric[,c('timeEnd1', 'uStar')], by='timeEnd1') %>%
#   left_join( Ufric.qfqm[,c('timeEnd1', 'uStar_qfqm')], by='timeEnd1')%>%
#   left_join(P[,c('timeEnd1', 'airpress')], by='timeEnd1') %>% 
#   left_join(P.qfqm[,c('timeEnd1', 'airpress_qfqm')], by='timeEnd1') %>% 
#   left_join( temp[,c('timeEnd1', 'airtemp')], by='timeEnd1') %>% 
#   left_join( SoniWind[,c('timeEnd1', 'uBar')], by='timeEnd1')%>% 
#   left_join( SoniWind.qfqm[,c('timeEnd1', 'uBar_qfqm')], by='timeEnd1')%>% 
#   left_join( MomRough[,c('timeEnd1', 'z0')], by='timeEnd1')%>% 
#   left_join( Solar[,c('timeEnd1','radiSwIn' )], by='timeEnd1')%>% 
#   left_join( Solar.qfqm[,c('timeEnd1', 'radiSwIn_qfqm')], by='timeEnd1')
# 
# try(totF <- totF %>%left_join( temp.qfqm[,c('timeEnd1', 'airtemp_qfqm')], by='timeEnd1'), silent = T)
#DEPRECIATED CODE
#grad qfqm identifier
# CH4.qfqm <- h5read(hd.file, ch4.qfqm)
# CO2.qfqm <- h5read(hd.file, co2.qfqm)
# H2O.qfqm <- h5read(hd.file, h2o.qfqm)
# 
# if(i==1){
#   #only grab timeEnd column from 1st height on list
#   #why are we only grabbing timeEnd?
#   #add mean data, timeEnd, qfFinl of measurement for given height to df
#   df_CH4 <- dplyr::select(CH4, timeEnd, mean) %>% cbind(dplyr::select(CH4.qfqm, qfFinl))
#   df_CO2 <- dplyr::select(CO2, timeEnd, mean) %>% cbind(dplyr::select(CO2.qfqm, qfFinl))
#   df_H2O <- dplyr::select(H2O, timeEnd, mean) %>% cbind(dplyr::select(H2O.qfqm, qfFinl))
#   #rename df to match height of measurement
#   colnames(df_CH4) <- c("timeEnd", paste('ch4', heights[i], sep="."), paste('ch4_qfqm', heights[i], sep="."))
#   colnames(df_CO2) <- c("timeEnd", paste('co2', heights[i], sep="."), paste('co2_qfqm', heights[i], sep="."))
#   colnames(df_H2O) <- c("timeEnd", paste('h2o', heights[i], sep="."), paste('h2o_qfqm', heights[i], sep="."))
# }else{
#   #add mean data of measurement for given height to df
#   df_CH4 <- cbind(df_CH4, dplyr::select(CH4, mean), dplyr::select(CH4.qfqm, qfFinl))
#   df_CO2 <- cbind(df_CO2, dplyr::select(CO2, mean), dplyr::select(CO2.qfqm, qfFinl))
#   df_H2O <- cbind(df_H2O, dplyr::select(H2O, mean), dplyr::select(H2O.qfqm, qfFinl))
#   #rename 2nd to last column in df to match measurement height
#   colnames(df_CH4)[which(colnames(df_CH4) == "mean")] <- paste('ch4', heights[i], sep=".")
#   colnames(df_CO2)[which(colnames(df_CO2) == "mean")] <- paste('co2', heights[i], sep=".")
#   colnames(df_H2O)[which(colnames(df_H2O) == "mean")] <- paste('h2o', heights[i], sep=".")
#   #rename last column in df to match measurement height qfqm
#   colnames(df_CH4)[which(colnames(df_CH4) == "qfFinl")] <- paste('ch4_qfqm', heights[i], sep=".")
#   colnames(df_CO2)[which(colnames(df_CO2) == "qfFinl")] <- paste('co2_qfqm', heights[i], sep=".")
#   colnames(df_H2O)[which(colnames(df_H2O) == "qfFinl")] <- paste('h2o_qfqm', heights[i], sep=".")
#   
# }