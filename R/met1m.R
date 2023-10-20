#' met.1m
#'
#' @param hd.file file type h5 containg NEON site specific data
#' @param sitecode NEON site code
#' @param startdate data start date
#' @param enddate data end date
#'
#' @return df containing high frequnecy site met
#'
met.1m <- function(hd.file, sitecode, startdate, enddate){
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
  #grabs the all unique heights of air pressure (name column in test.df = heights)
  heights.press <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/presBaro", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.press <- as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.press, '000')),'1m'),6,7))
  #grabs the all unique heights of air temp (name column in test.df = heights)
  heights.temp <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/tempAirLvl", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.temp <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.temp, '000')),'1m'),6,6))
  #grabs the all unique heights of horizontal wind speed (name column in test.df = heights)
  heights.sonic <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/soni", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.sonic <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.sonic, '000')),'1m'),6,6))
  #grabs the all unique heights of incoming solar radiation (name column in test.df = heights)
  heights.solar <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/radiNet", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.solar <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.solar, '000')),'1m'),6,6))
  
  #loop over all heights for air temperature
  df_temp <- data.frame()
  
  for (j in 1:length(heights.temp)) {
    # progress message
    message("Processing height ", j, " (", length(heights.temp) - j, " remaining)")
    
    #set the air temp dataset name to be used by h5read
    temp.dir <- paste0("/", sitecode, '/dp01/data/tempAirLvl/000_0',heights.temp[j],'0_01m/temp', sep="")
    #set the qfqm dataset name to be used by h5read
    temp.qfqm <- paste0("/", sitecode, '/dp01/qfqm/tempAirLvl/000_0',heights.temp[j],'0_01m/temp', sep="")
    
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
  heights.temp.top <-as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.temp.top, '000')),'1m'),6,6))
  
  temp.top <- h5read(hd.file, paste0("/", sitecode, '/dp01/data/tempAirTop/000_0', heights.temp.top,'0_1m/temp', sep=""))
  temp.top.qfqm <- h5read(hd.file, paste0("/", sitecode, '/dp01/qfqm/tempAirTop/000_0', heights.temp.top,'0_1m/temp', sep=""))
  temp.top.all <- left_join(temp.top, temp.top.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.temp.top))
  #combine all temp into one df
  df_temp <- rbind(df_temp, temp.top.all)
  
  #grab NEON level 1 air pressure at 2nd tower position 1min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/000_0", heights.press,"_01m/presAtm", sep=""))
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/000_0", heights.press,"_01m/presAtm", sep=""))
  P.all <- left_join(P, P.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = as.numeric(paste0(heights.press))/10)
  #grab NEON level 1 horizontal wind speed data from top of tower at 1min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/000_0", heights.sonic, "0_01m/veloXaxsYaxsErth", sep=""))
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/000_0", heights.sonic, "0_01m/veloXaxsYaxsErth", sep=""))
  Sonic.all <- left_join(SoniWind, SoniWind.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.sonic))
  
  #grab NEON top of tower incoming solar radiation to be used in uStar filtering
  #grab all radiation terms
  SWin <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_01m/radiSwIn", sep=""))
  SWin.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_01m/radiSwIn", sep=""))
  SWout <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_01m/radiSwOut", sep=""))
  SWout.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_01m/radiSwOut", sep=""))
  LWin <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_01m/radiLwIn", sep=""))
  LWin.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_01m/radiLwIn", sep=""))
  LWout <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_0", heights.solar, "0_01m/radiLwOut", sep=""))
  LWout.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_0", heights.solar, "0_01m/radiLwOut", sep=""))
  
  
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
  #                     timeIndex=1, package="basic", 
  #                     startdate=startdate, enddate=enddate,
  #                     check.size=F)
  # RH <- RH$RH_1min %>% 
  #   filter(horizontalPosition == "000") %>%
  #   select(verticalPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  
  #soil heat flux
  plots.soil <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/fluxHeatSoil", sep="")),]$name)
  plots.soil <- unique(substr(plots.soil,1,3))
  
  #loop over all heights for soil heat flux
  df_soil <- data.frame()
  
  for (k in 1:length(plots.soil)) {
    # progress message
    message("Processing height ", k, " (", length(plots.soil) - k, " remaining)")
    
    #set the air temp dataset name to be used by h5read
    soil.dir <- paste("/", sitecode, "/dp01/data/fluxHeatSoil/", plots.soil[k], "_501_01m/fluxHeatSoil", sep="")
    #set the qfqm dataset name to be used by h5read
    soil.qfqm <- paste("/", sitecode, "/dp01/qfqm/fluxHeatSoil/", plots.soil[k], "_501_01m/fluxHeatSoil", sep="")
    
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
  

  met = list(TAir = df_temp, Press = P.all, WS3D = Sonic.all, SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, SoilHF = df_soil)
  
  return(met)
}


# totMet <- df_temp %>% 
#   left_join(P.all, by = c("timeEnd", "timeBgn")) %>%
#   left_join(Sonic.all, by = c("timeEnd", "timeBgn")) %>%
#   left_join(Solar.all, by = c("timeEnd", "timeBgn"))