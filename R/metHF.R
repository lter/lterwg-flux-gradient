#' met.HF
#'
#' @param hd.file file type h5 containg NEON site specific data
#' @param sitecode NEON site code
#'
#' @return df containing high frequnecy site met
#'
met.HF <- function(hd.file, sitecode){
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
  
  #grab NEON level 1 air pressure at 2nd tower position 1min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/000_0", heights.press,"_01m/presAtm", sep=""))
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/000_0", heights.press,"_01m/presAtm", sep=""))
  P.all <- left_join(P, P.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.press))
  #grab NEON level 1 horizontal wind speed data from top of tower at 1min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/000_0", heights.sonic, "0_01m/veloXaxsYaxsErth", sep=""))
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/000_0", heights.sonic, "0_01m/veloXaxsYaxsErth", sep=""))
  Sonic.all <- left_join(SoniWind, SoniWind.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.sonic))
  #grab 2D wind -> different DPI
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
  
  
  Solar.all <- SWin %>%
    left_join(SWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
    left_join(SWout, by = c("timeEnd", "timeBgn")) %>%
    left_join(SWout.qfqm, by = c("timeEnd", "timeBgn")) %>%
    left_join(LWout, by = c("timeEnd", "timeBgn")) %>%
    left_join(LWin.qfqm, by = c("timeEnd", "timeBgn")) %>%
    left_join(LWout, by = c("timeEnd", "timeBgn")) %>%
    left_join(LWout.qfqm, by = c("timeEnd", "timeBgn")) %>%
    mutate(TowerPosition = paste0(heights.solar))
  #grab soil heat flux (G), add column for soil plot
  
  
  # totMet <- df_temp %>% 
  #   left_join(P.all, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(Sonic.all, by = c("timeEnd", "timeBgn")) %>%
  #   left_join(Solar.all, by = c("timeEnd", "timeBgn"))
  met = list(TAir = df_temp, Press = P.all, WS3D = Sonic.all, SWin = Solar.all)
  
  return(met)
}