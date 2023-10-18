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
  heights.press <-unique(stringr::str_subset(heights.press, '000')) %>% stringr::str_subset( '1m')
  #grabs the all unique heights of air temp (name column in test.df = heights)
  heights.temp <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/tempAirLvl", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.temp <-unique(stringr::str_subset(heights.temp, '000')) %>% stringr::str_subset( '1m')
  #grabs the all unique heights of horizontal wind speed (name column in test.df = heights)
  heights.sonic <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/soni", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.sonic <-unique(stringr::str_subset(heights.sonic, '000')) %>% stringr::str_subset( '1m')
  #grabs the all unique heights of incoming solar radiation (name column in test.df = heights)
  heights.solar <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/radiNet", sep="")),]$name)
  #subsets heights list grabbing only those at 1m
  heights.solar <-unique(stringr::str_subset(heights.solar, '000')) %>% stringr::str_subset( '1m')
  
  #loop over all heights for air temperature
  df_temp <- data.frame()
  
  for (j in 1:length(heights.temp)) {
    # progress message
    message("Processing height ", j, " (", length(heights.temp) - j, " remaining)")
    
    #set the air temp dataset name to be used by h5read
    temp.dir <- paste("/", sitecode, "/dp01/data/tempAirLvl/", heights.temp[j] ,"/temp", sep="")
    #set the qfqm dataset name to be used by h5read
    temp.qfqm <- paste("/", sitecode, "/dp01/qfqm/tempAirLvl/", heights.temp[j] ,"/temp", sep="")
    
    #grab NEON level 1 air temp at lowest tower position (assuming this is surface air temp) 1min resolution for a given site
    temp  <- h5read(hd.file, temp.dir) %>%
      select(timeEnd, mean)
    #grab NEON level 1 qfqm for surface air temperature
    qfqm  <- h5read(hd.file, temp.qfqm) %>%
      select(timeEnd, qfFinl)
    
    #join datasets
    temp.all <- left_join(temp, qfqm, by = "timeEnd") %>%
      mutate(heights = paste0('temp.', heights.temp[j]))
    #add to populated df
    df_temp <- bind_rows(df_temp, temp.all)
    
  }
  
  #remove looping variable
  rm(temp, qfqm)
  
  #grab NEON level 1 air pressure at 2nd tower position 1min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/", heights.press,"/presAtm", sep="")) %>%
    select(timeEnd, mean)
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/", heights.press,"/presAtm", sep="")) %>%
    select(timeEnd, qfFinl)
  #grab NEON level 1 horizontal wind speed data from top of tower at 1min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/", heights.sonic, "/veloXaxsYaxsErth", sep="")) %>%
    select(timeEnd, mean)
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/", heights.sonic, "/veloXaxsYaxsErth", sep="")) %>%
    select(timeEnd, qfFinl)
    
  #grab NEON top of tower incoming solar radiation to be used in uStar filtering
  Solar <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/", heights.solar, "/radiSwIn", sep="")) %>%
    select(timeEnd, mean)
  Solar.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/", heights.solar, "/radiSwIn", sep="")) %>%
    select(timeEnd, qfFinl)
  
  # Merge all data:
  P$airpress <- P$mean
  P.qfqm$airpress_qfqm <- P.qfqm$qfFinl
  #surface air temperature used in aerodynamic profile method estimation of eddy diffusivity
  temp$airtemp <-temp$mean
  temp.qfqm$airtemp_qfqm <- temp.qfqm$qfFinl
  #mean wind speed at measurement height used for wind profile method
  SoniWind$uBar <- SoniWind$mean
  SoniWind.qfqm$uBar_qfqm <- SoniWind.qfqm$qfFinl
  #incoming solar radiation to be used in uStar filtering
  Solar$radiSwIn <- Solar$mean
  Solar.qfqm$radiSwIn_qfqm <- Solar.qfqm$qfFinl
  
  # Format the time for the merge:
  P$timeEnd1 <- time.format(P$timeEnd)
  P.qfqm$timeEnd1 <- time.format(P.qfqm$timeEnd)
  temp$timeEnd1 <- time.format(temp$timeEnd)
  temp.qfqm$timeEnd1 <- time.format(temp.qfqm$timeEnd)
  SoniWind$timeEnd1 <- time.format(SoniWind$timeEnd)
  SoniWind.qfqm$timeEnd1 <- time.format(SoniWind.qfqm$timeEnd)
  Solar$timeEnd1 <- time.format(Solar$timeEnd)
  Solar.qfqm$timeEnd1 <- time.format(Solar.qfqm$timeEnd)
  
  
  totF <- totF %>% left_join(P[,c('timeEnd1', 'airpress')], by='timeEnd1') %>% 
    left_join(P.qfqm[,c('timeEnd1', 'airpress_qfqm')], by='timeEnd1') %>% 
    left_join( temp[,c('timeEnd1', 'airtemp')], by='timeEnd1') %>% 
    left_join( SoniWind[,c('timeEnd1', 'uBar')], by='timeEnd1')%>% 
    left_join( SoniWind.qfqm[,c('timeEnd1', 'uBar_qfqm')], by='timeEnd1')%>% 
    left_join( Solar[,c('timeEnd1','radiSwIn' )], by='timeEnd1')%>% 
    left_join( Solar.qfqm[,c('timeEnd1', 'radiSwIn_qfqm')], by='timeEnd1')
  
  try(totF <- totF %>%left_join( temp.qfqm[,c('timeEnd1', 'airtemp_qfqm')], by='timeEnd1'), silent = T)
}