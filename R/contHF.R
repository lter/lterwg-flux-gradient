#' cont.HF
#'
#' @param sitecode NEON site code
#' @param hd.file file type h5 containg NEON site specific data
#'
#' @return list with 4 data frames with site attributes and 9m tower concentrations for CH4, CO2, H2O 
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, Sparkle Malone, Jackie Matthes
cont.HF <- function(hd.file, sitecode){
  
  #lists the contents of hdf5 file and returns a df with file contents
  test.df <- rhdf5::h5ls(hd.file,
                  recursive = TRUE,
                  all =TRUE, 
                  datasetinfo = TRUE,
                  index_type = rhdf5::h5default("H5_INDEX"),
                  order = rhdf5::h5default("H5_ITER"), 
                  s3 = FALSE, 
                  s3credentials = NULL,
                  native = FALSE)
  
  #grabs the all unique heights of ch4Conc (name column in test.df = heights)
  heights.list <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/ch4Conc", sep="")),]$name)
  #grabs unique heights for co2/h2o concentrations
  #heights.co2h2o <- unique(test.df[which(test.df$group == paste( "/", sitecode,"/dp01/data/co2Stor", sep="")),]$name)
  #subsets heights list grabbing only those at 9m/2m
  heights.9m <-heights <- as.numeric(substr(stringr::str_subset(unique(stringr::str_subset(heights.list, '000')),'09m'),6,6))
  #heights.2m <-unique(stringr::str_subset(heights.co2h2o, '000')) %>% stringr::str_subset( '2m')
  
  # Get tower attributes
  attr <- data.frame(rhdf5::h5readAttributes(hd.file, name = paste0("/", sitecode)))
  attr <- dplyr::select(attr, DistZaxsLvlMeasTow, DistZaxsCnpy, TimeTube,
                        ElevRefeTow, LatTow, LonTow, ZoneUtm, ZoneTime)
  
  #add NEON sitecide as column
  attr$Site <- sitecode 
  
  #Grab NEON level 1 CH4 cont, CO2 stor, H2O stor for each height
  df_CH4 =data.frame()
  df_CO2 =data.frame()
  df_H2O  =data.frame()
  
  #loop over heights for CH4
  for(i in 1:length(heights.9m)){
    
    # progress message
    message("Processing height ", i, " (", length(heights.9m) - i, " remaining)")
    
    #set the gas concentration dataset name to be used by h5read
    ch4.dir <- paste0("/", sitecode, '/dp01/data/ch4Conc/000_0',heights[i],'0_09m/rtioMoleDryCh4', sep="")
    co2.dir <- paste0("/", sitecode, '/dp01/data/isoCo2/000_0',heights[i], '0_09m/rtioMoleDryCo2', sep="")
    h2o.dir <- paste0( "/", sitecode, '/dp01/data/isoH2o/000_0',heights[i], '0_09m/rtioMoleDryH2o', sep="")
    
    #set the qfqm dataset name to be used by h5read
    ch4.qfqm <- paste0("/", sitecode, '/dp01/qfqm/ch4Conc/000_0',heights[i],'0_09m/rtioMoleDryCh4', sep="")
    co2.qfqm <- paste0("/", sitecode, '/dp01/qfqm/isoCo2/000_0',heights[i], '0_09m/rtioMoleDryCo2', sep="")
    h2o.qfqm <- paste0( "/", sitecode, '/dp01/qfqm/isoH2o/000_0',heights[i], '0_09m/rtioMoleDryH2o', sep="")
    
    #h5read fcn reads a dataset in h5 file and returns an array containing data read
    CH4 <- rhdf5::h5read(hd.file, ch4.dir)
    CO2 <- rhdf5::h5read(hd.file, co2.dir)
    H2O <- rhdf5::h5read(hd.file, h2o.dir)
    
    #grad qfqm identifier and combine with data
    CH4.qfqm <- rhdf5::h5read(hd.file, ch4.qfqm)
    CH4_all = dplyr::full_join(CH4, CH4.qfqm)
    CH4_all$height = heights[i]
    
    CO2.qfqm <- rhdf5::h5read(hd.file, co2.qfqm)
    CO2_all = dplyr::full_join(CO2, CO2.qfqm)
    CO2_all$height = heights[i]
    
    H2O.qfqm <- rhdf5::h5read(hd.file, h2o.qfqm)
    H2O_all = dplyr::full_join(H2O, H2O.qfqm)
    H2O_all$height = heights[i]
    
    # Build dataframe across heights, sort by time
    if(i==1){
      df_CH4 = CH4_all
      df_CO2 = CO2_all
      df_H2O = H2O_all
    } else {
      df_CH4 = dplyr::arrange(dplyr::bind_rows(df_CH4, CH4_all),timeBgn)
      df_CO2 = dplyr::arrange(dplyr::bind_rows(df_CO2, CO2_all),timeBgn)
      df_H2O = dplyr::arrange(dplyr::bind_rows(df_H2O, H2O_all),timeBgn)
    }
  }
  tower_conc = list(attr = attr, CH4 = df_CH4, CO2 = df_CO2, 
                    H2O = df_H2O)
  return(tower_conc)
}
  
    
  #   ch4.dir <- paste("/", sitecode, '/dp01/data/ch4Conc/',heights.9m[i],'/rtioMoleDryCh4', sep="")
  #   #set the qfqm dataset name to be used by h5read
  #   ch4.qfqm <- paste("/", sitecode, '/dp01/qfqm/ch4Conc/',heights.9m[i],'/rtioMoleDryCh4', sep="")
  #   #h5read fcn reads a dataset in h5 file and returns an array containing data read, select for desired columns
  #   CH4 <- h5read(hd.file, ch4.dir) %>% 
  #     select(timeEnd, mean) %>%
  #     group_by(timeEnd) %>%
  #     summarize(mean = mean(mean, na.rm = T),
  #               value_paste = paste(mean, collapse= "; "),
  #               sd_val = sd(mean, na.rm = T),
  #               value_ct = n()) %>%
  #     ungroup()
  #   #grad qfqm identifier, select desired columns
  #   CH4.qfqm <- h5read(hd.file, ch4.qfqm) %>%
  #     select(timeEnd, qfFinl) %>%
  #     distinct()
  #   #join datasets
  #   CH4.all <- left_join(CH4, CH4.qfqm, by = "timeEnd") %>%
  #     mutate(heights = paste0('ch4.', heights.9m[i]))
  #   #add to populated df
  #   df_CH4 <- bind_rows(df_CH4, CH4.all)
  #   #DEPRECIATED CODE
  #   # if(i==1){
  #   #   #only grab timeEnd column from 1st height on list
  #   #   #why are we only grabbing timeEnd?
  #   #   #add mean data, timeEnd, qfFinl of measurement for given height to df
  #   #   df_CH4 <- dplyr::select(CH4, timeEnd, mean) %>% cbind(dplyr::select(CH4.qfqm, qfFinl))
  #   #   #rename df to match height of measurement
  #   #   colnames(df_CH4) <- c("timeEnd", paste('ch4', heights.9m[i], sep="."), paste('ch4_qfqm', heights.9m[i], sep="."))
  #   # }else{
  #   #   #rename 2nd to last column in df to match measurement height
  #   #   colnames(CH4)[which(colnames(CH4) == "mean")] <- paste('ch4', heights.9m[i], sep=".")
  #   #   CH4 <- CH4 %>% select("timeEnd", paste('ch4', heights.9m[i], sep="."))
  #   #   #rename last column in df to match measurement height qfqm
  #   #   colnames(CH4.qfqm)[which(colnames(CH4.qfqm) == "qfFinl")] <- paste('ch4_qfqm', heights.9m[i], sep=".")
  #   #   CH4.qfqm <- CH4.qfqm %>% select("timeEnd", paste('ch4_qfqm', heights.9m[i], sep="."))
  #   #   #add mean data of measurement for given height to df
  #   #   df_CH4 <- df_CH4 %>% left_join(CH4, by= 'timeEnd') %>% left_join(CH4.qfqm, by= 'timeEnd')
  #   #   
  #   #   
  #   # }
  #   
  # }
  # 
  # # Diagnostic check (DELETE ME)
  # #df_CH4 %>% filter(value_ct > 1)
  # 
  # #loop over heights for co2/h2o
  # for(k in 1:length(heights.2m)){
  #   
  #   # progress message
  #   message("Processing height ", k, " (", length(heights.2m) - k, " remaining)")
  #   
  #   #set the gas concentration dataset name to be used by h5read
  #   co2.dir <- paste("/", sitecode, '/dp01/data/co2Stor/',heights.2m[k], '/rtioMoleDryCo2', sep="")
  #   h2o.dir <- paste( "/", sitecode, '/dp01/data/h2oStor/',heights.2m[k], '/rtioMoleDryH2o', sep="")
  #   #set the qfqm dataset name to be used by h5read
  #   co2.qfqm <- paste("/", sitecode, '/dp01/qfqm/co2Stor/',heights.2m[k], '/rtioMoleDryCo2', sep="")
  #   h2o.qfqm <- paste( "/", sitecode, '/dp01/qfqm/h2oStor/',heights.2m[k], '/rtioMoleDryH2o', sep="")
  #   #h5read fcn reads a dataset in h5 file and returns an array containing data read
  #   CO2 <- h5read(hd.file, co2.dir)
  #   H2O <- h5read(hd.file, h2o.dir)
  #   #grad qfqm identifier
  #   CO2.qfqm <- h5read(hd.file, co2.qfqm)
  #   H2O.qfqm <- h5read(hd.file, h2o.qfqm)
  #   
  #   #build CO2 df
  #   CO2 <- h5read(hd.file, co2.dir) %>% 
  #     select(timeEnd, mean) %>%
  #     group_by(timeEnd) %>%
  #     summarize(mean = mean(mean, na.rm = T),
  #               value_paste = paste(mean, collapse= "; "),
  #               sd_val = sd(mean, na.rm = T),
  #               value_ct = n()) %>%
  #     ungroup()
  #   #grad qfqm identifier
  #   CO2.qfqm <- h5read(hd.file, co2.qfqm) %>%
  #     select(timeEnd, qfFinl) %>%
  #     distinct()
  #   
  #   CO2.all <- left_join(CO2, CO2.qfqm, by = "timeEnd") %>%
  #     mutate(heights = paste('co2', heights.2m[k], sep="."))
  #   
  #   df_CO2 <- bind_rows(df_CO2, CO2.all)
  #   
  #   #build H2O df
  #   H2O <- h5read(hd.file, h2o.dir) %>% 
  #     select(timeEnd, mean) %>%
  #     group_by(timeEnd) %>%
  #     summarize(mean = mean(mean, na.rm = T),
  #               value_paste = paste(mean, collapse= "; "),
  #               sd_val = sd(mean, na.rm = T),
  #               value_ct = n()) %>%
  #     ungroup()
  #   #grad qfqm identifier
  #   H2O.qfqm <- h5read(hd.file, h2o.qfqm) %>%
  #     select(timeEnd, qfFinl) %>%
  #     distinct()
  #   
  #   H2O.all <- left_join(H2O, H2O.qfqm, by = "timeEnd") %>%
  #     mutate(heights = paste('h2o', heights.2m[k], sep="."))
  #   
  #   df_H2O <- bind_rows(df_H2O, H2O.all)
  #   #DEPRECIATED CODE
  #   # if(k==1){
  #   #   #only grab timeEnd column from 1st height on list
  #   #   #why are we only grabbing timeEnd?
  #   #   #add mean data, timeEnd, qfFinl of measurement for given height to df
  #   #   df_CO2 <- dplyr::select(CO2, timeEnd, mean) %>% cbind(dplyr::select(CO2.qfqm, qfFinl))
  #   #   df_H2O <- dplyr::select(H2O, timeEnd, mean) %>% cbind(dplyr::select(H2O.qfqm, qfFinl))
  #   #   #rename df to match height of measurement
  #   #   colnames(df_CO2) <- c("timeEnd", paste('co2', heights.2m[k], sep="."), paste('co2_qfqm', heights.2m[k], sep="."))
  #   #   colnames(df_H2O) <- c("timeEnd", paste('h2o', heights.2m[k], sep="."), paste('h2o_qfqm', heights.2m[k], sep="."))
  #   # }else{
  #   #   #add mean data of measurement for given height to df
  #   #   df_CO2 <- cbind(df_CO2, dplyr::select(CO2, mean), dplyr::select(CO2.qfqm, qfFinl))
  #   #   df_H2O <- cbind(df_H2O, dplyr::select(H2O, mean), dplyr::select(H2O.qfqm, qfFinl))
  #   #   #rename 2nd to last column in df to match measurement height
  #   #   colnames(df_CO2)[which(colnames(df_CO2) == "mean")] <- paste('co2', heights.2m[k], sep=".")
  #   #   colnames(df_H2O)[which(colnames(df_H2O) == "mean")] <- paste('h2o', heights.2m[k], sep=".")
  #   #   #rename last column in df to match measurement height qfqm
  #   #   colnames(df_CO2)[which(colnames(df_CO2) == "qfFinl")] <- paste('co2_qfqm', heights.2m[k], sep=".")
  #   #   colnames(df_H2O)[which(colnames(df_H2O) == "qfFinl")] <- paste('h2o_qfqm', heights.2m[k], sep=".")
  #   #   
  #   # }
  #   
  # }
  # 
  # # Diagnostic check (DELETE ME)
  # # df_CO2 %>% filter(value_ct > 1)
  # # df_H2O %>% filter(value_ct > 1)
  # 
  # #remove looping vars
  # rm(CH4, CO2, H2O, CO2.qfqm, H2O.qfqm, CH4.qfqm)
  # 
  # #remove timesteps with multiple measurements, select desired columns, make heights columns
  # CH4.clean <- df_CH4 %>% filter(value_ct == 1) %>% select(timeEnd, mean, qfFinl, heights) %>% pivot_wider(names_from = heights, values_from = c(mean, qfFinl))
  # CO2.clean <- df_CO2 %>% filter(value_ct == 1) %>% select(timeEnd, mean, qfFinl, heights) %>% pivot_wider(names_from = heights, values_from = c(mean, qfFinl))
  # H2O.clean <- df_H2O %>% filter(value_ct == 1) %>% select(timeEnd, mean, qfFinl, heights) %>% pivot_wider(names_from = heights, values_from = c(mean, qfFinl))
  # 
  # # Merge all data:
  # totF <- H2O.clean %>% left_join(CH4.clean, by= 'timeEnd') %>% left_join(CO2.clean, by= 'timeEnd')
  # totF$timeEnd1 <- time.format(totF$timeEnd)  # Reformat the time and round it by one second
  # 
  #return(totF)
#}

