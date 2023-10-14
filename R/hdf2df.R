#' hdf2df
#'
#' @param sitecode NEON site code
#' @param hd.file file type h5 containing NEON site specific data
#' @return data frame containing site co2, h2o, ch4 measurements at various tower heights
#' @author Alexis Helgeson, David Reed, and Sparkle Malone
#' 
hdf2df <- function(hd.file, sitecode){
  
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
  heights <-unique(stringr::str_subset(heights.list, '000')) %>% stringr::str_subset( '30m')
  
  #Grab NEON level 1 CH4 cont, CO2 stor, H2O stor for each height
  df_CH4 =data.frame()
  df_CO2 =data.frame()
  df_H2O  =data.frame()
  #loop over heights
  for(i in 1:length(heights)){
    #set the gas concentration dataset name to be used by h5read
    ch4.dir <- paste("/", sitecode, '/dp01/data/ch4Conc/',heights[i],'/rtioMoleDryCh4', sep="")
    co2.dir <- paste("/", sitecode, '/dp01/data/co2Stor/',heights[i], '/rtioMoleDryCo2', sep="")
    h2o.dir <- paste( "/", sitecode, '/dp01/data/h2oStor/',heights[i], '/rtioMoleDryH2o', sep="")
    #set the qfqm dataset name to be used by h5read
    ch4.qfqm <- paste("/", sitecode, '/dp01/qfqm/ch4Conc/',heights[i],'/rtioMoleDryCh4', sep="")
    co2.qfqm <- paste("/", sitecode, '/dp01/qfqm/co2Stor/',heights[i], '/rtioMoleDryCo2', sep="")
    h2o.qfqm <- paste( "/", sitecode, '/dp01/qfqm/h2oStor/',heights[i], '/rtioMoleDryH2o', sep="")
    #h5read fcn reads a dataset in h5 file and returns an array containing data read
    CH4 <- h5read(hd.file, ch4.dir)
    CO2 <- h5read(hd.file, co2.dir)
    H2O <- h5read(hd.file, h2o.dir)
    #grad qfqm identifier
    CH4.qfqm <- h5read(hd.file, ch4.qfqm)
    CO2.qfqm <- h5read(hd.file, co2.qfqm)
    H2O.qfqm <- h5read(hd.file, h2o.qfqm)
    
    if(i==1){
      #only grab timeEnd column from 1st height on list
      #why are we only grabbing timeEnd?
      #add mean data, timeEnd, qfFinl of measurement for given height to df
      df_CH4 <- dplyr::select(CH4, timeEnd, mean) %>% cbind(dplyr::select(CH4.qfqm, qfFinl))
      df_CO2 <- dplyr::select(CO2, timeEnd, mean) %>% cbind(dplyr::select(CO2.qfqm, qfFinl))
      df_H2O <- dplyr::select(H2O, timeEnd, mean) %>% cbind(dplyr::select(H2O.qfqm, qfFinl))
      #rename df to match height of measurement
      colnames(df_CH4) <- c("timeEnd", paste('ch4', heights[i], sep="."), paste('ch4_qfqm', heights[i], sep="."))
      colnames(df_CO2) <- c("timeEnd", paste('co2', heights[i], sep="."), paste('co2_qfqm', heights[i], sep="."))
      colnames(df_H2O) <- c("timeEnd", paste('h2o', heights[i], sep="."), paste('h2o_qfqm', heights[i], sep="."))
    }else{
      #add mean data of measurement for given height to df
      df_CH4 <- cbind(df_CH4, dplyr::select(CH4, mean), dplyr::select(CH4.qfqm, qfFinl))
      df_CO2 <- cbind(df_CO2, dplyr::select(CO2, mean), dplyr::select(CO2.qfqm, qfFinl))
      df_H2O <- cbind(df_H2O, dplyr::select(H2O, mean), dplyr::select(H2O.qfqm, qfFinl))
      #rename 2nd to last column in df to match measurement height
      colnames(df_CH4)[which(colnames(df_CH4) == "mean")] <- paste('ch4', heights[i], sep=".")
      colnames(df_CO2)[which(colnames(df_CO2) == "mean")] <- paste('co2', heights[i], sep=".")
      colnames(df_H2O)[which(colnames(df_H2O) == "mean")] <- paste('h2o', heights[i], sep=".")
      #rename last column in df to match measurement height qfqm
      colnames(df_CH4)[which(colnames(df_CH4) == "qfFinl")] <- paste('ch4_qfqm', heights[i], sep=".")
      colnames(df_CO2)[which(colnames(df_CO2) == "qfFinl")] <- paste('co2_qfqm', heights[i], sep=".")
      colnames(df_H2O)[which(colnames(df_H2O) == "qfFinl")] <- paste('h2o_qfqm', heights[i], sep=".")
      
    }
    
  }
  
  #Add NEON level 4 CO2, sensible heat, latent heat fluxes to df
  F_co2 <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxCo2/nsae", sep=""))
  F_H <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxTemp/nsae", sep=""))
  F_LE <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxH2o/nsae", sep=""))
  #Add NEON level 4 CO2, sensisble heat, latent heat qfqm to df
  F_co2.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxCo2/nsae", sep=""))
  F_H.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxTemp/nsae", sep=""))
  F_LE.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxH2o/nsae", sep=""))
  df_CO2$F_co2 = F_co2$flux
  df_CO2$F_co2_qfqm = F_co2.qfqm$qfFinl
  df_H2O$F_H = F_H$flux
  df_H2O$F_H_qfqm = F_H.qfqm$qfFinl
  df_H2O$F_LE = F_LE$flux
  df_H2O$F_LE_qfqm = F_LE.qfqm$qfFinl
  
  #remove looping var and flux df
  rm(CH4, CO2, F_co2, F_H, H2O, F_LE, F_co2.qfqm, F_H.qfqm, F_LE.qfqm)
  
  #I am not sure that the location of these files are the same across sites!
  #grab NEON level 4 friction velocity for a given site
  Ufric  <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxMome/turb", sep=""))
  #grab NEON level 4 qfqm for friction velocity for a given site
  Ufric.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp04/qfqm/fluxMome/turb", sep=""))
  
  #grab NEON level 1 air pressure at 2nd tower position 30min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/",max(test.df$name[test.df$group==paste("/", sitecode, "/dp01/data/presBaro", sep="")]),"/presAtm", sep=""))
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/",max(test.df$name[test.df$group==paste("/", sitecode, "/dp01/qfqm/presBaro", sep="")]),"/presAtm", sep=""))
  
  #grab NEON level 1 air temp at lowest tower position (assuming this is surface air temp) 30min resolution for a given site
  temp  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/tempAirLvl/000_010_30m/temp", sep=""))
  #grab NEON level 1 qfqm for surface air temperature
  temp.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/tempAirLvl/000_010_30m/temp", sep=""))
  #grab NEON level 1 horizontal wind speed data from top of tower at 30min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/000_060_30m/veloXaxsYaxsErth", sep=""))
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/000_060_30m/veloXaxsYaxsErth", sep=""))
  #grab NEON momentum roughness from footprint stats table
  #which of these is momentum roughness? Assuming veloZaxsHorSd for now based on range and mean 
  MomRough <- h5read(hd.file, paste("/", sitecode, "/dp04/data/foot/stat", sep=""))
  #NEON only provides general qfqm for footprint not variable specific
  #grab NEON top of tower incoming solar radiation to be used in uStar filtering
  Solar <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/000_060_30m/radiSwIn", sep=""))
  Solar.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/000_060_30m/radiSwIn", sep=""))
  # Merge all data:
  totF <- df_H2O %>% left_join(df_CO2, by= 'timeEnd') %>% left_join(df_CH4 , by= 'timeEnd')
  
  Ufric$uStar <- Ufric$veloFric
  Ufric.qfqm$uStar_qfqm <- Ufric.qfqm$qfFinl
  P$airpress <- P$mean
  P.qfqm$airpress_qfqm <- P.qfqm$qfFinl
  #surface air temperature used in aerodynamic profile method estimation of eddy diffusivity
  temp$airtemp <-temp$mean
  temp.qfqm$airtemp_qfqm <- temp.qfqm$qfFinl
  #mean wind speed at measurement height used for wind profile method
  SoniWind$uBar <- SoniWind$mean
  SoniWind.qfqm$uBar_qfqm <- SoniWind.qfqm$qfFinl
  #roughness length used for wind profile method
  MomRough$z0 <- MomRough$veloZaxsHorSd
  #incoming solar radiation to be used in uStar filtering
  Solar$radiSwIn <- Solar$mean
  Solar.qfqm$radiSwIn_qfqm <- Solar.qfqm$qfFinl
  
  totF <- totF %>% left_join( Ufric[,c('timeEnd', 'uStar')], by='timeEnd') %>%
    left_join( Ufric.qfqm[,c('timeEnd', 'uStar_qfqm')], by='timeEnd')%>%
    left_join(P[,c('timeEnd', 'airpress')], by='timeEnd') %>% 
    left_join(P.qfqm[,c('timeEnd', 'airpress_qfqm')], by='timeEnd') %>% 
    left_join( temp[,c('timeEnd', 'airtemp')], by='timeEnd')%>% 
    left_join( temp.qfqm[,c('timeEnd', 'airtemp_qfqm')], by='timeEnd')%>% 
    left_join( SoniWind[,c('timeEnd', 'uBar')], by='timeEnd')%>% 
    left_join( SoniWind.qfqm[,c('timeEnd', 'uBar_qfqm')], by='timeEnd')%>% 
    left_join( MomRough[,c('timeEnd', 'z0')], by='timeEnd')%>% 
    left_join( Solar[,c('timeEnd','radiSwIn' )], by='timeEnd')%>% 
    left_join( Solar.qfqm[,c('timeEnd', 'radiSwIn_qfqm')], by='timeEnd')
  
  
  
  return(totF)
}

