#' hdf2df
#'
#' @param sitecode NEON site code
#' @param hd.file file type h5 containg NEON site specific data
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#'
#' @author Alexis Helgeson
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
    #set the dataset name to be used by h5read
    ch4.dir <- paste("/", sitecode, '/dp01/data/ch4Conc/',heights[i],'/rtioMoleDryCh4', sep="")
    co2.dir <- paste("/", sitecode, '/dp01/data/co2Stor/',heights[i], '/rtioMoleDryCo2', sep="")
    h2o.dir <- paste( "/", sitecode, '/dp01/data/h2oStor/',heights[i], '/rtioMoleDryH2o', sep="")
    #h5read fcn reads a dataset in h5 file and returns an array containing data read
    CH4 <- h5read(hd.file, ch4.dir)
    CO2 <- h5read(hd.file, co2.dir)
    H2O <- h5read(hd.file, h2o.dir)
    if(i==1){
      #only grab timeEnd column from 1st height on list
      #why are we only grabbing timeEnd?
      #add mean data and timeEnd of measurement for given height to df
      df_CH4 <- dplyr::select(CH4, timeEnd, mean)
      df_CO2 <- dplyr::select(CO2, timeEnd, mean)
      df_H2O <- dplyr::select(H2O, timeEnd, mean)
      #rename df to match height of measurement
      colnames(df_CH4) <- c("timeEnd", paste('ch4', heights[i], sep="."))
      colnames(df_CO2) <- c("timeEnd", paste('co2', heights[i], sep="."))
      colnames(df_H2O) <- c("timeEnd", paste('h2o', heights[i], sep="."))
    }else{
      #add mean data of measurement for given height to df
      df_CH4 <- cbind(df_CH4, dplyr::select(CH4, mean))
      df_CO2 <- cbind(df_CO2, dplyr::select(CO2, mean))
      df_H2O <- cbind(df_H2O, dplyr::select(H2O, mean))
      #rename last column in df to match measurement height
      colnames(df_CH4)[i+1] <- paste('ch4', heights[i], sep=".")
      colnames(df_CO2)[i+1] <- paste('co2', heights[i], sep=".")
      colnames(df_H2O)[i+1] <- paste('h2o', heights[i], sep=".")
      
    }
    
  }
  
  #Add NEON level 4 CO2, sensible heat, latent heat, fluxes to df
  F_co2 <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxCo2/nsae", sep=""))
  F_H <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxTemp/nsae", sep=""))
  F_LE <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxH2o/nsae", sep=""))
  df_CO2$F_co2 = F_co2[,3]
  df_H2O$F_H = F_H[,3]
  df_H2O$F_LE = F_LE[,3]
  
  #remove looping var and flux df
  rm(CH4, CO2, F_co2, F_H, H2O, F_LE)
  
  #I am not sure that the location of these files are the same across sites!
  #grab NEON level 4 friction velocity for a given site
  Ufric  <- h5read(hd.file, paste("/", sitecode, "/dp04/data/fluxMome/turb", sep=""))
  
  #grab NEON level 1 air pressure at 25m 30min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/",max(test.df$name[test.df$group==paste("/", sitecode, "/dp01/data/presBaro", sep="")]),"/presAtm", sep=""))
  
  #grab NEON level 1 air temp at lowest tower position (assuming this is sruface air temp) 30min resolution for a given site
  temp  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/tempAirLvl/000_010_30m/temp", sep=""))
  #grab NEON level 1 horizontal wind speed data from top of tower at 30min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/000_060_30m/veloXaxsYaxsErth", sep=""))
  #grab NEON momentum roughness from footprint stats table
  #which of these is momentum roughness? Assuming veloZaxsHorSd for now based on range and mean 
  MomRough <- h5read(hd.file, paste("/", sitecode, "/dp04/data/foot/stat", sep=""))
  # Merge all data:
  totF <- df_H2O %>% left_join(df_CO2, by= 'timeEnd') %>% left_join(df_CH4 , by= 'timeEnd')
  #ustar used in aerodynamic profile and wind profile method estimation of eddy diffusivity
  totF$uStar <- Ufric$veloFric
  #air pressure used in aerodynamic and wind profile FG calculation
  totF$airpress <- P$mean
  #surface air temperature used in aerodynamic profile method estimation of eddy diffusivity
  totF$airtemp <-temp$mean
  #mean wind speed at measurement height used for wind profile method
  totF$uBar <- SoniWind$mean
  #roughness length used for wind profile method
  totF$z0 <- MomRough$veloZaxsHorSd
  
  
  return(totF)
}
