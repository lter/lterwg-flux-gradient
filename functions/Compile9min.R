#' Compile9min 
#'
#' @param h5files list containing monthly h5 files
#' @param sitecode NEON site code 
#'
#' @return list of dataframes containing CH4, CO2, H2O gas concentration at 9 min resolution
#'
#' @author Alexis Helgeson
Compile9min <- function(h5files, sitecode){
  #create empty list to store monthly data
  ALL.data = list()
  
  #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  for(i in 1:length(h5files)){
    hd.file <- h5files[i]
    print(i)
    month.data <- cont.9min(hd.file = hd.file, sitecode = sitecode)
    
    ALL.data[[i]] <- month.data
    
  }
  #remove looping variable
  rm(month.data)
  
  #rbind similar df FOR 9M GAS
  CH4.all <- data.frame()
  CO2.all <- data.frame()
  H2O.all <- data.frame()

  for (k in 1:length(ALL.data)) {
    grabMonth <- ALL.data[[k]]
    #grab ch4 for all months and combine into one df
    grabCH4 <- grabMonth[[which(names(grabMonth) == "CH4")]]
    CH4.all <- bind_rows(CH4.all, grabCH4)
    #grab co2 for all months and combine into one df
    grabCO2 <- grabMonth[[which(names(grabMonth) == "CO2")]]
    CO2.all <- bind_rows(CO2.all, grabCO2)
    #grab h2o for all months and combine into one df
    grabH2O <- grabMonth[[which(names(grabMonth) == "H2O")]]
    H2O.all <- bind_rows(H2O.all, grabH2O)

  }
  
  #FOR 9M GAS
  DATA <- list(CH4 = CH4.all, CO2 = CO2.all, H2O = H2O.all)
  
  return(DATA)
}