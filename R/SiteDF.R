#' Site.DF
#'
#' @param sitecode NEON site code
#' @param hd.files file type h5 containg NEON site specific data
#' @param frequency high (9m or 2m) low (30m)
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
Site.DF <- function(hd.files, sitecode, frequency = "low"){
  
  #pulls 30m concentration, flux, and met data
  if(frequency == "low"){
    ALL.data = data.frame()
    #looping over all h5 files and extracting data over timeseries (startdate:enddate)
    for(i in 1:length(hd.files)){
      print(i)
      month.data <- hdf2df(hd.files[i], sitecode)
      ALL.data <-bind_rows(month.data,   ALL.data )
      
    }
    #remove looping variable
    rm(month.data)
    #add date/time column
    ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  }
  #pulls 9m CH4 concentration, 2m co2/h2o concentration
  if(frequency == "high"){
    ALL.data = data.frame()
    #looping over all h5 files and extracting data over timeseries (startdate:enddate)
    for(i in 1:length(hd.files)){
      print(i)
      month.data <- cont.HF(hd.files[i], sitecode)
      ALL.data <-bind_rows(month.data,   ALL.data )
      
    }
    #remove looping variable
    rm(month.data)
    #add date/time column
    #ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  }
  
  
  return( ALL.data )
}
