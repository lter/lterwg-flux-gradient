#' Site.DF
#'
#' @param sitecode NEON site code
#' @param frequency high (9m or 2m) low (30m)
#' @param startdate start date of desired data
#' @param enddate enddate of desired data
#' @param folder containing monthly h5 files
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
Site.DF <- function(folder, sitecode, frequency = "low", startdate, enddate){
  
  # #pulls 30m concentration, flux, and met data
  # if(frequency == "low"){
  #   ALL.data = data.frame()
  #   #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  #   for(i in 1:length(hd.files)){
  #     print(i)
  #     month.data <- hdf2df(hd.files[i], sitecode)
  #     ALL.data <-bind_rows(ALL.data, month.data)
  #     
  #   }
  #   #remove looping variable
  #   rm(month.data)
  #   #add date/time column
  #   ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  # }
  # #pulls 9m CH4 concentration, 2m co2/h2o concentration
  # if(frequency == "high"){
  #   ALL.data = data.frame()
  #   #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  #   for(i in 1:length(hd.files)){
  #     print(i)
  #     month.data <- cont.HF(hd.files[i], sitecode)
  #     ALL.data <-bind_rows(ALL.data, month.data)
  #     
  #   }
  #   #remove looping variable
  #   rm(month.data)
  #   #add date/time column
  #   #ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  # }
  
    ALL.data = list()
  
    #looping over all h5 files and extracting data over timeseries (startdate:enddate)
    for(i in 1:length(folder)){
      hd.file <-list.files(path = file.path(folder[i]), pattern="\\.h5$", full.names = T)
      print(i)
      # month.data <- met.cont.30m(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
      # month.data <- cont.9m(hd.file = hd.file, sitecode = sitecode)
      month.data <- met.1m(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
      
      ALL.data[[i]] <- month.data

    }
    #remove looping variable
    rm(month.data)
    
    #rbind similar df
    var.names <- names(ALL.data[[1]])
    for (k in 1:length(var.names)) {
      Name <- var.names[k]
      
      do
    }
  
    #grab relative humidity
    RH <- loadByProduct("DP1.00098.001", site="KONZ",
                        timeIndex=1, package="basic",
                        startdate=startdate, enddate=enddate,
                        check.size=F)
    RH <- RH$RH_1min %>%
      filter(horizontalPosition == "000") %>%
      select(verticalPosition, startDateTime, endDateTime, RHMean, RHFinalQF)

    # #grab 2D horizontal wind speed
    # WS2D <- loadByProduct("DP1.00001.001", site="KONZ",
    #                       timeIndex=30, package="basic",
    #                       startdate=startdate, enddate=enddate,
    #                       check.size=F)
    # WS2D <- WS2D$twoDWSD_30min %>%
    #   select(verticalPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
    
    DATA <- list(Cont = ALL.data, RH = RH)
  
  return(DATA)
}
