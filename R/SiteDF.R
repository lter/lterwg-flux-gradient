#' Site.DF
#'
#' @param sitecode NEON site code
#' @param hd.files file type h5 containg NEON site specific data
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Alexis Helgeson
<<<<<<< HEAD


=======
>>>>>>> 7ee6e3f10b85a8472020fd11397ff23f7ae80a82
Site.DF <- function(hd.files, sitecode){
  
  ALL.data = data.frame()
  #looping over all h5 files and extracting data over timeseries (startdate:enddate)
<<<<<<< HEAD

=======
>>>>>>> 7ee6e3f10b85a8472020fd11397ff23f7ae80a82
  for(i in 1:length(hd.files)){
    print(i)
    month.data <- hdf2df(hd.files[i], sitecode)
    ALL.data <-smartbind(month.data,   ALL.data )
<<<<<<< HEAD
    rm( month.data)
=======
>>>>>>> 7ee6e3f10b85a8472020fd11397ff23f7ae80a82
  }
  #add date/time column
  ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  
  return( ALL.data )
}
