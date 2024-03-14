#' time.format
#'
#' @param timeEnd
#'
#' @return rounded timeEnd up to 30min
#'
time.format <- function(x){
  time <-as.data.frame(t(as.data.frame(strsplit(x, split = "T"))))
  time$time <- substr(time$V2 ,1, 8)
  time$timeEnd <- as.POSIXct(paste(time$V1, time$time, sep=" " ), tz='EST') 
  time$timeEnd <- round_date(time$timeEnd, "30 minutes")
  
  return(time$timeEnd)
}