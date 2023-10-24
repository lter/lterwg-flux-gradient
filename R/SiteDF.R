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
Site.DF <- function(folder, sitecode, frequency = "30min"){
  
  #pulls 30m concentration, flux, and met data
  if(frequency == "30min"){
    DATA <- Compile30min(folder = folder, sitecode = sitecode)
  }
    
  
  #pulls 9m CH4, CO2, H2O gas concentrations
  if(frequency == "9min"){
    DATA <- Compile9min(folder = folder, sitecode = sitecode)
  }
  
  #pulls 9m CH4, CO2, H2O gas concentrations
  if(frequency == "1min"){
    DATA <- Compile1min(folder = folder, sitecode = sitecode)
  }
  
  return(DATA)
}
