#' compile.neon.data
#'
#' @param sitecode NEON site code
#' @param h5files list of h5 filepaths
#' @param frequency high (9m or 2m) low (30m)
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
compile.neon.data <- function(h5files, sitecode, frequency = "30min"){
  
  #pulls 30m concentration, flux, and met data
  if(frequency == "30min"){
    DATA <- compile.neon.data.30min(h5files = h5files, sitecode = sitecode)
  }
    
  
  #pulls 9m CH4, CO2, H2O gas concentrations
  if(frequency == "9min"){
    DATA <- compile.neon.data.9min.6min(h5files = h5files, sitecode = sitecode)
  }
  
  #pulls 9m CH4, CO2, H2O gas concentrations
  if(frequency == "1min"){
    DATA <- compile.neon.data.1min(h5files = h5files, sitecode = sitecode)
  }
  
  return(DATA)
}
