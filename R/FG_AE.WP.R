#' FG_AE.WP
#'
#' @param min9 gas concentration data frame, passed from computeFG.AE.WP.R
#' @param eddy.diff.name name of which eddy diffusivity to use, passed from computeFG.AE.WP.R
#'
#' @author Samuel Jurado and Alexis Helgeson
#' 
FG_AE.WP <- function(min9, eddy.diff.name){
  diff.conc <- as.numeric(min9$dConc)/0.0289 #umol CO2 mol-1 #divide by molar mass so units cancel
  diff.heights <- as.numeric(min9$dHeight) # m
  #does this eddy diffisivity need a direction correction?
  #DEPRECIATED CODE
  # k <- as.numeric(min9$EddyDiff) #m-2 s-1
  #select for desired eddy diffusivity using eddy.diff.name
  k <- as.numeric(min9[,paste0(eddy.diff.name)])
  rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
  
  min9$FG <- rho*(-k)*(diff.conc)/(diff.heights)
  
  return(min9)
}
