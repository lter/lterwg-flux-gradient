#' FG_AE.WP
#'
#' @param min9 gas concentration data frame
#' @param gas which gas are you computing
#' 
FG_AE.WP <- function(min9, gas){
  diff.conc <- as.numeric(min9$dConc)/0.0289 #umol CO2 mol-1 #divide by molar mass so units cancel
  diff.heights <- as.numeric(min9$dHeight) # m
  #does this eddy diffisivity need a direction correction?
  k <- as.numeric(min9$EddyDiff) #m-2 s-1
  rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
  
  min9$FG <- rho*(-k)*(diff.conc)/(diff.heights)
  
  min9.FG.list <- list(gas = min9)
  return(min9.FG.list)
}
