#' FG_AE.WP
#'
#' @param min9 gas concentration data frame, passed from computeFG.AE.WP.R
#' @param eddy.diff.name name of which eddy diffusivity to use, passed from computeFG.AE.WP.R
#'
#'  Code inputs molar mixing ratio: # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
#'  Code outputs molar flux: # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
#'  min9$dConc is a mixing ratio not a concetration
#'  
#' @author Samuel Jurado, Alexis Helgeson, Camilo Rey, and Roisin Commane
#' 
FG_AE.WP <- function(min9, eddy.diff.name){
  #remove NAs
  data.cols <- c("dConc", "rhoa_kgm3", "dHeight")
  min9 <- min9[complete.cases(min9[,data.cols]),]
  diff.conc <- as.numeric(min9$dConc) # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
  diff.heights <- as.numeric(min9$dHeight) # m
  #does this eddy diffisivity need a direction correction?
  #DEPRECIATED CODE
  # k <- as.numeric(min9$EddyDiff) #m-2 s-1
  #select for desired eddy diffusivity using eddy.diff.name
  k <- as.numeric(min9[,paste0(eddy.diff.name)])
  rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
  rho_mol <- rho*.0289 # mol m-3
  
  min9$FG <- rho_mol*(-k)*(diff.conc)/(diff.heights) # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
  
  return(min9)
}
