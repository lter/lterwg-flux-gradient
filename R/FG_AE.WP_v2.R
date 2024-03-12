#' FG_AE.WP
#'
#' @param min9 gas concentration data frame, passed from computeFG.AE.WP.R
#' @param eddy.diff.name name of which eddy diffusivity to use, passed from computeFG.AE.WP.R
#'
#' @author Samuel Jurado and Alexis Helgeson
#'  v2: Modified by Camilo Rey, Roisin Commane March 12 2024
#'  
#'  Code inputs molar mixing ratio: # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
#'  Code outputs molar flux: # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
#'  min9$dConc is a mixing ratio not a concetration
#' 
FG_AE.WP <- function(min9, eddy.diff.name){

  data.cols <- c("dConc", "rhoa_kgm3", "dHeight")
  min9 <- min9[complete.cases(min9[,data.cols]),]
  diff.conc <- as.numeric(min9$dConc) # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
  diff.heights <- as.numeric(min9$dHeight) # m

  k <- as.numeric(min9[,paste0(eddy.diff.name)])# m2 s-1
  rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
  rho_mol <- rho*.0289 # mol m-3
  
  min9$FGmol <- rho_mol*(-k)*(diff.conc)/(diff.heights)# CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
  
  
  return(min9)
}
