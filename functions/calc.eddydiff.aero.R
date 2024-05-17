#' calc.eddydiff.aero.R
#'
#' @param sitecode NEON site code
#' @param min9 9min interpolated data file for given site
#'
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez
#'
#' @return list of gas concentration dataframes containing variables associated with aerodynamic eddy diffusivity calculation
#' 
calc.eddydiff.aero <- function(sitecode, min9){
  #currently hard coded to calculate for all gas concentrations
  #grab H2O gas concentration
  H2O <- min9[[which(names(min9) == "H2O")]]
  #remove NAs from data columns used in calculation for AE this includes: z_displ_calc
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("z_displ_calc")
  H2O <- H2O[complete.cases(H2O[,data.cols]),]
  #calculate Obukhov length and stability parameters
  H2O <- calculate.stability.correction(gas = H2O)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  H2O$GeometricMean_AB <- sqrt(as.numeric(H2O$TowerHeight_A)*as.numeric(H2O$TowerHeight_B))
  #we want to calculate eddy diffusivity for each height
  H2O$EddyDiff = (k*as.numeric(H2O$ustar_interp)*as.numeric(H2O$GeometricMean_AB))/as.numeric(H2O$phih)
  # H2O$EddyDiff_phih = (k*as.numeric(H2O$ustar_interp)*as.numeric(H2O$GeometricMean_AB))/as.numeric(H2O$phih)
  #try calculating eddy diffusivity with wind shear parameter (phim) instead of heat parameter (phih)
  # H2O$EddyDiff_phim = (k*as.numeric(H2O$ustar_interp)*as.numeric(H2O$GeometricMean_AB))/as.numeric(H2O$phim)
  
  #grab CO2 gas concentration
  CO2 <- min9[[which(names(min9) == "CO2")]]
  #remove NAs
  CO2 <- CO2[complete.cases(CO2[,data.cols]),]
  #calculate obukhov length and stability parameters
  CO2 <- calculate.stability.correction(gas = CO2)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  CO2$GeometricMean_AB <- sqrt(as.numeric(CO2$TowerHeight_A)*as.numeric(CO2$TowerHeight_B))
  #we want to calculate eddy diffusivity for each height
  CO2$EddyDiff = (k*as.numeric(CO2$ustar_interp)*as.numeric(CO2$GeometricMean_AB))/as.numeric(CO2$phih)
  # CO2$EddyDiff_phih = (k*as.numeric(CO2$ustar_interp)*as.numeric(CO2$GeometricMean_AB))/as.numeric(CO2$phih)
  # #try calculating eddy diffusivity with wind shear parameter (phim) instead of heat parameter (phih)
  # CO2$EddyDiff_phim = (k*as.numeric(CO2$ustar_interp)*as.numeric(CO2$GeometricMean_AB))/as.numeric(CO2$phim)
  
  #grab CO2 gas concentration
  CH4 <- min9[[which(names(min9) == "CH4")]]
  #remove NAs
  CH4 <- CH4[complete.cases(CH4[,data.cols]),]
  #calculate obukhov length and stability parameters
  CH4 <- calculate.stability.correction(gas = CH4)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  CH4$GeometricMean_AB <- sqrt(as.numeric(CH4$TowerHeight_A)*as.numeric(CH4$TowerHeight_B))
  #we want to calculate eddy diffusivity for each height
  CH4$EddyDiff = (k*as.numeric(CH4$ustar_interp)*as.numeric(CH4$GeometricMean_AB))/as.numeric(CH4$phih)
  # CH4$EddyDiff_phih = (k*as.numeric(CH4$ustar_interp)*as.numeric(CH4$GeometricMean_AB))/as.numeric(CH4$phih)
  # #try calculating eddy diffusivity with wind shear parameter (phim) instead of heat parameter (phih)
  # CH4$EddyDiff_phim = (k*as.numeric(CH4$ustar_interp)*as.numeric(CH4$GeometricMean_AB))/as.numeric(CH4$phim)
  
  
  #add to list
  min9.K.AE.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.AE.list)
}