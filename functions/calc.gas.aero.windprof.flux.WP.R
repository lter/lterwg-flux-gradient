#' calc.gas.aero.windprof.flux.R
#'
#' Use the dataframe that includes k (diffisivity from the aerodynamic or wind profile) 
#' with the gas concentration difference between two tower heights (dConc)
#' to calculate the gas flux by the flux gradient method.
#'
#' Option to create bootstrapped uncertainty for the calculated fluxes 
#' by sampling from the mean & variance of gas concentrations 
#' and concentration differences. 
#'
#' @param min9.K list of data frames output of eddydiffAE or eddydiffWP
#' @param eddy.diff.name name of which eddy diffusivity to use
#' @param bootstrap: 1 to run bootstrap iterations for conc mean & sd or 0 to use provided mean dConc
#' @param nsamp: number of bootstrap iterations to run - default 1000
#'
#' @return list of data frames (one df per gas) containing fluxes calculate using AE and WP methods
#' @return 
#'
#' @author Alexis Helgeson, Jackie Matthes
#' 
calc.gas.aero.windprof.flux.WP <- function(min9.K, eddy.diff.name = "EddyDiff_WP", 
                            bootstrap, nsamp){
  
  # Calculate H2O fluxes.
  # Select only H2O conc data
  H2O <- min9.K[[which(names(min9.K) == "H2O")]]
  
  #calculate difference in tower heights
  H2O$dHeight <- as.numeric(H2O$TowerHeight_A) - as.numeric(H2O$TowerHeight_B)
  
  #set tower height difference = 0 to NA so it will be removed
  H2O[which(H2O$dHeight==0.00),"dHeight"] <- NA
  H2O.FG <- calc.eqn.aero.windprof.flux(min9 = H2O, eddy.diff.name = eddy.diff.name,
                                     bootstrap, nsamp)
  
  #calculate CO2 fluxes
  CO2 <- min9.K[[which(names(min9.K) == "CO2")]]
  #calculate difference in tower heights
  CO2$dHeight <- as.numeric(CO2$TowerHeight_A) - as.numeric(CO2$TowerHeight_B)
  #set tower height difference = 0 to NA so it will be removed
  CO2[which(CO2$dHeight==0.00),"dHeight"] <- NA
  CO2.FG <- calc.eqn.aero.windprof.flux(min9 = CO2, eddy.diff.name = eddy.diff.name,
                                     bootstrap, nsamp)
  
  #calculate CO2 fluxes
  CH4 <- min9.K[[which(names(min9.K) == "CH4")]]
  #calculate difference in tower heights
  CH4$dHeight <- as.numeric(CH4$TowerHeight_A) - as.numeric(CH4$TowerHeight_B)
  #set tower height difference = 0 to NA so it will be removed
  CH4[which(CH4$dHeight==0.00),"dHeight"] <- NA
  CH4.FG <- calc.eqn.aero.windprof.flux(min9 = CH4, eddy.diff.name = eddy.diff.name,
                                      bootstrap, nsamp)
  
  #add to list
  min9.FG.list <- list(H2O = H2O.FG, CO2 = CO2.FG, CH4 = CH4.FG)
  return(min9.FG.list)
}