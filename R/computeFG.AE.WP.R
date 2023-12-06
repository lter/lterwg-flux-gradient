#' computeFG.AE.WP
#'
#' @param min9.K list of data frames output of eddydiffAE or eddydiffWP
#'
#' @return list of data frame containing fluxes calculate using AE and WP methods
#' 
#' @author Alexis Helgeson
#' 
computeFG.AE.WP <- function(min9.K){
  #calculate H2O fluxes
  H2O <- min9.K[[which(names(min9.K) == "H2O")]]
  #calculate difference in tower heights
  H2O$dHeight <- as.numeric(H2O$TowerHeight_A) - as.numeric(H2O$TowerHeight_B)
  H2O.FG <- FG_AE.WP(min9 = H2O)
  
  #calculate CO2 fluxes
  CO2 <- min9.K[[which(names(min9.K) == "CO2")]]
  #calculate difference in tower heights
  CO2$dHeight <- as.numeric(CO2$TowerHeight_A) - as.numeric(CO2$TowerHeight_B)
  CO2.FG <- FG_AE.WP(min9 = CO2)
  
  #calculate CO2 fluxes
  CH4 <- min9.K[[which(names(min9.K) == "CH4")]]
  #calculate difference in tower heights
  CH4$dHeight <- as.numeric(CH4$TowerHeight_A) - as.numeric(CH4$TowerHeight_B)
  CH4.FG <- FG_AE.WP(min9 = CH4)
  
  #add to list
  min9.FG.list <- list(H2O = H2O.FG, CO2 = CO2.FG, CH4 = CH4.FG)
  return(min9.FG.list)
}