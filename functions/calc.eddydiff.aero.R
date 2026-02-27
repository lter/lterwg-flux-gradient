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
  
  ### H2O ###
  #grab H2O gas concentration
  H2O <- min9[[which(names(min9) == "H2O")]]
  #remove NAs from data columns used in calculation for AE this includes: z_displ_calc
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("z_displ_calc")
  #H2O <- H2O[complete.cases(H2O[,data.cols]),]
  #calculate Obukhov length and stability parameters
  H2O <- calc.stability.correction(gas = H2O)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4

  # EDDY DIFF AERO (include stability correction): 
  H2O$EddyDiff = (k*as.numeric(H2O$ustar_interp)*as.numeric(H2O$effective_h))/as.numeric(H2O$phih) 
  #H2O$EddyDiff = (k*as.numeric(H2O$ustar_interp)*as.numeric(H2O$effective_h))
  
  # EDDY DIFF WP: create column to store wind profile eddy diffusivity with Wind Profiler Method
  H2O$EddyDiff_WP <- "hold"
  
  #TO DO: REFORMAT ubar COLUMNS SO THAT WE CAN SELECT FOR CORRECT ubar USING TowerPosition
  for(j in 1:dim(H2O)[1]){
    c.name <- paste0("ubar", as.character(H2O[j,"TowerPosition_A"]))
    ubar = as.numeric(H2O[j,grep(c.name, names(H2O))])
    zd = as.numeric(H2O[j,"TowerHeight_A"])
    zo= as.numeric(H2O[j,"roughLength_calc"])
    phih=H2O[j,"phih"]
    
    H2O[j,"EddyDiff_WP"] <- ((k^2)*ubar*zd)/(log(zd/zo)*phih)
  }
  #set EddyDiff as numeric
  H2O$EddyDiff_WP <- as.numeric(H2O$EddyDiff_WP)
  
  ### CO2 ###
  
  #grab CO2 gas concentration
  CO2 <- min9[[which(names(min9) == "CO2")]]
  #remove NAs
  #CO2 <- CO2[complete.cases(CO2[,data.cols]),]
  #calculate obukhov length and stability parameters
  CO2 <- calc.stability.correction(gas = CO2)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4

  # EDDY DIFF AERO:
  CO2$EddyDiff = (k*as.numeric(CO2$ustar_interp)*as.numeric(CO2$effective_h))/as.numeric(CO2$phih)
  
  # EDDY DIFF WP: create column for store wind profile eddy diffusivity with Wind Profiler Method
  CO2$EddyDiff_WP <- "hold"
  #TO DO: REFORMAT ubar COLUMNS SO THAT WE CAN SELECT FOR CORRECT ubar USING TowerPosition
  for(j in 1:dim(CO2)[1]){
    c.name <- paste0("ubar", as.character(CO2[j,"TowerPosition_A"]))
    ubar = as.numeric(CO2[j,grep(c.name, names(CO2))])
    zd = as.numeric(CO2[j,"TowerHeight_A"])
    zo= as.numeric(CO2[j,"roughLength_calc"])
    phih=CO2[j,"phih"]
    
    CO2[j,"EddyDiff_WP"] <- ((k^2)*ubar*zd)/(log(zd/zo)*phih)
  }
  #set EddyDiff as numeric
  CO2$EddyDiff_WP <- as.numeric(CO2$EddyDiff_WP)
  
  
  ### CH4 ###
  #grab CH4 gas concentration
  CH4 <- min9[[which(names(min9) == "CH4")]]
  #remove NAs
  #CH4 <- CH4[complete.cases(CH4[,data.cols]),]
  #calculate obukhov length and stability parameters
  CH4 <- calc.stability.correction(gas = CH4)
  #calculate eddy diffusivity
  #we need: von karman constant (k), friction velocity (u_star), geometric mean of upper and lower heights (z_g), stability parameter (phih)
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  CH4$effective_h <- sqrt(as.numeric(CH4$TowerHeight_A)*as.numeric(CH4$TowerHeight_B))
  #EDDY DIFF AERO
  CH4$EddyDiff = (k*as.numeric(CH4$ustar_interp)*as.numeric(CH4$effective_h))/as.numeric(CH4$phih)
  
  # EDDY DIFF WP: create column for store wind profile eddy diffusivity with Wind Profiler Method
 CH4$EddyDiff_WP <- "hold"
  #TO DO: REFORMAT ubar COLUMNS SO THAT WE CAN SELECT FOR CORRECT ubar USING TowerPosition
  for(j in 1:dim(CH4)[1]){
    c.name <- paste0("ubar", as.character(CH4[j,"TowerPosition_A"]))
    ubar = as.numeric(CH4[j,grep(c.name, names(CH4))])
    zd = as.numeric(CH4[j,"TowerHeight_A"])
    zo= as.numeric(CH4[j,"roughLength_calc"])
    phih=CH4[j,"phih"]
    
   CH4[j,"EddyDiff_WP"] <- ((k^2)*ubar*zd)/(log(zd/zo)*phih)
  }
 
  #set EddyDiff as numeric
 CH4$EddyDiff_WP <- as.numeric(CH4$EddyDiff_WP)
  
  #add to list
  min9.K.AE.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.AE.list)
}