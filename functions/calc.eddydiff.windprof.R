#' calc.eddydiff.windprof.R
#'
#' @param sitecode  NEON site code
#' @param min9 9min interpolated data file for given site
#'
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez
#'
#' @return list of gas concentration dataframes containing variables associated with wind profile eddy diffusivity calculation
#' 
calc.eddydiff.windprof <- function(sitecode, min9){
  
  #currently hard coded to calculate for all gas concentrations
  #grab H2O
  H2O <- min9[[which(names(min9) == "H2O")]]
  #remove NAs from data columns used in calculation for WP this includes: ubar1:n and roughLength_interp
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("roughLength_interp", grep("ubar", names(H2O), value = TRUE))
  #remove NAs
  #H2O <- H2O[complete.cases(H2O[,data.cols]),]
  #calculate obukhov length and stability parameters
  H2O <- calculate.stability.correction(gas = H2O)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  H2O$GeometricMean_AB <- sqrt(as.numeric(H2O$TowerHeight_A)*as.numeric(H2O$TowerHeight_B))
  
  #create column for store wind profile eddy diffusivity
  H2O$EddyDiff <- "hold"
  #TO DO: REFORMAT ubar COLUMNS SO THAT WE CAN SELECT FOR CORRECT ubar USING TowerPosition
  for(j in 1:dim(H2O)[1]){
    c.name <- paste0("ubar", as.character(H2O[j,"TowerPosition_A"]))
    ubar = as.numeric(H2O[j,grep(c.name, names(H2O))])
    z = as.numeric(H2O[j,"TowerHeight_A"])
    
    #H2O[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(H2O[j,"GeometricMean_AB"])/(log(z/as.numeric(H2O[j,"roughLength_interp"]))*H2O[j,"phih"]))

    H2O[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(H2O[j,"GeometricMean_AB"])/(log(z/as.numeric(H2O[j,"roughLength_interp"]))))
    
  }
  #set EddyDiff as numeric
  H2O$EddyDiff <- as.numeric(H2O$EddyDiff)
  #grab CO2
  CO2 <- min9[[which(names(min9) == "CO2")]]
  #remove NAs
  #CO2 <- CO2[complete.cases(CO2[,data.cols]),]
  #calculate obukhov length and stability columns
  CO2 <- calculate.stability.correction(gas = CO2)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  CO2$GeometricMean_AB <- sqrt(as.numeric(CO2$TowerHeight_A)*as.numeric(CO2$TowerHeight_B))
  
  #create column for store wind profile eddy diffusivity
  CO2$EddyDiff <- "hold"
  for(j in 1:dim(CO2)[1]){
    c.name <- paste0("ubar", as.character(CO2[j,"TowerPosition_A"]))
    ubar = as.numeric(CO2[j,grep(c.name, names(CO2))])
    z = as.numeric(CO2[j,"TowerHeight_A"])
    
    CO2[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(CO2[j,"GeometricMean_AB"])/(log(z/as.numeric(CO2[j,"roughLength_interp"]))*CO2[j,"phih"]))
  }
  #set EddyDiff as numeric
  CO2$EddyDiff <- as.numeric(CO2$EddyDiff)
  #grab CH4
  CH4 <- min9[[which(names(min9) == "CH4")]]
  #remove NAs
  #CH4 <- CH4[complete.cases(CH4[,data.cols]),]
  #calculate obukhov length and stability parameter
  CH4 <- calculate.stability.correction(gas = CH4)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  CH4$GeometricMean_AB <- sqrt(as.numeric(CH4$TowerHeight_A)*as.numeric(CH4$TowerHeight_B))
  
  #create column for store wind profile eddy diffusivity
  CH4$EddyDiff <- "hold"
  for(j in 1:dim(CH4)[1]){
    c.name <- paste0("ubar", as.character(CH4[j,"TowerPosition_A"]))
    ubar = as.numeric(CH4[j,grep(c.name, names(CH4))])
    z = as.numeric(CH4[j,"TowerHeight_A"])
    
    CH4[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(CH4[j,"GeometricMean_AB"])/(log(z/as.numeric(CH4[j,"roughLength_interp"]))*CH4[j,"phih"]))
  }
  #set EddyDiff as numeric
  CH4$EddyDiff <- as.numeric(CH4$EddyDiff)
  #add to list
  min9.K.WP.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.WP.list)
}