#' eddydiffWP
#'
#' @param sitecode  NEON site code
#' @param min9 9min interpolated data file for given site
#' @param attr site attribute file
#' 
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez
#'
#' @return list of gas concentration dataframes containing variables associated with wind profile eddy diffusivity calculation
#' 
eddydiffWP <- function(sitecode, min9, attr){
  #currently hard coded to calculate for all gas concentrations
  #grab H2O
  H2O <- min9[[which(names(min9) == "H2O")]]
  #grab only tower heights and positions for matching
  tower.heights <- attr.df %>% select(DistZaxsLvlMeasTow, TowerPosition)
  #adding place holder identifier to create tower height columns
  H2O$TowerHeight_A <- "hold"
  H2O$TowerHeight_B <- "hold"
  for(i in 1:dim(attr.df)[1]){
    #loop over position A
    H2O[which(H2O$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
    #loop over position B
    H2O[which(H2O$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  }
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  #why are we using geometric mean instead of regular mean?
  H2O$GeometricMean_AB <- sqrt(as.numeric(H2O$TowerHeight_A)*as.numeric(H2O$TowerHeight_B))
  
  #create column for store wind profile eddy diffusivity
  H2O$EddyDiff <- "hold"
  for(j in 1:dim(H2O)[1]){
    c.name <- paste0("ubar", as.character(H2O[j,"TowerPosition_A"]))
    ubar = as.numeric(H2O[j,grep(c.name, names(H2O))])
    z = as.numeric(H2O[j,"TowerHeight_A"])
    
    H2O[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(H2O[j,"GeometricMean_AB"])/log(z/as.numeric(H2O[j,"roughLength_interp"])))
  }
  
  #grab CO2
  CO2 <- min9[[which(names(min9) == "CO2")]]
  #adding place holder identifier to create tower height columns
  CO2$TowerHeight_A <- "hold"
  CO2$TowerHeight_B <- "hold"
  for(i in 1:dim(attr.df)[1]){
    #loop over position A
    CO2[which(CO2$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
    #loop over position B
    CO2[which(CO2$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  }
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
    
    CO2[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(CO2[j,"GeometricMean_AB"])/log(z/as.numeric(CO2[j,"roughLength_interp"])))
  }
  
  #grab CH4
  CH4 <- min9[[which(names(min9) == "CH4")]]
  #adding place holder identifier to create tower height columns
  CH4$TowerHeight_A <- "hold"
  CH4$TowerHeight_B <- "hold"
  for(i in 1:dim(attr.df)[1]){
    #loop over position A
    CH4[which(CH4$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
    #loop over position B
    CH4[which(CH4$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  }
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
    
    CH4[j,"EddyDiff"] <- ((k^2)*ubar*as.numeric(CH4[j,"GeometricMean_AB"])/log(z/as.numeric(CH4[j,"roughLength_interp"])))
  }
  
  #add to list
  min9.K.WP.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.WP.list)
}