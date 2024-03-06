#' eddydiffAE
#'
#' @param sitecode NEON site code
#' @param min9 9min interpolated data file for given site
#'
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez
#'
#' @return list of gas concentration dataframes containing variables associated with aerodynamic eddy diffusivity calculation
#' 
eddydiffAE <- function(sitecode, min9){
  #currently hard coded to calculate for all gas concentrations
  #grab H2O gas concentration
  H2O <- min9[[which(names(min9) == "H2O")]]
  #remove NAs from data columns used in calculation for AE this includes: P_kPa, Tair1, H_turb_interp, LE_turb_interp, ustar_interp, z_displ_calc
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("P_kPa", "Tair1", "H_turb_interp", "LE_turb_interp", "ustar_interp", "z_displ_calc")
  #remove NAs
  H2O <- H2O[complete.cases(H2O[,data.cols]),]
  #calculate obukov length
  MO.vars <- MOlength(press = H2O$P_kPa, temp = H2O$Tair1, H = H2O$H_turb_interp, LE = H2O$LE_turb_interp, velofric = H2O$ustar_interp)
  #add OB params to data frame for eddy diffusivty calculation
  H2O <- cbind(H2O, MO.vars$rho, MO.vars$vpotflux, MO.vars$L)
  #rename columns
  old.names <- grep("MO", names(H2O))
  names(H2O)[old.names[1]] <- "rho"
  names(H2O)[old.names[2]] <- "vpotflux"
  names(H2O)[old.names[3]] <- "L"
  #DEPRECIATED CODE: TOWER HEIGHT NOW ADDED IN flow.formatConcentrationDiffs.R
  # #grab only tower heights and positions for matching
  # tower.heights <- attr %>% select(DistZaxsLvlMeasTow, TowerPosition)
  # #adding place holder identifier to create tower height columns
  # H2O$TowerHeight_A <- "hold"
  # H2O$TowerHeight_B <- "hold"
  # for(i in 1:dim(attr)[1]){
  #   #loop over position A
  #   H2O[which(H2O$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  #   #loop over position B
  #   H2O[which(H2O$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  # }
  #grab canopy height
  #want to calculate stability param with respect to tower positions A & B
  #want to use heights where z > d
  #want to add flag when z < d
  H2O$MO.param <- "hold"
  H2O$phih <- "hold"
  H2O$phim <- "hold"
  #DEPRECIATED CODE KEEP FOR NOW
  #MO.param <- list()
  #phih.heights <- list()
  #phim.heights <- list()
  for(k in 1:dim(H2O)[1]){
    #pull out row for looping
    row.loop <- H2O[k,]
    #set z as mean of tower positions A & B
    z = mean(c(as.numeric(row.loop$TowerHeight_A), as.numeric(row.loop$TowerHeight_B)))
    #set d as canopy displacement height
    d = as.numeric(row.loop$z_displ_calc)
    #calculate obukhov parameter
    H2O[k,"MO.param"] <- (z - d)/as.numeric(row.loop$L)
    mo.param <- as.numeric(H2O[k,"MO.param"])
    #calculate stability correction for each row
    if(mo.param > 0){
      zt=0
      phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
      phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = -5 * mo.param
      psih = psim
      H2O[k,"phih"] <- phih
      H2O[k,"phim"] <- phim
    }else{
      zt = (1-16*mo.param)^0.25
      phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
      phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
      psih = 2*log((1+zt^2)/2)
      H2O[k,"phih"] <- phih
      H2O[k,"phim"] <- phim
    }
    #DEPRECIATED CODE KEEP FOR NOW
    #MO.param[[k]] <- (z - d)/as.numeric(row.loop$L)
    #calculate stability correction for each height
    #MO.param.height <- MO.param[[k]]
    #momentum stability parameters
    #phih.loop <- c()
    #phim.loop <- c()
    # for(j in 1:length(MO.param.height)){
    #   if(MO.param.height[j] > 0){
    #     zt=0
    #     phim=1+5*MO.param.height[j] #Eq 4.37 Lee. Stability coefficient for wind shear
    #     phih=1+5*MO.param.height[j] #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
    #     psim = -5 * MO.param.height[j]
    #     psih = psim
    #     phih.loop[j] <- phih
    #     phim.loop[j] <- phim
    #   }else{
    #     zt = (1-16*MO.param.height[j])^0.25
    #     phim=(1-16*MO.param.height[j])^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
    #     phih=(1-16*MO.param.height[j])^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
    #     psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
    #     psih = 2*log((1+zt^2)/2)
    #     phih.loop[j] <- phih
    #     phim.loop[j] <- phim
    #   }
    # }
    # 
    # phih.heights[[k]] <- phih.loop
    # phim.heights[[k]] <- phim.loop
    
  }
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
  #calculate obukov length
  MO.vars <- MOlength(press = CO2$P_kPa, temp = CO2$Tair1, H = CO2$H_turb_interp, LE = CO2$LE_turb_interp, velofric = CO2$ustar_interp)
  #add OB params to data frame for eddy diffusivty calculation
  CO2 <- cbind(CO2, MO.vars$rho, MO.vars$vpotflux, MO.vars$L)
  #rename columns
  old.names <- grep("MO", names(CO2))
  names(CO2)[old.names[1]] <- "rho"
  names(CO2)[old.names[2]] <- "vpotflux"
  names(CO2)[old.names[3]] <- "L"
  #DEPRECIATED CODE: TOWER HEIGHT NOW ADDED IN flow.formatConcentrationDiffs.R
  # #adding place holder identifier to create tower height columns
  # CO2$TowerHeight_A <- "hold"
  # CO2$TowerHeight_B <- "hold"
  # for(i in 1:dim(attr)[1]){
  #   #loop over position A
  #   CO2[which(CO2$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  #   #loop over position B
  #   CO2[which(CO2$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  # }
  #want to calculate stability param with respect to tower positions A & B
  #want to use heights where z > d
  #want to add flag when z < d
  CO2$MO.param <- "hold"
  CO2$phih <- "hold"
  CO2$phim <- "hold"
  for(k in 1:dim(CO2)[1]){
    #pull out row for looping
    row.loop <- CO2[k,]
    #set z as mean of tower positions A & B
    z = mean(c(as.numeric(row.loop$TowerHeight_A), as.numeric(row.loop$TowerHeight_B)))
    #set d as canopy displacement height
    d = as.numeric(row.loop$z_displ_calc)
    #calculate obukhov parameter
    CO2[k,"MO.param"] <- (z - d)/as.numeric(row.loop$L)
    mo.param <- as.numeric(CO2[k,"MO.param"])
    #calculate stability correction for each row
    if(mo.param > 0){
      zt=0
      phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
      phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = -5 * mo.param
      psih = psim
      CO2[k,"phih"] <- phih
      CO2[k,"phim"] <- phim
    }else{
      zt = (1-16*mo.param)^0.25
      phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
      phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
      psih = 2*log((1+zt^2)/2)
      CO2[k,"phih"] <- phih
      CO2[k,"phim"] <- phim
    }
  }
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
  #calculate obukov length
  MO.vars <- MOlength(press = CH4$P_kPa, temp = CH4$Tair1, H = CH4$H_turb_interp, LE = CH4$LE_turb_interp, velofric = CH4$ustar_interp)
  #add OB params to data frame for eddy diffusivty calculation
  CH4 <- cbind(CH4, MO.vars$rho, MO.vars$vpotflux, MO.vars$L)
  #rename columns
  old.names <- grep("MO", names(CH4))
  names(CH4)[old.names[1]] <- "rho"
  names(CH4)[old.names[2]] <- "vpotflux"
  names(CH4)[old.names[3]] <- "L"
  #DEPRECIATED CODE: TOWER HEIGHT NOW ADDED IN flow.formatConcentrationDiffs.R
  # #adding place holder identifier to create tower height columns
  # CH4$TowerHeight_A <- "hold"
  # CH4$TowerHeight_B <- "hold"
  # for(i in 1:dim(attr)[1]){
  #   #loop over position A
  #   CH4[which(CH4$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  #   #loop over position B
  #   CH4[which(CH4$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
  # }
  #want to calculate stability param with respect to tower positions A & B
  #want to use heights where z > d
  #want to add flag when z < d
  CH4$MO.param <- "hold"
  CH4$phih <- "hold"
  CH4$phim <- "hold"
  for(k in 1:dim(CH4)[1]){
    #pull out row for looping
    row.loop <- CH4[k,]
    #set z as mean of tower positions A & B
    z = mean(c(as.numeric(row.loop$TowerHeight_A), as.numeric(row.loop$TowerHeight_B)))
    #set d as canopy displacement height
    d = as.numeric(row.loop$z_displ_calc)
    #calculate obukhov parameter
    CH4[k,"MO.param"] <- (z - d)/as.numeric(row.loop$L)
    mo.param <- as.numeric(CH4[k,"MO.param"])
    #calculate stability correction for each row
    if(mo.param > 0){
      zt=0
      phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
      phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = -5 * mo.param
      psih = psim
      CH4[k,"phih"] <- phih
      CH4[k,"phim"] <- phim
    }else{
      zt = (1-16*mo.param)^0.25
      phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
      phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
      psih = 2*log((1+zt^2)/2)
      CH4[k,"phih"] <- phih
      CH4[k,"phim"] <- phim
    }
  }
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