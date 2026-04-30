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

  calc_wp_diffusivity <- function(gas_df, k_const){
    gas_df$EddyDiff_WP <- NA_real_
    ubar_cols <- grep("^ubar", names(gas_df), value = TRUE)

    if(length(ubar_cols) == 0){
      return(gas_df)
    }

    ubar_lookup <- suppressWarnings(as.integer(sub("^ubar", "", ubar_cols)))
    tower_pos <- suppressWarnings(as.integer(as.character(gas_df$TowerPosition_A)))
    ubar_idx <- match(tower_pos, ubar_lookup)
    valid_idx <- which(!is.na(ubar_idx))

    if(length(valid_idx) == 0){
      return(gas_df)
    }

    ubar_mat <- as.matrix(gas_df[, ubar_cols, drop = FALSE])
    ubar <- rep(NA_real_, nrow(gas_df))
    ubar[valid_idx] <- ubar_mat[cbind(valid_idx, ubar_idx[valid_idx])]

    zd <- as.numeric(gas_df$effective_h)
    zo <- as.numeric(gas_df$roughLength_calc)
    phih <- as.numeric(gas_df$phih)
    gas_df$EddyDiff_WP <- ((k_const^2) * ubar * zd) / (log(zd / zo) * phih)
    gas_df
  }
  
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
  H2O <- calc_wp_diffusivity(H2O, k)

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
  CO2 <- calc_wp_diffusivity(CO2, k)
  
  
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
  #EDDY DIFF AERO
  CH4$EddyDiff = (k*as.numeric(CH4$ustar_interp)*as.numeric(CH4$effective_h))/as.numeric(CH4$phih)
  
  # EDDY DIFF WP: create column for store wind profile eddy diffusivity with Wind Profiler Method
  CH4 <- calc_wp_diffusivity(CH4, k)
  
  #add to list
  min9.K.AE.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.AE.list)
}
