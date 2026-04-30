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
  calc_wp_eddy_diff <- function(gas_df, k_const){
    gas_df$GeometricMean_AB <- sqrt(as.numeric(gas_df$TowerHeight_A) * as.numeric(gas_df$TowerHeight_B))
    gas_df$EddyDiff <- NA_real_
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

    z <- as.numeric(gas_df$TowerHeight_A)
    phih <- as.numeric(gas_df$phih)
    rough_length <- as.numeric(gas_df$roughLength_interp)
    gas_df$EddyDiff <- ((k_const^2) * ubar * as.numeric(gas_df$GeometricMean_AB)) /
      (log(z / rough_length) * phih)
    gas_df
  }
  
  #currently hard coded to calculate for all gas concentrations
  #grab H2O
  H2O <- min9[[which(names(min9) == "H2O")]]
  #remove NAs from data columns used in calculation for WP this includes: ubar1:n and roughLength_interp
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("roughLength_interp", grep("ubar", names(H2O), value = TRUE))
  #remove NAs
  #H2O <- H2O[complete.cases(H2O[,data.cols]),]
  #calculate obukhov length and stability parameters
  H2O <- calc.stability.correction(gas = H2O)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  H2O <- calc_wp_eddy_diff(H2O, k)
  #grab CO2
  CO2 <- min9[[which(names(min9) == "CO2")]]
  #remove NAs
  #CO2 <- CO2[complete.cases(CO2[,data.cols]),]
  #calculate obukhov length and stability columns
  CO2 <- calc.stability.correction(gas = CO2)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  CO2 <- calc_wp_eddy_diff(CO2, k)
  
  #grab CH4
  CH4 <- min9[[which(names(min9) == "CH4")]]
  #remove NAs
  #CH4 <- CH4[complete.cases(CH4[,data.cols]),]
  #calculate obukhov length and stability parameter
  CH4 <- calc.stability.correction(gas = CH4)
  #calculate eddy diffusivty using WP
  #assuming von karman constant is 0.4
  k = 0.4
  CH4 <- calc_wp_eddy_diff(CH4, k)
  #add to list
  min9.K.WP.list <- list(H2O = H2O, CO2 = CO2, CH4 = CH4)
  return(min9.K.WP.list)
}
