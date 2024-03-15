#' applyeqn.FG.aerowindprof.R
#' 
#' Apply the calculation for the aerodynamic/wind profile calculation
#' FG flux = rho_mol x (-k) x (diff.conc)/(diff.heights)
#' This is the same function for aero/windprof, since just k is different 
#'
#' @param min9 gas concentration data frame, passed from computeFG.AE.WP.R
#' @param eddy.diff.name name of which eddy diffusivity to use, passed from computeFG.AE.WP.R
#'
#'  Code inputs molar mixing ratio: # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
#'  Code outputs molar flux: # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
#'  min9$dConc is a mixing ratio not a concentration
#'  
#' @author Samuel Jurado, Alexis Helgeson, Camilo Rey, and Roisin Commane
#' 
applyeqn.FG.aero.windprof <- function(min9, eddy.diff.name, bootstrap, nsamp){
  
  if(bootstrap == 1){
    # Select concentration mean & variance columns
    data.cols <- c("mean_A", "vari_A", "mean_B", "vari_B",
                   "rhoa_kgm3", "dHeight")
    
    # Remove missing data
    min9 <- min9[complete.cases(min9[,data.cols]),]
    nsamp = nsamp
    
    # Sample over concentration mean & variance 
    #FCO2_MBR_H2Otrace = FH2O_MBR_CO2trace = FCH4_MBR_CO2trace = FCH4_MBR_H2Otrace = vector()
    FG_mean = FG_lo = FG_hi = FG_sd = vector()
    dConc_mean = dConc_sd = vector()
    
    for(i in 1:nrow(min9)){
      # Draw nsamp from normal with mean & sd of concentration
      cConc_A = rnorm(n = nsamp, mean = min9$mean_A[i],
                      sd = sqrt(min9$vari_A[i]))
      cConc_B = rnorm(n = nsamp, mean = min9$mean_B[i],
                      sd = sqrt(min9$vari_B[i]))
      dConc = cConc_A-cConc_B
      
      # Save dConc mean & var
      dConc_mean[i] = mean(dConc)
      dConc_sd[i] = sd(dConc)
      
      # Pull bootstrapped samples through the flux gradient calculation
      # for the aerodynamic or wind profile method
      diff.heights <- as.numeric(min9$dHeight)[i] # m
      k <- as.numeric(min9[,paste0(eddy.diff.name)])[i]
      rho <- as.numeric(min9$rhoa_kgm3)[i] #kg m-3
      rho_mol <- rho*.0289 # mol m-3
      
      FG = vector()
      for(j in 1:nsamp){ # loop over sampled conc to calculate flux
        
        # Apply equation for the aero/windprof method
        FG[j] <- rho_mol*(-k)*(dConc[j])/(diff.heights) # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
        
      }
      # Calculate the mean & sd of flux over the bootstrapped values
      FG_mean[i] = mean(FG)
      FG_sd[i] = sd(FG)
    }
    # Attach the mean & sd for all times to the orig table
    min9$FG_mean = FG_mean
    min9$FG_sd = FG_sd
  } 
  else{ # do not bootstrap
    diff.conc <- as.numeric(min9$dConc) # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
    diff.heights <- as.numeric(min9$dHeight) # m
    
    #select for desired eddy diffusivity using eddy.diff.name
    k <- as.numeric(min9[,paste0(eddy.diff.name)]) #m-2 s-1
    rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
    rho_mol <- rho*.0289 # mol m-3
    min9$FG <- rho_mol*(-k)*(diff.conc)/(diff.heights) # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
  }
  
  # Return the original table with flux attached
  return(min9)
}
# #remove NAs
# data.cols <- c("dConc", "rhoa_kgm3", "dHeight")
# min9 <- min9[complete.cases(min9[,data.cols]),]
# diff.conc <- as.numeric(min9$dConc) # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
# diff.heights <- as.numeric(min9$dHeight) # m
# #does this eddy diffisivity need a direction correction?
# #DEPRECIATED CODE
# # k <- as.numeric(min9$EddyDiff) #m-2 s-1
# #select for desired eddy diffusivity using eddy.diff.name
# k <- as.numeric(min9[,paste0(eddy.diff.name)])
# rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
# rho_mol <- rho*.0289 # mol m-3
# 
# min9$FG <- rho_mol*(-k)*(diff.conc)/(diff.heights) # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1