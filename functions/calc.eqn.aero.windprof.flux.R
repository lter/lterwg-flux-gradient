#' calc.eqn.aero.windprof.flux.R
#' 
#' Apply the calculation for the aerodynamic/wind profile calculation
#' FG flux = rho_mol*(-k)*(diff.conc)/(diff.heights)
#' This applies the same equation for aero/windprof methods, 
#' where k is is calculated upstream for each method. 
#'
#' @param min9 gas concentration data frame, passed from computeFG.AE.WP.R (now calc.gas.aero.windprof.flux.R)
#' @param eddy.diff.name name of which eddy diffusivity to use, passed from computeFG.AE.WP.R (now calc.gas.aero.windprof.flux.R)
#'
#'  Code inputs molar mixing ratio: # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
#'  Code outputs molar flux: # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
#'  min9$dConc is a mixing ratio not a concentration
#'  
#' @author Samuel Jurado, Alexis Helgeson, Camilo Rey, Roisin Commane, Jackie Matthes
#' 
calc.eqn.aero.windprof.flux <- function(min9, eddy.diff.name, bootstrap, nsamp){
  
  # Use concentration mean & variance to sample dConc at each timestep (bootstrap=1)
  # or just use the given mean dConc (bootstrap = 0)
  if(bootstrap == 1){ 
    # Select concentration mean & variance columns
    data.cols <- c("mean_A", "vari_A", "mean_B", "vari_B",
                   "rhoa_kgm3", "dHeight")
    
    # Remove missing data
    # min9 <- min9[complete.cases(min9[,data.cols]),]
    nsamp = nsamp # number of bootstrap samples
    
    # Storage for flux & concentration diff mean & variance 
    FG_mean = FG_sd = vector()
    dConc_mean = dConc_sd = vector()
    
    for(i in 1:nrow(min9)){ # loop over each row (timestep) in df
     
      # Draw nsamp for concentrations at tower levels A & B 
      # from normal using the mean & sd of concentration
      cConc_A = rnorm(n = nsamp, mean = min9$mean_A[i],
                      sd = sqrt(min9$vari_A[i]))
      cConc_B = rnorm(n = nsamp, mean = min9$mean_B[i],
                      sd = sqrt(min9$vari_B[i]))
      
      # Store mean and sd of concentration difference between
      # re-sampled concentrations at tower levels A & B
      dConc = cConc_A-cConc_B
      dConc_mean[i] = mean(dConc)
      dConc_sd[i] = sd(dConc)
      
      # Pull sampled dConc through the flux gradient calculation
      # for the aerodynamic or wind profile method
      diff.heights <- as.numeric(min9$dHeight)[i] # m
      k <- as.numeric(min9[,paste0(eddy.diff.name)])[i]
      rho <- as.numeric(min9$rhoa_kgm3)[i] #kg m-3
      rho_mol <- rho/.0289 # mol m-3 (divided by molar mass of dry air 0.0289 kg/mol)
      
      # loop over each sampled dConc at one timestep to apply FG equation
      FG = vector()
      for(j in 1:nsamp){ 
        
        # Apply equation for the aero/windprof method
        # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
        FG[j] <- rho_mol*(-k)*(dConc[j])/(diff.heights) 
        
      }
      # Calculate the mean & sd of calculated flux at one timestep 
      # from sampled dConc values
      FG_mean[i] = mean(FG)
      FG_sd[i] = sd(FG)
    }
    # Attach the bootstrapped flux mean & sd for all times 
    # to the orig data frame
    min9$FG_mean = FG_mean
    min9$FG_sd = FG_sd
    min9$dConc_sd = dConc_sd
  } 
  else{ # do not bootstrap, just use the dConc mean
    diff.conc <- as.numeric(min9$dConc) # CO2 umol mol-1, CH4 nmol mol-1, H2O mmol mol-1
    diff.heights <- as.numeric(min9$dHeight) # m
    
    #select for desired eddy diffusivity using eddy.diff.name
    k <- as.numeric(min9[,paste0(eddy.diff.name)]) #m-2 s-1
    rho <- as.numeric(min9$rhoa_kgm3) #kg m-3
    rho_mol <- rho/.0289 # mol m-3 (divided by molar mass of dry air 0.0289 kg/mol)
    min9$FG_mean <- rho_mol*(-k)*(diff.conc)/(diff.heights) # CO2 umol m-2 s-1, CH4 nmol m-2 s-1, H2O mmol m-2 s-1
    min9$FG_sd <- NA # does not exist for non-bootstrap
    min9$dConc_sd <- NA # does not exist for non-bootstrap
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