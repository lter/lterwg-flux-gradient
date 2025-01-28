# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)
library(ggh4x)
library(googledrive)


#Real Eddy Diff. converter - Sam J.

"This code is intended to take the EC fluxes and concetration observations from NEON
towers and convert them into an eddy diffusivity for comparison between H2O and CO2 K's
and cross-gradient analysis"

### EC Eddy Diffusivity ###
"input site file is a list of 3 data frames, CO2,H2O,and CH4. Back calculates 
CO2 and H2O and returns original frames with added columns KCO2 and KH2O"

eddy_diff_real <- function(site) {
  
  
  site.2 <- site %>% mutate(
    rho = rhoa_kgm3,
    dz1 = TowerHeight_A,
    dz2 = TowerHeight_B,
    dx = dConc,
    dz = as.numeric(dz1)-as.numeric(dz2),
    cp = 1005 ,#J/kgK
    mol_air = rho*34.53,
    
    # H2O
    flux =  case_when( gas == 'H2O' ~ ((LE_turb_interp/(2.25*10**6))/(.01801))*1000, gas == 'CO2'~ FC_turb_interp), 
    Kgas = -(flux*dz)/(dx*mol_air) #m2/s
  )
 
  return(site.2)
}

####Cross Gradient Flux Flagger###
"Flags all instances of a cross gradient flux"
#' K is eddy diffusivity of gas H2O or CO2
#' df is the dataframe of interest

cross_grad_flag <- function(df,K){
  df <- cbind(df, cross_grad_flag = NA)
  df$cross_grad_flag <- ifelse(df$K < 0,1,0 )
  return(df)
}

crossGradientDF <- function( DATA ){

  for (i in 1:length(DATA)) {
    DATA[[i]] <- eddy_diff_real(DATA[[i]]) #calculates Kgas
    DATA[[i]] <- cross_grad_flag(DATA[[i]], Kgas)
  }
  
  return(DATA)

}
