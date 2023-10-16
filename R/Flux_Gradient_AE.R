#' Flux_Gradient_AE
#'
#' @param cont.df df containing site co2, h2o, ch4 measurements at various heights as well as uStar, air pressure, air temperature, uBar, z0
#' @param attr.df df containing site measurment heights
#' @param z1_height lower tower height (taken from attr.df$DistZaxsLvlMeasTow)
#' @param z2_height upper tower height (taken from attr.df$DistZaxsLvlMeasTow)
#' @param z_height tallest tower height (i.e. location of sonic anemometer)
#'
#' @return df with additional columns for AE calculated ch4, co2, h2o fluxes
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
Flux_Gradient_AE <- function(cont.df, attr.df, z1_height, z2_height, z_height){
  #set heights for grabbing cont
  z1 <- as.numeric(z1_height)
  z2 <- as.numeric(z2_height)
  z <- as.numeric(z_height)
  #set col names for grabbing concentrations
  site_max_height <- which(attr.df$DistZaxsLvlMeasTow == z2)
  site_min_height <- which(attr.df$DistZaxsLvlMeasTow == z1)
  #cont.df has measurement height as part of column name, so we need to set the correct col name to grab co2/h2o/ch4 measurements
  co2_max_col <- paste("co2.000_0",site_max_height,"0_30m",sep="")
  ch4_max_col <- paste("ch4.000_0",site_max_height,"0_30m",sep="")
  h2o_max_col <- paste("h2o.000_0",site_max_height,"0_30m",sep="")
  co2_min_col <- paste("co2.000_0",site_min_height,"0_30m",sep="")
  ch4_min_col <- paste("ch4.000_0",site_min_height,"0_30m",sep="")
  h2o_min_col <- paste("h2o.000_0",site_min_height,"0_30m",sep="")
  
  #build df to fill with AE estimated fluxes
  ae.df <- as.data.frame(matrix(NA, nrow = dim(cont.df)[1], ncol = 12))
  #we want the measurement height to be part of the calculated flux col name for matching/validation
  F_ch4_AE <- paste0("F_ch4_AE_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_ch4_AE_WP <- paste0("F_ch4_AE_WP_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_co2_AE <- paste0("F_co2_AE_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_co2_AE_WP <- paste0("F_co2_AE_WP_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_LE_AE <- paste0("F_LE_AE_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_LE_AE_WP <- paste0("F_LE_AE_WP_0", site_min_height, "0_0", site_max_height, "0_30m")
  colnames(ae.df) <- c("timeEnd", "datetime", F_ch4_AE,  F_ch4_AE_WP, F_co2_AE, F_co2_AE_WP, F_LE_AE, F_LE_AE_WP, "F_co2", "F_H", "F_LE", "L")
  ae.df$timeEnd <- cont.df$timeEnd
  ae.df$datetime <- cont.df$datetime
  ae.df$F_co2 <- as.numeric(cont.df$F_co2)
  ae.df$F_LE <- as.numeric(cont.df$F_LE)
  ae.df$F_H <- as.numeric(cont.df$F_H)
  
  #calculate fluxes using AE estimation of eddy diffusivity
  #calculate geometric mean of upper and lower heights
  zG <- sqrt(z1*z2)
  #calculate obukhov length: 0.4 = Von Karman constant, 9.8 = gravitational acceleration, convert air temp from celcius to kelvin (T+273.15)
  L <- ((-as.numeric(cont.df$uStar)^3)*(as.numeric(cont.df$airtemp)+273.15))/(0.4*9.8*as.numeric(cont.df$F_H))
  #add to ae.df for classification of stability classes
  ae.df$L <- as.numeric(L)
  #calculate obukhov stability param
  gamma <- as.numeric(z)/as.numeric(L)
  #calculate AE eddy diffusivity: 0.4 = Von Karman constant
  ae.K <- (0.4*as.numeric(cont.df$uStar)*as.numeric(zG))/as.numeric(gamma)
  
  #calculate ch4 flux using AE
  #grabs level 1 ch4 cont at lowest height
  Conc_CH4_z1<-as.numeric(cont.df[,which(names(cont.df) == ch4_min_col)])
  #grabs level 1 ch4 cont at max height for site
  Conc_CH4_z2<-as.numeric(cont.df[,which(names(cont.df) == ch4_max_col)])
  #FG AE calculation: unit conversion constant for molar to mass mixing ratio for ch4 is conversion from umol to mol then multiply by ratio of dry air g/mol (28.96) and ch4 g/mol (16.04), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_ch4_AE)] <- ((1/1000000)*(28.96/16.04))*(1293*as.numeric(ae.K))*(Conc_CH4_z1-Conc_CH4_z2/z1-z2)
  
  #calculate co2 flux using AE
  #grabs level 1 co2 stor at lowest height
  Conc_co2_z1<-as.numeric(cont.df[,which(names(cont.df) == co2_min_col)])
  #grabs level 1 co2 stor at max height for site
  Conc_co2_z2<-as.numeric(cont.df[,which(names(cont.df) == co2_max_col)])
  #FG AE calculation: unit conversion constant for molar to mass mixing ratio for co2 is conversion from umol to mol then multiply by ratio of dry air g/mol (28.96) and c02 g/mol (44.01), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_co2_AE)] <- ((1/1000000)*(28.96/44.01))*(1293*as.numeric(ae.K))*(Conc_co2_z1-Conc_co2_z2/z1-z2)
  
  #calculate LE flux using AE
  #grab NEON level 1 h2o stor at lowest height
  Conc_h2o_z1<-as.numeric(cont.df[,which(names(cont.df) == h2o_min_col)])
  #grab NEON level 1 h2o stor at max height
  Conc_h2o_z2<-as.numeric(cont.df[,which(names(cont.df) == h2o_max_col)])
  #FG AE calculation: unit conversion constant for molar to mass mixing ratio for h2o is conversion from mmol to mol then multiply by ratio of dry air g/mol (28.96) and h2o g/mol (18.02), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_LE_AE)] <- ((1/1000)*(28.96/18.02))*(1293*as.numeric(ae.K))*(Conc_h2o_z1-Conc_h2o_z2/z1-z2)
  
  #calculate fluxes using WP estimation of eddy diffusivity
  #calculate WP eddy diffusivity, assuming neutral stability: 0.4 = Von Karman constant
  wp.K <- ((0.4^2)*as.numeric(zG)*as.numeric(cont.df$uBar))/(log(as.numeric(z)/as.numeric(cont.df$z0)))
  
  #calculate ch4 flux using WP
  #FG WP calculation: unit conversion constant for molar to mass mixing ratio for ch4 is conversion from umol to mol then multiply by ratio of dry air g/mol (28.96) and ch4 g/mol (16.04), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_ch4_AE_WP)] <- ((1/1000000)*(28.96/16.04))*(1293*as.numeric(wp.K))*(Conc_CH4_z1-Conc_CH4_z2/z1-z2)
  
  #calculate co2 flux using WP
  #FG WP calculation: unit conversion constant for molar to mass mixing ratio for co2 is conversion from umol to mol then multiply by ratio of dry air g/mol (28.96) and c02 g/mol (44.01), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_co2_AE_WP)] <- ((1/1000000)*(28.96/44.01))*(1293*as.numeric(wp.K))*(Conc_co2_z1-Conc_co2_z2/z1-z2)
  
  #calculate LE flux using WP
  #FG WP calculation: unit conversion constant for molar to mass mixing ratio for h2o is conversion from mmol to mol then multiply by ratio of dry air g/mol (28.96) and h2o g/mol (18.02), assuming rho (air density) is 1293 g m−3
  ae.df[,which(names(ae.df) == F_LE_AE_WP)] <- ((1/1000)*(28.96/18.02))*(1293*as.numeric(wp.K))*(Conc_h2o_z1-Conc_h2o_z2/z1-z2)
  
  return(ae.df)
}
