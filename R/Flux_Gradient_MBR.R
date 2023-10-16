#' Flux_Gradient_MBR
#'
#' @param cont.df df containing site co2, h2o, ch4 measurements at various heights
#' @param z1_height lower tower height (taken from attr.df$DistZaxsLvlMeasTow)
#' @param z2_height upper tower height (taken from attr.df$DistZaxsLvlMeasTow)
#' @param attr.df df containing site measurment heights
#'
#' @return df with additional columns for MBR calculated ch4, co2, h2o fluxes
#'
#' @author Alexis Helgeson
Flux_Gradient_MBR <- function(cont.df, attr.df, z1_height, z2_height){
  
  #set heights for grabbing cont
  z1 <- as.numeric(z1_height)
  z2 <- as.numeric(z2_height)
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
  
  #build df to fill with MBR estimated fluxes
  mbr.df <- as.data.frame(matrix(NA, nrow = dim(cont.df)[1], ncol = 9))
  #we want the measurement height to be part of the calculated flux col name for matching/validation
  F_ch4_MBR_co2 <- paste0("F_ch4_MBR_co2_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_ch4_MBR_LE <- paste0("F_ch4_MBR_LE_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_co2_MBR_LE <- paste0("F_co2_MBR_LE_0", site_min_height, "0_0", site_max_height, "0_30m")
  F_LE_MBR_co2 <- paste0("F_LE_MBR_co2_0", site_min_height, "0_0", site_max_height, "0_30m")
  colnames(mbr.df) <- c("timeEnd", "datetime", F_ch4_MBR_co2,  F_ch4_MBR_LE, F_co2_MBR_LE, F_LE_MBR_co2, "F_co2", "F_H", "F_LE")
  mbr.df$timeEnd <- cont.df$timeEnd
  mbr.df$datetime <- cont.df$datetime
  mbr.df$F_co2 <- as.numeric(cont.df$F_co2)
  mbr.df$F_LE <- as.numeric(cont.df$F_LE)
  mbr.df$F_H <- as.numeric(cont.df$F_H)
  
  #MBR ch4 flux using co2
  #grabs level 4 co2 flux
  Flux_co2 <- as.numeric(cont.df$F_co2)
  #grabs level 1 co2 stor at lowest height
  Conc_co2_z1<-as.numeric(cont.df[,which(names(cont.df) == co2_min_col)])
  #grabs level 1 co2 stor at max height for site
  Conc_co2_z2<-as.numeric(cont.df[,which(names(cont.df) == co2_max_col)])
  #grabs level 1 ch4 cont at lowest height
  Conc_CH4_z1<-as.numeric(cont.df[,which(names(cont.df) == ch4_min_col)])
  #grabs level 1 ch4 cont at max height for site
  Conc_CH4_z2<-as.numeric(cont.df[,which(names(cont.df) == ch4_max_col)])
  #calculate ch4 flux and add to df
  mbr.df[,which(names(mbr.df) == F_ch4_MBR_co2)] <- ((Conc_CH4_z1-Conc_CH4_z2)/(z2-z1))*(Flux_co2/((Conc_co2_z1-Conc_co2_z2)/(z2-z1)))
  
  #MBR ch4 flux using latent heat flux
  #grab NEON level 4 latent heat flux 
  Flux_LE <- as.numeric(cont.df$F_LE)
  #grab NEON level 1 h2o stor at lowest height
  Conc_h2o_z1<-as.numeric(cont.df[,which(names(cont.df) == h2o_min_col)])
  #grab NEON level 1 h2o stor at max height
  Conc_h2o_z2<-as.numeric(cont.df[,which(names(cont.df) == h2o_max_col)])
  #calculate ch4 flux and add to df: need to include unit conversion for h2O to get from W/m^2 to mmmol using latent heat of vaporization (2.26e3 J/g) and molar mass (10.02 g/mol)
  mbr.df[,which(names(mbr.df) == F_ch4_MBR_LE)] <- ((Conc_CH4_z1-Conc_CH4_z2)/(z2-z1))*((Flux_LE/((1/2.26e3)*(1/18.02)*(1000)))*((Conc_h2o_z1-Conc_h2o_z2)/(z2-z1)))
  
  #MBR co2 flux using latent heat: need to include unit conversion for h2O to get from W/m^2 to mmmol using latent heat of vaporization (2.26e3 J/g) and molar mass (10.02 g/mol)
  mbr.df[,which(names(mbr.df) == F_co2_MBR_LE)] <- ((Conc_co2_z1-Conc_co2_z2)/(z2-z1))*((Flux_LE/((1/2.26e3)*(1/18.02)*(1000)))*((Conc_h2o_z1-Conc_h2o_z2)/(z2-z1)))
  
  #MBR LE flux using co2
  mbr.df[,which(names(mbr.df) == F_LE_MBR_co2)] <- ((Conc_h2o_z1-Conc_h2o_z2)/(z2-z1))*(Flux_co2/((Conc_co2_z1-Conc_co2_z2)/(z2-z1)))
  
  return(mbr.df)
}
