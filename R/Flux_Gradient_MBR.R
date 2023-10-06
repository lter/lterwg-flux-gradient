#' Flux_Gradient_MBR
#'
#' @param cont.df df containing site co2, h2o, ch4 measurements at various heights
#' @param attr.df df containing site measurment heights
#'
#' @return df with additional columns for MBR calculated ch4 fluxes
#'
#' @author Alexis Helgeson
Flux_Gradient_MBR <- function(cont.df, attr.df){
  
  #grab max height for given site
  #assuming each row is a different height therefore the number of rows is number of heights
  site_max_height <- dim(attr.df)[1]
  #cont.df has measurement height as part of column name, so we need to set the correct col name to grab co2/h2o/ch4 measurements
  co2_col <- paste("co2.000_0",site_max_height,"0_30m",sep="")
  ch4_col <- paste("ch4.000_0",site_max_height,"0_30m",sep="")
  h2o_col <- paste("h2o.000_0",site_max_height,"0_30m",sep="")
  #grabs the first entry in DistZaxsLvlMeasTow column, assuming lowest height
  z1 <- as.numeric(attr.df$DistZaxsLvlMeasTow[1])
  #grabs the last entry (max height of DistZaxsLvlMeasTow column
  z2 <- as.numeric(attr.df$DistZaxsLvlMeasTow[site_max_height])
  
  #MBR flux using co2
  #grabs level 4 co2 flux
  Flux_co2 <- as.numeric(cont.df$F_co2)
  #grabs level 1 co2 stor at lowest height
  Conc_co2_z1<-as.numeric(cont.df$co2.000_010_30m)
  #grabs level 1 co2 stor at max height for site
  Conc_co2_z2<-as.numeric(cont.df[,which(names(cont.df) == co2_col)])
  #grabs level 1 ch4 cont at lowest height
  Conc_CH4_z1<-as.numeric(cont.df$ch4.000_010_30m)
  #grabs level 1 ch4 cont at max height for site
  Conc_CH4_z2<-as.numeric(cont.df[,which(names(cont.df) == ch4_col)])
  #calculate ch4 flux and add to df
  cont.df$F_ch4_MBR_co2 <- Flux_co2*(Conc_CH4_z1-Conc_CH4_z2/Conc_co2_z1-Conc_co2_z2)
  
  #MBR flux using sensible heat flux
  #grab NEON level 4 sensible heat flux 
  Flux_H <- as.numeric(cont.df$F_H)
  #grab NEON level 1 h2o stor at lowest height
  Conc_h2o_z1<-as.numeric(cont.df$h2o.000_010_30m)
  #grab NEON level 1 h2o stor at max height
  Conc_h2o_z2<-as.numeric(cont.df[,which(names(cont.df) == h2o_col)])
  #calculate ch4 flux and add to df
  cont.df$F_ch4_MBR_H <- Flux_H*(Conc_CH4_z1-Conc_CH4_z2/Conc_h2o_z1-Conc_h2o_z2)
  
  #MBR flux using latent heat flux
  #grab NEON level 4 sensible heat flux 
  Flux_LE <- as.numeric(cont.df$F_LE)
  #grab NEON level 1 h2o stor at lowest height
  Conc_h2o_z1<-as.numeric(cont.df$h2o.000_010_30m)
  #grab NEON level 1 h2o stor at max height
  Conc_h2o_z2<-as.numeric(cont.df[,which(names(cont.df) == h2o_col)])
  #calculate ch4 flux and add to df
  cont.df$F_ch4_MBR_LE <- Flux_LE*(Conc_CH4_z1-Conc_CH4_z2/Conc_h2o_z1-Conc_h2o_z2)
  
  
  return(cont.df)
}