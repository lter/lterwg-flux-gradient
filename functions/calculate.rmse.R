#' calculate.rmse
#'
#' @param site df of calculated flux from specific site taken from Validation df
#'
#' @return df with RMSE and residual columns
#' 
#'
#' @author Alexis Helgeson
#' 
calculate.rmse <- function(site){
  #calculate RMSE for CO2
  site.CO2 <- site %>% filter(gas == "CO2")
  site.CO2$residual <- site.CO2$FC_turb_interp - site.CO2$FG
  site.CO2$RMSE <- sqrt(mean((site.CO2$residual)^2, na.rm = T))
  
  #calculate RMSE for H2O
  site.H2O <- site %>% filter(gas == "H2O")
  site.H2O$residual <- site.H2O$FH2O_interp - site.H2O$FG
  site.H2O$RMSE <- sqrt(mean((site.H2O$residual)^2, na.rm = T))
  
  #calculate RMSE for CH4
  site.CH4 <- site %>% filter(gas == "CH4")
  site.CH4$residual <- NA #set NA for now
  site.CH4$RMSE <- NA
  print("NO VALIDATION DATA AVAILABLE FOR CH4, SETTING RESIDUAL AND RMSE COLUMNS TO NA")
  # site.CH4$residual <- site.CH4$ - site.CH4$FG
  # site.CH4$RMSE <- sqrt(mean((site.CH4$residual)^2, na.rm = T))
  
  site.residual <- rbind(site.CO2, site.H2O, site.CH4)
  
  return(site.residual)

}