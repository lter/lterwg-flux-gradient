ustar.filter <- function(site){
  #ustar for CO2; calculate ustar based on eddy-covariance CO2 flux and apply to other fluxes
  site.CO2 <- site %>% filter(gas == "CO2")
  plot(site.CO2$ustar_interp, site.CO2$FC_interp)
  
  
  
  
}