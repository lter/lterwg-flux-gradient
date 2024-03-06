#' calculate.all.sites.diurnal.avg
#'
#' @param all.sites data frame of fluxes across all sites
#'
#' @return data frame of hourly flux avg across all sites
#' 
#'
#' @author Alexis Helgeson and Sam Jurado
calculate.all.sites.diurnal.avg <- function(all.sites){
  all.sites.diurnal <- all.sites %>% filter(gas == "CO2") %>% group_by(hour, site) %>% summarise(mean_FG_flux = mean(FG, na.rm=TRUE), mean_EC_flux = mean(FC_nee_interp, na.rm =TRUE), sd_FG_flux = sd(FG, na.rm=TRUE), sd_EC_flux = sd(FC_nee_interp, na.rm =TRUE), n =n())
  #add standard error column
  all.sites.diurnal$std_FG_err <- all.sites.diurnal$sd_FG_flux/sqrt(all.sites.diurnal$n)
  all.sites.diurnal$std_EC_err <- all.sites.diurnal$sd_EC_flux/sqrt(all.sites.diurnal$n)
  #add min/max column for plotting error bars
  all.sites.diurnal$FG_ymin <- all.sites.diurnal$mean_FG_flux - all.sites.diurnal$std_FG_err
  all.sites.diurnal$FG_ymax <- all.sites.diurnal$mean_FG_flux + all.sites.diurnal$std_FG_err
  all.sites.diurnal$EC_ymin <- all.sites.diurnal$mean_EC_flux - all.sites.diurnal$std_EC_err
  all.sites.diurnal$EC_ymax <- all.sites.diurnal$mean_EC_flux + all.sites.diurnal$std_EC_err
  
  return(all.sites.diurnal)
}