#' calculate.all.sites.diurnal.avg
#'
#' @param all.sites data frame of fluxes across all sites
#' @param gas.name name of desired gas CO2, H2O, CH4
#'
#' @return data frame of hourly flux avg across all sites
#' 
#'
#' @author Alexis Helgeson and Sam Jurado
calculate.all.sites.diurnal.avg <- function(all.sites, FG.name = "FG", EC.name = "FC_turb_interp"){
  #calculate diurnal averages by site for each flux type
  all.sites.diurnal.FG <- all.sites  %>% group_by(hour, site) %>% summarise(mean_flux = mean(!! sym(FG.name), na.rm=TRUE), sd_flux = sd(!! sym(FG.name), na.rm=TRUE), n =n())
  all.sites.diurnal.EC <- all.sites  %>% group_by(hour, site) %>% summarise(mean_flux = mean(!! sym(EC.name), na.rm =TRUE), sd_flux = sd(!! sym(EC.name), na.rm =TRUE), n =n())
  #add standard error column
  all.sites.diurnal.FG$std_err <- all.sites.diurnal.FG$sd_flux/sqrt(all.sites.diurnal.FG$n)
  all.sites.diurnal.EC$std_err <- all.sites.diurnal.EC$sd_flux/sqrt(all.sites.diurnal.EC$n)
  #add min/max column for plotting error bars
  all.sites.diurnal.FG$ymin <- all.sites.diurnal.FG$mean_flux - all.sites.diurnal.FG$std_err
  all.sites.diurnal.FG$ymax <- all.sites.diurnal.FG$mean_flux + all.sites.diurnal.FG$std_err
  all.sites.diurnal.EC$ymin <- all.sites.diurnal.EC$mean_flux - all.sites.diurnal.EC$std_err
  all.sites.diurnal.EC$ymax <- all.sites.diurnal.EC$mean_flux + all.sites.diurnal.EC$std_err
  #add column to distinguish flux type
  all.sites.diurnal.FG$flux.name <- "FG"
  all.sites.diurnal.EC$flux.name <- "EC"
  #combine into one data frame
  all.sites.diurnal <- bind_rows(all.sites.diurnal.FG, all.sites.diurnal.EC)
  
  return(all.sites.diurnal)
}