#' plot.all.sites.diurnal
#'
#' @param all.sites dataframe of site hourly flux averages along wih std error
#' @param flux.name column name of desired flux to plot
#' @param flux.ymin.name column name of ymin of desired flux for lower bound of error bar
#' @param flux.ymax.name column name of ymax of desired flux for upper bound of error bar
#'
#' @return plot of diurnal cycle of flux across all sites
#' 
#'
#' @author Alexis Helgeson and Sam Jurado 
plot.all.sites.diurnal <- function(all.sites, flux.name, flux.ymin.name, flux.ymax.name){
  #plot diurnal cycle of all sites
  ggplot(all.sites, aes(x = hour, y = !! sym(flux.name))) +
    geom_point() +
    geom_errorbar(aes(ymin = !! sym(flux.ymin.name), ymax = !! sym(flux.ymax.name)), width = 0.2) +  
    facet_wrap(~ site, scales = "free")+
    scale_x_discrete(breaks = c("00", "06", "12", "18", "23"))+
    xlab("Hour of Day")+
    ylab(expression(paste("CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}