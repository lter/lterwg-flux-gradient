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
plot.all.sites.diurnal <- function(all.sites, plot.title){
  #plot diurnal cycle of all sites
  ggplot(all.sites, aes(x = hour, y = mean_flux, colour = flux.name)) +
    geom_point() +
    geom_errorbar(aes(ymin = all.sites$ymin, ymax = all.sites$ymax, width = 0.2)) +  
    facet_wrap(~ site, scales = "free")+
    scale_x_discrete(breaks = c("00", "06", "12", "18", "23"))+
    xlab("Hour of Day")+
    ylab(expression(paste("CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))+
    guides(colour=guide_legend(title=""))+
    theme_minimal()+
    ggtitle(paste0(plot.title))+
    theme(text = element_text(size = 20), axis.title=element_text(size=24), legend.position = "top", plot.title = element_text(hjust = 0.5))
}