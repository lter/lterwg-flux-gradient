#' plot.all.sites.1to1
#'
#' @param all.sites dataframe of all sites filtered to desired gas: CO2, H2O
#' @param desired.var which NEON EC to use as comparison against FG
#' @param x.lab x axis label
#' @param y.lab y axis label
#'
#' @return linear 1 to 1 plot across all sites
#' 
#'
#' @author Alexis Helgeson
plot.all.sites.1to1 <- function(all.sites, desired.var, x.lab, y.lab){
  #set scale for each facet so x and y are even: NOTE THESE LIST ENTRIES NEED TO BE IN THE SAME ORDER AS THE FACETTED PLOTS
  x.limits <- list(scale_x_continuous(limits = range(all.sites[which(all.sites$site=="BONA"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="CPER"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="GUAN"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="HARV"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="JORN"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="KONZ"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="NIWO"), "FG"])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="TOOL"), "FG"])))
  y.limits <- list(scale_y_continuous(limits = range(all.sites[which(all.sites$site=="BONA"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="CPER"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="GUAN"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="HARV"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="JORN"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="KONZ"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="NIWO"), "FG"])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="TOOL"), "FG"])))
  #plot linear 1:1 facet by site
  ggplot(all.sites, aes(x = !! sym(desired.var), y = FG)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    facet_wrap(~ site, scales = "free")+
    facetted_pos_scales(x = x.limits, y = y.limits)+
    xlab(x.lab)+
    ylab(y.lab)+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}