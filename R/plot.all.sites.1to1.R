#' plot.all.sites.1to1
#'
#' @param all.sites dataframe of all sites filtered to desired gas: CO2, H2O
#' @param desired.var which NEON EC to use as comparison against FG
#' @param x.lab x axis label
#' @param y.lab y axis label
#' @param plot.title should reflect which method is used to calculate FG i.e. AE or WP or MBR
#'
#' @return linear 1 to 1 plot across all sites
#' 
#'
#' @author Alexis Helgeson
plot.all.sites.1to1 <- function(all.sites, x.flux, y.flux, x.lab, y.lab, plot.title){
  #add na filter for desired.var
  all.sites <- all.sites[complete.cases(all.sites[,c(x.flux, y.flux)]),]
  #set scale for each facet so x and y are even: NOTE THESE LIST ENTRIES NEED TO BE IN THE SAME ORDER AS THE FACETTED PLOTS
  x.limits <- list(scale_x_continuous(limits = range(all.sites[which(all.sites$site=="BONA"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="CPER"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="GUAN"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="HARV"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="JORN"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="KONZ"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="NIWO"), y.flux])), scale_x_continuous(limits = range(all.sites[which(all.sites$site=="TOOL"), y.flux])))
  y.limits <- list(scale_y_continuous(limits = range(all.sites[which(all.sites$site=="BONA"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="CPER"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="GUAN"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="HARV"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="JORN"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="KONZ"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="NIWO"), y.flux])), scale_y_continuous(limits = range(all.sites[which(all.sites$site=="TOOL"), y.flux])))
  #plot linear 1:1 facet by site
  ggplot(all.sites, aes(x = !! sym(x.flux), y = !! sym(y.flux))) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    facet_wrap(~ site, scales = "free")+
    facetted_pos_scales(x = x.limits, y = y.limits)+
    xlab(x.lab)+
    ylab(y.lab)+
    ggtitle(paste0(plot.title))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24), plot.title = element_text(hjust = 0.5))
}