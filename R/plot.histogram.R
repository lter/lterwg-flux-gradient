#' plot.histogram
#'
#' @param all.sites dataframe of all sites
#' @param desired.var which variable to feature in the histogram
#' @param x.lab x axis label
#'
#' @return histogram plot of variable across all sites
#' 
#'
#' @author Alexis Helgeson
plot.histogram <- function(all.sites, desired.var, x.lab){
  #plot histogram of desired variable
  ggplot(all.sites, aes(x = all.sites[,paste0(desired.var)])) +
    geom_bar() +
    facet_wrap(~ site, scales = "free")+
    xlab(paste0(x.lab))+
    ylab("Frequency")+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}