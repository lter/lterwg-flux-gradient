#' plot.all.sites.bar
#'
#' @param all.sites dataframe of all sites
#' @param desired.var which variable to feature in the bar plot
#' @param x.lab x axis label
#' @param plot.title should reflect which method is used to calculate FG i.e. AE or WP or MBR
#'
#' @return bar plot of variable across all sites
#' 
#'
#' @author Alexis Helgeson
plot.all.sites.bar <- function(all.sites, desired.var, x.lab, plot.title){
  #bar plot of desired variable
  ggplot(all.sites, aes(x = !! sym(desired.var))) +
    geom_bar() +
    facet_wrap(~ site, scales = "free")+
    xlab(paste0(x.lab))+
    ylab("Frequency")+
    ggtitle(paste0(plot.title))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24), plot.title = element_text(hjust = 0.5))
}