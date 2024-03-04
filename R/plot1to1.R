#' plot.1to1
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
plot.1to1 <- function(all.sites, desired.var, x.lab, y.lab){
  ggplot(all.sites, aes(x = all.sites[,paste0(desired.var)], y = FG)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    facet_wrap(~ site, scales = "free")+
    xlab(x.lab)+
    ylab(y.lab)+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}