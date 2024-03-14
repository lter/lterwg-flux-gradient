#' plot.single.site.1to1
#'
#' @param site single site dataframe filtered to desired gas: CO2, H2O
#' @param desired.var which NEON EC to use as comparison against FG
#' @param x.lab x axis label
#' @param y.lab y axis label
#' @param plot.title should reflect which method is used to calculate FG i.e. AE or WP or MBR
#'
#' @return single site linear 1to1 plot 
#' 
#'
#' @author Alexis Helgeson
plot.single.site.1to1 <- function(site, x.flux, y.flux, x.lab, y.lab, plot.title){
  #add na filter for desired.var
  site <- site[complete.cases(site[,c(x.flux, y.flux)]),]
  #set range for x/y axis based on y.flux
  range.x.y <- range(site[,y.flux])
  #plot linear 1:1 facet by site
  ggplot(site, aes(x = !! sym(x.flux), y = !! sym(y.flux))) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    scale_x_continuous(limits = range.x.y)+
    scale_y_continuous(limits = range.x.y)+
    xlab(x.lab)+
    ylab(y.lab)+
    ggtitle(paste0(plot.title))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24), plot.title = element_text(hjust = 0.5))
}