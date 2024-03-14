#' plot.light.response
#'
#' @param model model object returned by light.response.curve
#' @param site dataframe filtered to daytime CO2 and PAr for given site
#' @param flux.name name of flux column to use as CO2 flux
#' @param plot.title should reflect which method is used to calculate FG i.e. AE or WP or MBR
#'
#' @return light response curve plot
#' 
#'
#' @author Alexis Helgeson
plot.light.response <- function(model, site, flux.name, plot.title){
  #remove NAs from dataframe for plotting
  site <- site[complete.cases(site[,c("PAR", paste0(flux.name))]),]
  #grab model coefficients
  model.coeff <- coefficients(model)
  alpha <- model.coeff[["alpha"]]
  beta <- model.coeff[["beta"]]
  gama <- model.coeff[["gama"]]
  #plot light response curve
  ggplot(site, aes(x = PAR, y = !! sym(flux.name))) +
    geom_point() +
    geom_function(fun = function(x) ((alpha*beta*x)/((alpha*x) + beta)) - gama, colour = "blue")+
    #facet_wrap(~ site, scales = "free")+
    xlab(expression(paste("PAR (umol m"^-2, " s"^-1, ")")))+
    ylab(expression(paste("CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))+
    ggtitle(paste0(plot.title))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24), plot.title = element_text(hjust = 0.5))
}