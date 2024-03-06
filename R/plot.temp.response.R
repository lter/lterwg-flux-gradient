#' plot.temp.response
#'
#' @param model model object returned by light.response.curve
#' @param site df filtered to site nighttime CO2 and Air Temperature
#' @param TA.name name of air temperature column
#' @param flux.name name of flux column to use as CO2 flux
#'
#' @return temperature response curve 
#' 
#'
#' @author Alexis Helgeson
plot.temp.response <- function(model, site, TA.name, flux.name){
  #remove NAs from dataframe for plotting
  site <- site[complete.cases(site[,c(paste0(TA.name), paste0(flux.name))]),]
  #grab model coefficients
  model.coeff <- coefficients(model)
  rho <- model.coeff[["rho"]]
  psi <- model.coeff[["psi"]]
  #plot light response curve
  ggplot(site, aes(x = !! sym(TA.name), y = !! sym(flux.name))) +
    geom_point() +
    geom_function(fun = function(x) rho*exp(psi*x), colour = "blue")+
    #facet_wrap(~ site, scales = "free")+
    xlab("Air Temperature (deg C)")+
    ylab(expression(paste("CO"[2], " Flux (umol CO"[2], " m"^-2," s"^-1,")")))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}