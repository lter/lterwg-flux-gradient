#' temp.response.curve
#'
#' @param site df filtered to site nighttime CO2 and Air Temperature
#' @param TA.name name of Ait Temperature column
#' @param rho initial value of amplitude parameter
#' @param psi initial value of growth/decay parameter
#' @param flux.name name of flux column to use as CO2 flux
#'
#' @return model object with estimated temperature response curve parameters from nighttime CO2 flux and Air Temperature
#' 
#'
#' @author Alexis Helgeson
temp.response.curve <- function(site, TA.name, rho, psi, flux.name){
  #using exponential model
  temp.model   <- R ~ rho*exp(psi*TA)
  #set initial parameter values
  initial.param  <- c(rho = rho, psi = psi)
  #select for only model predictor and response data
  temp.flux.vars <- data.frame(TA=site[,paste0(TA.name)], R = site[,paste0(flux.name)])
  #fit model
  temp.response.model <- gsl_nls(fn=temp.model, data=temp.flux.vars, start=initial.param)
  #print(light.response.model)
  #make predictions
  #df_pred <- predict(temp.response.model, newdata=temp.flux.vars)
  
  return(temp.response.model)
}