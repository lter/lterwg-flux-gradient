#' light.response.curve
#'
#' @param site df of selected site filtered to only daytime measurements
#' @param alpha initial value of slope parameter
#' @param beta initial value of plateau parameter
#' @param gama initial value of respiration term
#' @param flux.name name of flux column to use as CO2 flux
#'
#' @return model object with estimated light response curve parameters from daytime CO2 flux and PAR
#' 
#'
#' @author Alexis Helgeson
light.response.curve <- function(site, alpha, beta, gama, flux.name){
  #using rectangular hyperbolic model
  light.model   <- P ~ ((alpha*beta*Q)/((alpha*Q) + beta)) - gama
  #set initial parameter values
  inital.param  <- c(alpha = alpha, beta = beta, gama = gama)
  #select for only model predictor and response data
  light.flux.vars <- data.frame(Q=site$PAR, P = site[,paste0(flux.name)])
  #fit model
  light.response.model <- gsl_nls(fn=light.model, data=light.flux.vars, start=inital.param)
  #print(light.response.model)
  #make predictions
  #df_pred <- predict(light.response.model, newdata=light.flux.vars)
  
  return(light.response.model)
}

