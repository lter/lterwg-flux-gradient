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
  #define model
  frmu   <- P ~ ((alpha*beta*Q)/((alpha*Q) + beta)) - gama
  #set initial parameter values
  stprm  <- c(alpha = alpha, beta = beta, gama = gama)
  #select for only model predictor and response data
  df_accurate <- data.frame(Q=site$PAR, P = site[,paste0(flux.name)])
  #fit model
  light.response.model <- gsl_nls(fn=frmu, data=df_accurate, start=stprm)
  #print(light.response.model)
  #make predictions
  #df_pred <- predict(light.response.model, newdata=df_accurate)
  
  return(light.response.model)
}

