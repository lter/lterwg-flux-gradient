#' temp.response.curve
#'
#' @param site df filtered to site nighttime CO2 and Air Temperature
#' @param TA.name name of Ait Temperature column
#' @param rho initial value of amplitude parameter
#' @param psi initial value of growth/decay parameter
#'
#' @return model object with estimated temperature response curve parameters from nighttime CO2 flux and Air Temperature
#' 
#'
#' @author Alexis Helgeson
temp.response.curve <- function(site, TA.name, rho, psi){
  #define model
  frmu   <- R ~ rho*exp(psi*TA)
  #set initial parameter values
  stprm  <- c(rho = rho, psi = psi)
  #select for only model predictor and response data
  df_accurate <- data.frame(TA=site[,paste0(TA.name)], R = site$FG)
  #fit model
  light.response.model <- gsl_nls(fn=frmu, data=df_accurate, start=stprm)
  #print(light.response.model)
  #make predictions
  #df_pred <- predict(light.response.model, newdata=df_accurate)
  
  return(light.response.model)
}