
###########################FLUX SMOOTHING FUNCTION##############################

# data is vector of GF data
# k is running median length
# z is average height of measurement
# L is MO length

FluxDespike <- function(flux,k,L,z,zeta,IQR){
  
  data <- data.frame(flux) 
  
  
  # 1.5 IQR FILTER
  
  q1 <- quantile(data$flux,prob=c(.25), type = 1 , na.rm =TRUE)
  q3 <- quantile(data$flux, prob=c(.75), type=1, na.rm=TRUE)
  
  outliers = (q3-q1)*IQR
  floor = q1 - outliers
  ceiling = q3 + outliers
  
  data$flux <- replace(data$flux, data$flux < floor , NA)
  data$flux <- replace(data$flux, data$flux > ceiling , NA)
  
  #STABILITY FILTER
  data$zeta <- z/L
  data$flux <- replace(data$flux, abs(data$zeta) > zeta, NA)
  
  
  #MEDIAN FILTER
  
  data$flux  <- runmed(data$flux ,k=k)
  
  
  
  return(data$flux)
}
