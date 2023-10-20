diffusivityAE <- function(){
  #calculate obukhov stability param
  gamma <- as.numeric(z)/as.numeric(L)
  #calculate AE eddy diffusivity: 0.4 = Von Karman constant
  ae.K <- (0.4*as.numeric(cont.df$uStar)*as.numeric(zG))/as.numeric(gamma)
}