#' Usage: SurfProp_LogWindProfile(z,U)
#' 
#' Description: Determine zero-plane displacement and roughness length from the log-wind profile.
#'      The zero-plane displacement is the vertical displacement of the wind profile as a result of 
#'        closely-spaced surface elements, approx. 2/3 the height of surface elements
#'      The roughness length is a length-scale of surface roughness, and is the height above the zero-plane
#'        displacement at which the wind speed theoretically becomes zero, approx. 1/10 height of 
#'        surface roughness elements
#'      Notes: Three measurement levels at or above the zero-plane displacement are required.
#'             Estimates are valid only for neutral atmospheric stability conditions.
#'             NA will be output for values not converging to a solution
#' 
#' ----- Inputs ------
#' z: vector of three measurement heights, ex. z <- c(0.5,2,4)
#' U: A n x 3 matrix of average (ex. 30-minute) wind speeds corresponding to the measurement heights in z, 
#'    where n is the sample size
#'
#' ----- Outputs ------
#' result is a data frame of n observations and 3 variables:
#' d: the zero-plane displacement
#' z0: the roughness length
#' ustar: the friction velocity, a velocity scale of turbulence
#' --------------------
#' 
#' Cove Sturtevant
#' 19 November 2015
#' 
surfProp_LogWindProfile <- function(z,U) {
  
  # Do some error checking
  if(length(z) != 3 | !is.vector(z)) {stop("z must be a vector of length 3")}
  sz <- dim(U)
  if(sz[2] != 3) {stop("U must be a matrix with 3 columns")}
  
  # Reassign data for easy processing
  z1 <- z[1]
  z2 <- z[2]
  z3 <- z[3]
  U1 <- U[,1]
  U2 <- U[,2]
  U3 <- U[,3]
    
  # Iteratively solve for d
  dout <- matrix(data=NA,nrow=sz[1])
  for(i in 1:sz[1]) {
  
    fun <- function(d) { abs((U2[i]-U1[i])/(U3[i]-U1[i])*log((z3-d)/(z1-d))-log((z2-d)/(z1-d)))}  
    
    b <- optimize(f=fun,interval=c(-1,1),maximum = FALSE) # Minimize residual
    
    # If we found a good solution, record it
    if(1-abs(b$minimum) < 0.02) {dout[i]=NA} else {dout[i] <- b$minimum}

  }
  
  # Compute roughness length 
  z0=exp((U2*log(z3-dout)-U3*log(z2-dout))/(U2-U3))
    
  # Compute ustar
  k <- 0.4 # von Karman constant
  ustar <- U*k/log((t(matrix(data=z,nrow=3,ncol=sz[1]))-matrix(dout,nrow=sz[1],ncol=3))/matrix(z0,nrow=sz[1],ncol=3))
  ustar <- ustar[,1]
  
  # Make a list of the result
  result <- data.frame(d=dout,z0=z0,ustar=ustar)

  return(result)
}