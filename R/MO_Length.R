
###########################MONIN OBUKHOV LENGTH################################
#' @param press atmospheric pressure in Kpa
#' @param temp temperature in C
#' @param H sensible heat flux
#' @param LE latent heat flux
#' @param velofric velocity friction (u*)
#'
#' @return Vector of monin obuhkov length
#'
#' @author Samuel Jurado
#' 


MOlength <- function(press,temp,H,LE,velofric){
  #######ATMOSPHERIC COMPUTATION######
  
  #Constants
  l = 2.26*10**6 #Latent Heat of Vaporization
  k = .41        # Von Karmaan Constant
  g = 9.81       # Gravity
  cp = 1005      # Specific Heat of Air
  
  #Air Density#
  
  for(x in list(1:length(press))){
    rho = (press[x]*1000)/((temp[x]+273.15)*287)
    print(rho)
  }
  
  
  #Surface Flux Virtual Temperature #
  
  for(x in list(1:length(H))){
    vpotflux <- (H[x]/(rho[x]*cp))+.61*(temp[x]+273.15)*(LE[x]/(rho[x]*l))
    vpotflux <- vpotflux
    print(vpotflux)
  }
  
  #Monin-Obukhov Length#
  
  for(x in list(1:length(vpotflux))){
    L <- -((velofric[x]**3)*(temp[x]+273.15))/(k*g*(vpotflux[x]))
    print(L)
  }
  return(L)
  
}
