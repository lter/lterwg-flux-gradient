###########################MONIN OBUKHOV LENGTH################################
#' @param press atmospheric pressure in Kpa
#' @param temp temperature in C
#' @param H sensible heat flux
#' @param LE latent heat flux
#' @param velofric velocity friction (u*)
#'
#' @return Vector of monin obuhkov length
#'
#' @author Samuel Jurado and Alexis Helgeson
#' Modified by Camilo Rey Sanchez March 14 2024


MOlength <- function(press,temp,H,LE,velofric){
  #######ATMOSPHERIC COMPUTATION######
  
  #Constants
  lambda = 2.26*10**3 # Latent Heat of Vaporization (J g-1)
  k = .41        # Von Karmaan Constant (unitless)
  g = 9.81       # Gravity (m s-2)
  cp = 1005      # Specific Heat of Air J K-1 g-1
  R= 8.314462    # Universal gas constant m3 Pa K-1 mol-1
  Md= 29         # Molecular mass of dry air g mol-1
  
  #Dry Air Density#
  
  for(x in list(1:length(press))){
    rho = (press[x]*1000)/((temp[x]+273.15)*R)*Md # g m-3
    #print(rho)
  }
  
  
  #Virtual Potential Temperature Flux #
  
  for(x in list(1:length(H))){
    vpotflux <- (H[x]/(rho[x]*cp))+.61*(temp[x]+273.15)*(LE[x]/(rho[x]*lambda))
    vpotflux <- vpotflux
    #print(vpotflux)
  }
  
  #Monin-Obukhov Length#
  
  for(x in list(1:length(vpotflux))){
    L <- -((velofric[x]**3)*(temp[x]+273.15))/(k*g*(vpotflux[x]))
    #print(L)
  }
  
  DATA <- list(rho = rho, vpotflux = vpotflux, L = L)
  return(DATA)
  
}
