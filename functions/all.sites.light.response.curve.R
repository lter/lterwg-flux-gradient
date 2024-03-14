#' all.sites.light.response.curve
#'
#' @param all.sites df of all sites for a given method
#' @param flux.name name of flux column to use as CO2 flux
#' @param alpha initial value of slope parameter
#' @param beta initial value of plateau parameter
#' @param gama initial value of respiration term
#' @param method which method was used for flux calculation: AE, WP, or MBR
#'
#' @return list containing dataframe of parameters and list of model objects
#' 
#'
#' @author Alexis Helgeson
all.sites.light.response.curve <- function(all.sites, flux.name, alpha, beta, gama, method){
  #get list of sites
  site.names <- unique(all.sites.ae$site)
  #create dataframe to store light response curve parameters for all sites
  light.response.df <- data.frame(site = site.names, flux.name = flux.name, method = method, alpha = NA, beta = NA, gama = NA)
  #create list to store model object
  all.sites.model.LRC <- list()
  #loop over sites and add estimated parameters to df
  for(s in 1:length(site.names)){
    #filter to daytime flux and PAR for single site
    site <- all.sites %>% filter(gas == "CO2" & day_night == "day" & site == site.names[s])
    #fit curve
    model.LRC <- light.response.curve(site = site, alpha = alpha, beta = beta, gama = gama, flux.name = flux.name)
    #grab model coefficients
    model.coeff <- coefficients(model.LRC)
    #add to dataframe for given site
    light.response.df[which(light.response.df$site == site.names[s]),"alpha"] <- model.coeff[["alpha"]]
    light.response.df[which(light.response.df$site == site.names[s]),"beta"] <- model.coeff[["beta"]]
    light.response.df[which(light.response.df$site == site.names[s]),"gama"] <- model.coeff[["gama"]]
    #save model object to list
    all.sites.model.LRC[[s]] <- model.LRC
  }
  #set names for model object list as site names
  names(all.sites.model.LRC) <- site.names
  
  return(list(param.df = light.response.df, model.objects = all.sites.model.LRC))
  
}