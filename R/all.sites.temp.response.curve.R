#' all.sites.temp.response.curve
#'
#' @param all.sites df of all sites for a given method
#' @param flux.name name of flux column to use as CO2 flux
#' @param method which method was used for flux calculation: AE, WP, or MBR
#' @param rho initial value of amplitude parameter
#' @param psi initial value of growth/decay parameter
#'
#' @return list containing dataframe of parameters and list of model objects
#' 
#'
#' @author Alexis Helgeson
all.sites.temp.response.curve <- function(all.sites, flux.name, rho, psi, method){
  #get list of sites
  site.names <- unique(all.sites.ae$site)
  #create dataframe to store light response curve parameters for all sites
  temp.response.df <- data.frame(site = site.names, flux.name = flux.name, method = method, rho = NA, psi = NA)
  #create list to store model object
  all.sites.model.TRC <- list()
  #loop over sites and add estimated parameters to df
  for(s in 1:length(site.names)){
    #filter to daytime flux and PAR for single site
    site <- all.sites %>% filter(gas == "CO2" & day_night == "night" & site == site.names[s])
    #select for only temperature columns
    TA.cols <- c(names(site)[grep("Tair", names(site))])
    ta.check <- c()
    for(t in 1:length(TA.cols)){
      ta.nas <- length(is.na(site[,TA.cols[t]])[is.na(site[,TA.cols[t]]) == TRUE])
      if(ta.nas == length(site[,TA.cols[t]])){
        ta.check[t] <- NA
      }else{
        ta.check[t] <- 1
      }
    }
    #check which columns are NA and remove from options to pass to temp.response.curve
    if(TRUE %in% is.na(ta.check)){
      TA.cols <- TA.cols[-which(is.na(ta.check))]
    }else{
      TA.cols <- TA.cols
    }
    #select for top of tower air temperature
    TA.levels <- as.numeric(substr(TA.cols, 5,5))
    TA.top <- max(TA.levels, na.rm = T)
    TA.name <- paste0("Tair",TA.top)
    #fit curve
    model.TRC <- temp.response.curve(site = site, rho = rho, psi = psi, flux.name = flux.name, TA.name = TA.name)
    #grab model coefficients
    model.coeff <- coefficients(model.TRC)
    #add to dataframe for given site
    temp.response.df[which(temp.response.df$site == site.names[s]),"rho"] <- model.coeff[["rho"]]
    temp.response.df[which(temp.response.df$site == site.names[s]),"psi"] <- model.coeff[["psi"]]
    #save model object to list
    all.sites.model.TRC[[s]] <- model.TRC
  }
  #set names for model object list as site names
  names(all.sites.model.TRC) <- site.names
  
  return(list(param.df = temp.response.df, model.objects = all.sites.model.TRC))
  
}