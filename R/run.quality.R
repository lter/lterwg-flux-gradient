#' run.quality
#'
#' @param list.sites list of sites with computed FG fluxes
#' @param method FG method either MBR, AE, WP
#'
#' @return list of data frames for each site containing additional columns for quality filtering and residuals 
#' 
#'
#' @author Alexis Helgeson
run.quality <- function(list.sites, method){
  #loop over sites
  list.sites.quality <- list()
  list.sites.ustar <- list()
  for(s in 1:length(list.sites)){
    #select one site, all quality metrics are site specific
    site <- list.sites[[s]]
    #First step before validation figures can be made is to filter the data for stability conditions and remove extreme values
    #Additional filtering for AE and WP methods for top two tower heights
    if(method == "AE" | method == "WP"){
      #make TowerHeight numeric
      site$TowerHeight_A <- as.numeric(site$TowerHeight_A)
      site$TowerHeight_B <- as.numeric(site$TowerHeight_B)
      site.upper.height <- sort(unique(site$TowerHeight_A), decreasing = TRUE)[[1]] #tallest height
      site.lower.height <- sort(unique(site$TowerHeight_A), decreasing = TRUE)[[2]] #2nd tallest height
      #filter to top of tower and next level down
      site <- site %>% filter(TowerHeight_A == site.upper.height & TowerHeight_B == site.lower.height)
    }
    #flag outliers, create new column IQR_flag that flags outliers
    site.outlier <- IQR.outlier.filter(site = site)
    #flag spikes in fluxes, create new columns spike.bin which is the bin assigned for comparison of differences, spike.flag that flags spikes, date, and day_night
    site.spike <- spike.detection(site = site.outlier)
    #flag atmospheric stability conditions, create new columns Stability_100 and Stability_500 that indicates neutral, stable, unstable conditions
    #only for AE which uses L
    if(method == "AE"){
      site.stability <- stability.condition(site = site.spike)
      site.residuals <- calculate.rmse(site = site.stability)
    }else{
      #Next calculate RMSE and residuals
      site.residuals <- calculate.rmse(site = site.spike)
    }
    #Add in month column for grouping/visualizations
    site.residuals$month <- month(site.residuals$timeBgn_A)
    #flag ustar -> this function must be called after the month column is added, we need the month column for ustar threshold calculation
    #still needs work -> REddy Proc function only works with dataframe where rows are exactly 30 min apart
    #Decided to use "plotting" method for determining ustar threshold: selecting for ustar values when -1 >= nighttime NEE <= 1 and take median, grouping data by month, growing/nongrowing, all data
    #the ustar_threshold column is a dataframe
    site.ustar <- ustar.threshold.interp(site = site.residuals)
    list.sites.ustar[[s]] <- site.ustar
    #Add in hour column for grouping/visualizations: the timezone will be local for that site and corrected for daylight savings
    #TO DO: ADD PROPER TZ CORRECTION CODE FOR GUAN
    site.hour <- add.hour.column(site = site.residuals, site.name = unique(site$site))
    
    list.sites.quality[[s]] <- site.hour
  }
  #only includes top two tower levels
  names(list.sites.quality) <- names(list.sites)
  names(list.sites.ustar) <- names(list.sites)
  
  return(list(list.sites.quality, list.sites.ustar))
}
