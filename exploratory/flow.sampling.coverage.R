
# I fixed the issue with the filter function,

# Next we need to evaluate differences in sampling between the filter dataset and the pre filter dataset for temperature, PAR, VPD, Month and Hour. .... Need to format the time to the local time for each location.... Can this information be optained from the lat long in r?

library(dispRity)


bhatt.coeff.df <- function(df , df.filter, approach, site){

  heights <- df$dLevelsAminusB %>% na.omit() %>% unique 
  sample.coverage <- data.frame()
  for(j in heights){
    print(j)
    
    df.j <- df %>% filter(dLevelsAminusB == j )
    df.filter.j <- df.filter %>% filter(dLevelsAminusB == j )
    
    summary <- data.frame( 
      approach = approach,
      site = site,
      dLevelsAminusB = j,
      bhatt.PAR = bhatt.coeff(x = df.j$PAR %>% na.omit, y = df.filter.j$PAR %>% na.omit),
      bhatt.Tair_K = bhatt.coeff(x = df.j$Tair_K %>% na.omit, y = df.filter.j$Tair_K %>% na.omit),
      bhatt.VPD = bhatt.coeff(x = df.j$VPD %>% na.omit, y = df.filter.j$VPD %>% na.omit))
    
    sample.coverage <- rbind( sample.coverage, summary)
  }
  
  return(sample.coverage)
}

AE.bhatt <- bhatt.coeff.df(df= AE_9min.df.final , 
               df.filter = AE_9min_FILTER , 
               approach = "AE", 
               site = site)  %>% na.omit

WP.bhatt <- bhatt.coeff.df(df= WP_9min.df.final , 
                           df.filter = WP_9min_FILTER , 
                           approach = "WP", 
                           site = site)  %>% na.omit

MBR.bhatt <- bhatt.coeff.df(df= MBR_9min.df.final , 
                           df.filter = MBR_9min_FILTER , 
                           approach = "MBR", 
                           site = site) %>% na.omit

total.bhatt <- rbind( AE.bhatt, WP.bhatt, MBR.bhatt)



# Month and time of day...

# Get NEON sites from the server and find the timezones: https://cran.r-project.org/web/packages/lutz/readme/README.html

# Conver the TZ from GMT to local time 
MBR_9min.df.final$timeEndA.local <- MBR_9min.df.final$timeEndA %>%  as.POSIXlt( tz = "")

# calculate the number of half hours for a month and hour and see % sampled by the remaining data. 

