
# compare distributions in filtered and non-filtered conditions:

library(dispRity)
library(tidyverse)
library(sf)

bhatt.coeff.df <- function(df , df.filter, approach){
  
  heights <- df$dLevelsAminusB %>% na.omit() %>% unique 
  sample.coverage <- data.frame()
  for(j in heights){
    print(j)
    
    df.j <- df %>% filter(dLevelsAminusB == j )
    df.filter.j <- df.filter %>% filter(dLevelsAminusB == j )
    
    summary <- data.frame( 
      approach = approach,
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
                           approach = "AE")  %>% na.omit

WP.bhatt <- bhatt.coeff.df(df= WP_9min.df.final , 
                           df.filter = WP_9min_FILTER , 
                           approach = "WP")  %>% na.omit

MBR.bhatt <- bhatt.coeff.df(df= MBR_9min.df.final , 
                            df.filter = MBR_9min_FILTER , 
                            approach = "MBR") %>% na.omit

total.bhatt <- rbind( AE.bhatt, WP.bhatt, MBR.bhatt)