library(lubridate)

count.samples.month <- function(df, gas){

  df.sub.gas <- df %>% filter( gas == gas) %>% select("FG_mean", "hour.local", "timeEndA.local") %>% 
    reframe(.by= timeEndA.local, FG_mean = mean(FG_mean, na.rm=T)) %>%  
    mutate( count = 1, timeEndA.local = as.POSIXct(timeEndA.local), Month = format(timeEndA.local, "%m"), hour.local = format(timeEndA.local, "%H")) %>% 
    select("FG_mean", "count", "Month", "hour.local", "timeEndA.local") %>% 
    mutate( Days.in.Month = days_in_month(month(timeEndA.local)),
            Years.count = length(unique(format(timeEndA.local, "%Y")))) 
  
  df.sub.gas.month <-  df.sub.gas %>% reframe( .by= c(Month), Month.sample = sum(count), 
                                               Days.in.Month = mean(Days.in.Month),
                                               Years.count = mean(Years.count )) %>% 
    mutate( space = Years.count * Days.in.Month*48,
            Percent.Coverage.Month  = (Month.sample / space) *100) %>% select(Month, Percent.Coverage.Month)
  
  
}

count.samples.diel <- function(df, gas){
  
  
  df.sub.gas <- df %>% filter( gas == gas) %>% select("FG_mean","timeEndA.local") %>% 
    reframe(.by= timeEndA.local, FG_mean = mean(FG_mean, na.rm=T)) %>% 
    mutate( count = 1, timeEndA.local = as.POSIXct(timeEndA.local),
            Month = format(timeEndA.local, "%m"),
            hour.local = format(timeEndA.local, "%H")) %>% 
    select("FG_mean", "count", "Month", "hour.local", "timeEndA.local") %>% 
    mutate( Days.in.Month = days_in_month(month(timeEndA.local)),
            Years.count = length(unique(format(timeEndA.local, "%Y"))))
  
  df.sub.gas.diel <-  df.sub.gas %>% reframe( .by= c(hour.local), Diel.sample = sum(count),
                                              Years.count = mean(Years.count )) %>% 
    mutate( space = 17520,
            Percent.Coverage.Diel  = (Diel.sample/ space) *100) %>% select(hour.local,  Percent.Coverage.Diel)
  
  return( df.sub.gas.diel)
}


MBR_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter( Good.CCC == 1) 

WP_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)

AE_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)


DF.FILTER_CCC <- gtools::smartbind(MBR_9min_FILTER_CCC, WP_9min_FILTER_CCC, AE_9min_FILTER_CCC   ) %>% filter( Canopy_L1 != "WW")

month.CO2 <-  count.samples.month(df= DF.FILTER_CCC, gas = "CO2") %>% mutate( gas="CO2")
month.H2O <-  count.samples.month(df= DF.FILTER_CCC, gas = "H2O") %>% mutate( gas="H2O")

sample.month <- rbind(month.CO2, month.H2O)  %>% mutate(site = site)

diel.CO2 <-  count.samples.diel(df= DF.FILTER_CCC, gas = "CO2") %>% mutate( gas="CO2")
diel.H2O <-  count.samples.diel(df= DF.FILTER_CCC, gas = "H2O") %>% mutate( gas="H2O")

sample.diel <- rbind( diel.CO2, diel.H2O) %>% mutate(site = site)
