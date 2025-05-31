library(lubridate)

count.samples.month <- function(df, gas){
  
  
  df.sub.gas <- df %>% filter( gas == gas) %>% mutate( count = 1,
                                                       Month = format(timeEndA.local, "%m")) %>% 
    select("FG_mean", "count", "Month", "time.local", "hour.local", "timeEndA.local", "dLevelsAminusB") %>% 
    mutate( Days.in.Month = days_in_month(month(timeEndA.local)),
            Years.count = length(unique(format(timeEndA.local, "%Y"))))
  
  df.sub.gas.month <-  df.sub.gas %>% reframe( .by= c(Month, dLevelsAminusB), Month.sample = sum(count), 
                                               Days.in.Month = mean(Days.in.Month),
                                               Years.count = mean(Years.count )) %>% 
    mutate( space = Years.count * Days.in.Month*48,
            Percent.Coverage.Month  = (Month.sample / space) *100) %>% select(Month, dLevelsAminusB, Percent.Coverage.Month)
  
  
}

count.samples.diel <- function(df, gas){
  
  
  df.sub.gas <- df %>% filter( gas == gas) %>% mutate( count = 1,
                                                       Month = format(timeEndA.local, "%m")) %>% 
    select("FG_mean", "count", "Month", "time.local", "hour.local", "timeEndA.local", "dLevelsAminusB") %>% 
    mutate( Days.in.Month = days_in_month(month(timeEndA.local)),
            Years.count = length(unique(format(timeEndA.local, "%Y"))))
  
  df.sub.gas.diel <-  df.sub.gas %>% reframe( .by= c(hour.local, dLevelsAminusB), Diel.sample = sum(count),
                                              Years.count = mean(Years.count )) %>% 
    mutate( space = 17520,
            Percent.Coverage.Diel  = (Diel.sample/ space) *100) %>% select(hour.local, dLevelsAminusB, Percent.Coverage.Diel)
  
  return( df.sub.gas.diel)
}

AE.month.CO2 <-  count.samples.month(df= AE_9min_FILTER, gas = "CO2") %>% mutate(approach = "AE", gas="CO2")
WP.month.CO2 <-  count.samples.month(df= WP_9min_FILTER, gas = "CO2") %>% mutate(approach = "WP", gas="CO2")
MBR.month.CO2 <-  count.samples.month(df= MBR_9min_FILTER, gas = "CO2") %>% mutate(approach = "MBR", gas="CO2")

AE.month.H2O <-  count.samples.month(df= AE_9min_FILTER, gas = "H2O") %>% mutate(approach = "AE", gas="H2O")
WP.month.H2O <-  count.samples.month(df= WP_9min_FILTER, gas = "H2O") %>% mutate(approach = "WP", gas="H2O")
MBR.month.H2O <-  count.samples.month(df= MBR_9min_FILTER, gas = "H2O") %>% mutate(approach = "MBR", gas="H2O")


sample.month <- rbind( AE.month.CO2, WP.month.CO2, MBR.month.CO2,
                       AE.month.H2O, WP.month.H2O,MBR.month.H2O )  %>% mutate(site = site)

AE.diel.CO2 <-  count.samples.diel(df= AE_9min_FILTER, gas = "CO2") %>% mutate(approach = "AE", gas="CO2")
WP.diel.CO2 <-  count.samples.diel(df= WP_9min_FILTER, gas = "CO2") %>% mutate(approach = "WP", gas="CO2")
MBR.diel.CO2 <-  count.samples.diel(df= MBR_9min_FILTER, gas = "CO2") %>% mutate(approach = "MBR", gas="CO2")

AE.diel.H2O <-  count.samples.diel(df= AE_9min_FILTER, gas = "H2O") %>% mutate(approach = "AE", gas="H2O")
WP.diel.H2O <-  count.samples.diel(df= WP_9min_FILTER, gas = "H2O") %>% mutate(approach = "WP", gas="H2O")
MBR.diel.H2O <-  count.samples.diel(df= MBR_9min_FILTER, gas = "H2O") %>% mutate(approach = "MBR", gas="H2O")


sample.diel <- rbind( AE.diel.CO2, WP.diel.CO2, MBR.diel.CO2,
                      AE.diel.H2O, WP.diel.H2O,MBR.diel.H2O ) %>% mutate(site = site)