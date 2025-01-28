# Diurnal:

TIME.MBR <- function(df.list){
  
  for( i in 1:length(df.list)){
    print(i)
    df.list[[i]] <- df.list[[i]] %>%  as.data.frame %>% mutate( Hour = match_time %>% format( '%H') %>% as.numeric,
                                                                YearMon = match_time %>% format( '%Y-%m'),
                                                                TowerH = dLevelsAminusB_CO2)
    
  }
  
  return(df.list)
}

TIME <- function(df.list){
  
  for( i in 1:length(df.list)){
    print(i)
    df.list[[i]] <- df.list[[i]] %>%  as.data.frame %>% mutate( Hour =  timeEnd_A %>% format( '%H')%>% as.numeric(),
                                                                YearMon = timeEnd_A %>% format( '%Y-%m'),
                                                                TowerH = dLevelsAminusB)
    
  }
  
  return(df.list)
}

DIURNAL <- function( dataframe, flux){
  
  yearmon <- unique(dataframe$YearMon)
  
  new.data <- data.frame()
  Final.data <- data.frame()
  
  for ( a in unique(dataframe$TowerH)) {
    print(a)
    for( i in yearmon){
      print(i)
      
      try(subset <- dataframe %>% filter(YearMon == i, TowerH == a), silent = T)
      
      subset$flux <-subset[,flux]
      try( model <- loess( flux ~ Hour , data = subset ), silent = T)
      
      try(pred <- predict(model, newdata = subset, se=TRUE), silent = T)
      
      
      try(new.data  <- dataframe %>% filter(YearMon == i, TowerH == a) %>% mutate(DIURNAL = pred$fit,
                                                                      DIURNAL.SE =pred$fit* qt(0.95 / 2 + 0.5, pred$df) ), silent = T)
      Final.data <- rbind(  Final.data, new.data)
    }
  }
  
  return( Final.data)
}

DIURNAL.COMPILE <- function( dataframe, FG_flux, EC_flux){
  
  MBR.DIURNAL <- DIURNAL( dataframe = dataframe,
                               flux = FG_flux)
  
  EC.DIURNAL <- DIURNAL( dataframe = dataframe,
                          flux = EC_flux)
  
  
  MBR.DIURNAL.1 <-  MBR.DIURNAL %>% select(YearMon, Hour, DIURNAL, DIURNAL.SE, TowerH) %>%  mutate( FG= DIURNAL,
                  FG.SE= DIURNAL.SE) %>% select( YearMon, Hour, FG, FG.SE,TowerH) %>% distinct()
  
  EC.DIURNAL.1 <- EC.DIURNAL %>% select(YearMon, Hour, 
                                        DIURNAL, DIURNAL.SE, TowerH) %>% 
    mutate( EC= DIURNAL,EC.SE= DIURNAL.SE) %>% 
    select(YearMon, Hour, EC, EC.SE, TowerH)%>% distinct()
  
  
  DIURNAL <- MBR.DIURNAL.1 %>% full_join( EC.DIURNAL.1, by= c('YearMon', 'Hour','TowerH')) %>% distinct()
  
  return( DIURNAL)
  
}
