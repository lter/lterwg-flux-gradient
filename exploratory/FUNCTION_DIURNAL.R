

# 'These are a series of functions used in the diurnal analysis:'
DIURNAL <- function( dataframe, flux, flux.other , gas){
  
  dataframe <- dataframe %>% as.data.frame
  names(dataframe) <- substring( names( dataframe), 6)
  
  dataframe$flux.other <- dataframe[, flux.other]
  dataframe$flux <- dataframe[, flux]
  dataframe.GrowS <- dataframe %>% mutate( Month = Month %>% as.numeric) %>% filter( Month >= 4,
                                                                                     Month < 11, 
                                                                                    !is.na(flux.other ),
                                                                                     !is.na(flux),
                                                                                    gas == gas)
  
  dataframe.GrowS$flux <- dataframe.GrowS$flux.other <- NULL
  
  yearmon <- unique(dataframe.GrowS$YearMon)
  
  new.data <- data.frame()
  Final.data <- data.frame()
  
  for ( a in unique(dataframe.GrowS$TowerH)) {
    print(a)
    for( i in yearmon){
      print(i)
      
      try(subset <- dataframe.GrowS %>% filter(YearMon == i, TowerH == a), silent = T)
      
      subset$flux <-subset[,flux]
      
      try( model <- loess( flux ~ Hour , data = subset ), silent = T)
      
      try(pred <- predict(model, newdata = subset, se=TRUE), silent = T)
      
      
      try(new.data  <- dataframe.GrowS %>% filter(YearMon == i, TowerH == a) %>% mutate(DIURNAL = pred$fit,
                                                                      DIURNAL.SE =pred$fit* qt(0.95 / 2 + 0.5, pred$df) ), silent = T)
      Final.data <- rbind(  Final.data, new.data)
    }
  }
  
  return( Final.data)
}

DIURNAL.COMPILE <- function( dataframe, FG_flux, EC_flux, gas){
  
  FG.DIURNAL <- DIURNAL( dataframe = dataframe,
                               flux = FG_flux,
                             flux.other = EC_flux, gas)
  
  EC.DIURNAL <- try(DIURNAL( dataframe = dataframe,
                          flux = EC_flux,flux.other = FG_flux, gas), silent = T)
  
  
  FG.DIURNAL.1 <- try( FG.DIURNAL %>% select(YearMon, Hour, DIURNAL, DIURNAL.SE, TowerH) %>%  mutate( FG= DIURNAL,
                  FG.SE= DIURNAL.SE) %>% select( YearMon, Hour, FG, FG.SE,TowerH) %>% distinct() , silent = T)
  
  EC.DIURNAL.1 <- try( EC.DIURNAL %>% select(YearMon, Hour, 
                                        DIURNAL, DIURNAL.SE, TowerH) %>% 
    mutate( EC= DIURNAL,EC.SE= DIURNAL.SE) %>% 
    select(YearMon, Hour, EC, EC.SE, TowerH)%>% distinct(), silent = T)
  
  
  DIURNAL <- try (FG.DIURNAL.1 %>% full_join( EC.DIURNAL.1, by= c('YearMon', 'Hour','TowerH')) %>% distinct(), silent = T)
  
  return( DIURNAL)
  
}

DIURNAL.COMPILE.Sites <- function( FG.tibble, FG_flux, EC_flux, gas ) {
  
  sites <- names(FG.tibble)
  
  Diurnal.list <- list()
  
  for ( i in sites){
    
    print(paste("Diurnal Calculation for", i, sep= " "))
    
    df = DIURNAL.COMPILE( dataframe= FG.tibble[i],
                                        FG_flux = FG_flux , 
                                        EC_flux = EC_flux, gas)
     
    Diurnal.list[i] <- list( df %>% mutate( DIFF = FG-EC) )
    print("Done")
  }
  
  return( Diurnal.list )
}

Diurnal.Summary <- function(diurnal.tibble, TYP ) {
  # Summarize the Diurnal Information:
  diurnal.tibble
  sites <- names( diurnal.tibble)
  
  summary.diurnal <- data.frame(
    TowerH = as.character(), 
    FG.mean = as.numeric(),   
    FG.SE = as.numeric(), 
    EC.mean = as.numeric(),
    EC.SE = as.numeric(),
    DIFF.mean = as.numeric(),  
    DIFF.SE = as.numeric(),
    Site = as.character())
  
  for( i in sites){
    
    dataframe <-  diurnal.tibble[i]  %>% as.data.frame
    names(dataframe) <- substring( names( dataframe), 6)
    
    
    sub <- dataframe %>% reframe(.by = c(YearMon, TowerH),
                                 FG = sum(abs(FG), na.rm=T), 
                                 FG.SE = sum(abs(FG.SE), na.rm=T), 
                                 EC = sum(abs(EC), na.rm=T), 
                                 EC.SE = sum(abs(EC.SE), na.rm=T),
                                 DIFF = sum(abs(DIFF), na.rm=T)) %>% reframe( .by = TowerH,
                                                                     FG.mean = mean(FG, na.rm=T), 
                                                                     FG.SE = var(FG, na.rm=T)/sqrt(length(FG)), 
                                                                     EC.mean = mean(EC, na.rm=T), 
                                                                     EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                     DIFF.mean = mean(DIFF, na.rm=T), 
                                                                     DIFF.SE = var(DIFF, na.rm=T)/sqrt(length(DIFF)),) %>% mutate( Site = i)
    
    summary.diurnal <- rbind( summary.diurnal, sub)
    
  }
  summary.diurnal.final <- summary.diurnal %>% mutate(Type= TYP, 
                              Flux.deviation = (DIFF.mean/EC.mean)*100)
  
  
  return(summary.diurnal.final)
  
}
