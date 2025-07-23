
# 'These are a series of functions used in the diurnal analysis:'

# Need to add CCC into the file to be able to filter by good data only
# Decide if it is possible to fit by year.... Measurement levels....

DIEL <- function( dataframe, flux, Gas, flux.other){
  
  dataframe <- dataframe %>% as.data.frame
  #names(dataframe) <- substring( names( dataframe), 6)
  
  dataframe$flux.other <- dataframe[, flux.other]
  dataframe$flux <- dataframe[, flux]
  
  # Need to define the growing season???
  dataframe.gs <- dataframe %>% 
    mutate(  Month = timeEndA.local %>% format("%m") %>% as.numeric,
             YearMon = timeEndA.local %>% format("%Y-%m"),
             Year = timeEndA.local %>% format("%Y"),
             Hour = timeEndA.local %>% format("%H") %>% as.numeric,
             TowerH = paste(TowerPosition_A, TowerPosition_B, sep="-")) %>% filter(!is.na(FC_turb_interp) == TRUE) %>% reframe(.by=YearMon, min.EC = min(FC_turb_interp, na.rm=T) ) 
  
  dataframe.gs.threshold <-   dataframe.gs$min.EC %>% mean 
  
  dataframe.GrowS <- dataframe %>% 
                     mutate(  Month = timeEndA.local %>% format("%m") %>% as.numeric,
                              YearMon = timeEndA.local %>% format("%Y-%m"),
                              Year = timeEndA.local %>% format("%Y"),
                              Hour = timeEndA.local %>% format("%H")%>% as.numeric,
                              TowerH = paste(TowerPosition_A, TowerPosition_B, sep="-")) %>% 
                     filter( !is.na(flux.other),
                             !is.na(flux),
                             gas == Gas) %>% left_join( dataframe.gs, by="YearMon") %>% filter( min.EC <= dataframe.gs.threshold)


  dataframe.GrowS$flux <- dataframe.GrowS$flux.other <- NULL
  
 Year <- unique(dataframe.GrowS$Year)
  

  message(" Ready to fit loess for all data")
  new.data <- data.frame()
  Final.data.all <- data.frame()
  
    for( i in Year){
      print(i)
      try({
          subset <- dataframe.GrowS %>% filter(Year == i)
          subset$flux <-subset[,flux]
          count <- subset$flux %>% na.omit %>% length
          
          if(count > 48){
          model <- loess( flux ~ Hour , data = subset )
          model %>% plot
          Diel.df <- data.frame(Hour = seq(0, 23), Year = i)
          pred <- predict(model, newdata = Diel.df, se=TRUE)
      
          new.data  <- Diel.df %>% mutate(DIEL = pred$fit, 
                                         DIEL.SE =pred$fit* qt(0.95 / 2 + 0.5, pred$df)) %>% mutate(data="all")
          
          new.data %>% ggplot( aes(x=Hour, y=DIEL)) +geom_point()
          Peak <- new.data$DIEL %>% max(na.rm=T)
          MIN <- new.data$DIEL %>% min(na.rm=T)
          
          new.data$Peak.Hour <-  new.data$Hour[new.data$DIEL == Peak] %>% as.numeric %>% mean(na.rm=T)
          new.data$Min.Hour <-  new.data$Hour[new.data$DIEL == MIN] %>% as.numeric %>% mean(na.rm=T)
          new.data$count <- count
          Final.data.all <- rbind(Final.data.all, new.data) } },silent=T)}
  
  rm(new.data)
  message(" Done fitting loess for all data")
  
  
  message(" Ready to fit loess for good data")
  new.data <- data.frame()
  Final.data.good <- data.frame()
  
  for( i in Year){
    print(i)
    try({
      subset <- dataframe.GrowS %>% filter(Year == i, Good.CCC == 1)
      count <- subset[,flux] %>% na.omit %>% length
      
      if(count > 48){
      subset$flux <-subset[,flux]
      model <- loess( flux ~ Hour , data = subset )
      model %>% plot
      Diel.df <- data.frame(Hour = seq(0, 23), Year = i)
      pred <- predict(model, newdata = Diel.df, se=TRUE)
      
      new.data  <- Diel.df %>% mutate(DIEL = pred$fit, 
                                      DIEL.SE =pred$fit* qt(0.95 / 2 + 0.5, pred$df)) %>% mutate(data="good")
      
      new.data %>% ggplot( aes(x=Hour, y=DIEL)) +geom_point()
      Peak <- new.data$DIEL %>% max(na.rm=T)
      MIN <- new.data$DIEL %>% min(na.rm=T)
      
      new.data$Peak.Hour <-  new.data$Hour[new.data$DIEL == Peak] %>% as.numeric %>% mean(na.rm=T)
      new.data$Min.Hour <-  new.data$Hour[new.data$DIEL == MIN] %>% as.numeric %>% mean(na.rm=T)
      new.data$count <- count
      Final.data.good  <- rbind(Final.data.good , new.data) %>% mutate(data="good")}},silent=T)}
  rm(new.data)
  message(" Done fitting loess for good data")
  
  
  message(" Ready to fit loess for bad data")
  new.data <- data.frame()
  Final.data.bad <- data.frame()
  for( i in Year){
    print(i)
    try({
      subset <- dataframe.GrowS %>% filter(Year == i, Good.CCC == 0)
      count <- subset[,flux] %>% na.omit %>% length
      if(count > 48){
      subset$flux <-subset[,flux]
      model <- loess( flux ~ Hour , data = subset )
      model %>% plot
      Diel.df <- data.frame(Hour = seq(0, 23), Year = i)
      pred <- predict(model, newdata = Diel.df, se=TRUE)
      
      new.data  <- Diel.df %>% mutate(DIEL = pred$fit, 
                                      DIEL.SE =pred$fit* qt(0.95 / 2 + 0.5, pred$df))%>% mutate(data="bad")
      
      new.data %>% ggplot( aes(x=Hour, y=DIEL)) +geom_point()
      Peak <- new.data$DIEL %>% max(na.rm=T)
      MIN <- new.data$DIEL %>% min(na.rm=T)
      
      new.data$Peak.Hour <-  new.data$Hour[new.data$DIEL == Peak] %>% as.numeric %>% mean(na.rm=T)
      new.data$Min.Hour <-  new.data$Hour[new.data$DIEL == MIN] %>% as.numeric %>% mean(na.rm=T)
      new.data$count <- count
      Final.data.bad  <- rbind(Final.data.bad , new.data) %>% mutate(data="bad")}},silent=T) }
  rm(new.data)
  message(" Done fitting loess for all data")
  
  Final.data <- rbind(Final.data.all, Final.data.good, Final.data.bad )
  
  if( exists('Final.data' )) { 
    return( Final.data)
  }
 
}

DIEL.COMPILE <- function( dataframe, FG_flux, EC_flux, Gas){

  try({
        FG.DIEL <- DIEL( dataframe = dataframe, flux = FG_flux, Gas, flux.other = EC_flux)
        
        EC.DIEL <- DIEL( dataframe = dataframe, flux = EC_flux, Gas, flux.other = FG_flux)
        
        if(length(FG.DIEL ) > 0) {
          
          FG.DIEL.1 <- FG.DIEL %>%  rename( FG= DIEL, FG.SE= DIEL.SE,Peak.Hour.FG = Peak.Hour, Min.Hour.FG = Min.Hour) %>% 
            select( Year, Hour, FG, FG.SE, Peak.Hour.FG, Min.Hour.FG, data, count) %>% distinct()
          
          EC.DIEL.1 <- EC.DIEL %>% rename( EC= DIEL, EC.SE= DIEL.SE, Peak.Hour.EC = Peak.Hour, Min.Hour.EC = Min.Hour ) %>% 
            select(Year, Hour, EC, EC.SE,  Peak.Hour.EC, Min.Hour.EC, data)%>% distinct()
          
          
          DIEL.df <- FG.DIEL.1 %>% full_join( EC.DIEL.1, by= c('Year', 'Hour', 'data')) %>% distinct() %>% mutate( DIFF.DIEL = FG-EC)
        } })
          
        }
        
        
  
  if(exists('DIEL.df' )) { 
    return( DIEL.df)
    }
    
  
  
}

# Depreciated:

DIEL.COMPILE.Sites <- function( FG.tibble, FG_flux, EC_flux, Gas ) {
  
  sites <- names(FG.tibble)
  
  DIEL.list <- list()
  
  for ( i in sites){
    
    print(paste("DIEL Calculation for", i, sep= " "))
    
    df = DIEL.COMPILE( dataframe= FG.tibble[i],
                          FG_flux = FG_flux , 
                          EC_flux = EC_flux, Gas)
    
    if(is.null(df)){
      message('DIEL.COMPILE output is NULL (errored) for ',i, '. Skipping.')
      next
    }
    DIEL.list[i] <- list( df %>% mutate( DIFF = FG-EC) )
    print("Done")
  }
  
  return( DIEL.list )
}

DIEL.Summary.old <- function(DIEL.tibble, TYP ) {
  # Summarize the DIEL Information:
  sites <- names( DIEL.tibble)
  
  summary.DIEL <- data.frame(
    TowerH = as.character(), 
    FG.count = as.numeric(),
    FG.mean = as.numeric(),   
    FG.min = as.numeric(),
    FG.max = as.numeric(),
    FG.SE = as.numeric(), 
    EC.count = as.numeric(),
    EC.mean = as.numeric(),
    EC.min = as.numeric(),
    EC.max = as.numeric(),
    EC.SE = as.numeric(),
    DIFF.mean = as.numeric(),  
    DIFF.SE = as.numeric(),
    Site = as.character())
  
  for( i in sites){
    
    dataframe <-  DIEL.tibble[i]  %>% as.data.frame
    names(dataframe) <- substring( names( dataframe), 6)
    
    
    sub <- dataframe %>% reframe(.by = c(YearMon, TowerH),
                                 FG = sum(abs(FG), na.rm=T), 
                                 FG.count = length(FG %>% na.omit ),
                                 EC.count = length(EC %>% na.omit ),
                                 FG.SE = sum(abs(FG.SE), na.rm=T), 
                                 EC = sum(abs(EC), na.rm=T), 
                                 EC.SE = sum(abs(EC.SE), na.rm=T),
                                 DIFF = sum(abs(DIFF), na.rm=T)) %>% 
      reframe( .by = TowerH,
               FG.count = sum(FG.count ),
               FG.min = min(FG, na.rm=T),
               FG.max = max(FG, na.rm=T),
               FG.mean = mean(FG, na.rm=T), 
               FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
               EC.count = sum(EC.count ),
               EC.min = min(EC, na.rm=T), 
               EC.max = max(EC, na.rm=T), 
               EC.mean = mean(EC, na.rm=T), 
               EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
               DIFF.mean = mean(DIFF, na.rm=T), 
               DIFF.SE = var(DIFF, na.rm=T)/sqrt(length(DIFF))) %>% mutate( Site = i)
    
    
    summary.DIEL <- rbind( summary.DIEL, sub)
    
  }
  summary.DIEL.final <- summary.DIEL %>% mutate(Type= TYP, 
                                                      Flux.deviation = (DIFF.mean/EC.mean)*100)
  
  
  return(summary.DIEL.final)
  
}