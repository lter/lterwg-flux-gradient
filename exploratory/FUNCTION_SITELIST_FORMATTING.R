library(tidyverse)
 

Tair_at_TowerTop <- function(df){
  levels <- df %>% select( starts_with( 'Tair')) %>% names %>% str_split_fixed( 'Tair',2) 
  max.levels <- levels[,2] %>% max 
  Tair <- paste('Tair',max.levels, sep="")
  return( Tair)
}

TIME_TOWER_LEVEL_FORMAT <- function(df.list, time.col, dLevelsAminusB.colname ){
  
  for( i in 1:length( df.list )){
    print(i)
    
    df <- df.list[[i]] %>%  as.data.frame
   
    timestamp <- df %>% select( all_of(time.col), all_of(dLevelsAminusB.colname))
    df$timestamp <- timestamp[,1]
    df$TowerH <- timestamp[,2]
    Tair <- Tair_at_TowerTop(df)
    airT <- df %>% select( all_of(Tair))
    df$Tair <- airT[,1]
    
    df.list[[i]] <-  df %>% mutate( Hour = timestamp %>% format( '%H') %>% as.numeric,
                                   Month = timestamp %>% format( '%m'),
                                   YearMon = timestamp %>% format( '%Y-%m'))
    
    
  }
  
  return(df.list)
}