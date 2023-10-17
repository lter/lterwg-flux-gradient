met.HF <- function(){
  #grab NEON level 1 air pressure at 2nd tower position 30min resolution for a given site
  P  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/presBaro/",max(test.df$name[test.df$group==paste("/", sitecode, "/dp01/data/presBaro", sep="")]),"/presAtm", sep=""))
  #grab NEON level 1 qfqm for air pressure
  P.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/presBaro/",max(test.df$name[test.df$group==paste("/", sitecode, "/dp01/qfqm/presBaro", sep="")]),"/presAtm", sep=""))
  
  #grab NEON level 1 air temp at lowest tower position (assuming this is surface air temp) 30min resolution for a given site
  temp  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/tempAirLvl/", heights[1] ,"/temp", sep=""))
  #grab NEON level 1 qfqm for surface air temperature
  temp.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/tempAirLvl/", heights[1] ,"/temp", sep=""))
  #grab NEON level 1 horizontal wind speed data from top of tower at 30min resolution for a given site
  SoniWind  <- h5read(hd.file, paste("/", sitecode, "/dp01/data/soni/",heights[ length(heights)] , "/veloXaxsYaxsErth", sep=""))
  #grab NEON level 1 qfqm for horizontal wind speed
  SoniWind.qfqm  <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/soni/", heights[ length(heights)], "/veloXaxsYaxsErth", sep=""))
  #grab NEON momentum roughness from footprint stats table
  #which of these is momentum roughness? Assuming veloZaxsHorSd for now based on range and mean 
  MomRough <- h5read(hd.file, paste("/", sitecode, "/dp04/data/foot/stat", sep=""))
  #NEON only provides general qfqm for footprint not variable specific
  #grab NEON top of tower incoming solar radiation to be used in uStar filtering
  Solar <- h5read(hd.file, paste("/", sitecode, "/dp01/data/radiNet/", heights[ length(heights)], "/radiSwIn", sep=""))
  Solar.qfqm <- h5read(hd.file, paste("/", sitecode, "/dp01/qfqm/radiNet/", heights[ length(heights)], "/radiSwIn", sep=""))
  
  # Merge all data:
  totF <- df_H2O %>% left_join(df_CO2, by= 'timeEnd') %>% left_join(df_CH4 , by= 'timeEnd')
  totF$timeEnd1 <- time.format(totF$timeEnd)  # Reformat the time and round it by one second
  
  Ufric$uStar <- Ufric$veloFric
  Ufric.qfqm$uStar_qfqm <- Ufric.qfqm$qfFinl
  P$airpress <- P$mean
  P.qfqm$airpress_qfqm <- P.qfqm$qfFinl
  #surface air temperature used in aerodynamic profile method estimation of eddy diffusivity
  temp$airtemp <-temp$mean
  temp.qfqm$airtemp_qfqm <- temp.qfqm$qfFinl
  #mean wind speed at measurement height used for wind profile method
  SoniWind$uBar <- SoniWind$mean
  SoniWind.qfqm$uBar_qfqm <- SoniWind.qfqm$qfFinl
  #roughness length used for wind profile method
  MomRough$z0 <- MomRough$veloZaxsHorSd
  #incoming solar radiation to be used in uStar filtering
  Solar$radiSwIn <- Solar$mean
  Solar.qfqm$radiSwIn_qfqm <- Solar.qfqm$qfFinl
  
  # Format the time for the merge:
  Ufric$timeEnd1 <- time.format(Ufric$timeEnd)
  Ufric.qfqm$timeEnd1 <- time.format(Ufric.qfqm$timeEnd)
  P$timeEnd1 <- time.format(P$timeEnd)
  P.qfqm$timeEnd1 <- time.format(P.qfqm$timeEnd)
  temp$timeEnd1 <- time.format(temp$timeEnd)
  temp.qfqm$timeEnd1 <- time.format(temp.qfqm$timeEnd)
  SoniWind$timeEnd1 <- time.format(SoniWind$timeEnd)
  SoniWind.qfqm$timeEnd1 <- time.format(SoniWind.qfqm$timeEnd)
  MomRough$timeEnd1 <- time.format(MomRough$timeEnd)
  Solar$timeEnd1 <- time.format(Solar$timeEnd)
  Solar.qfqm$timeEnd1 <- time.format(Solar.qfqm$timeEnd)
  
  
  totF <- totF %>% left_join( Ufric[,c('timeEnd1', 'uStar')], by='timeEnd1') %>%
    left_join( Ufric.qfqm[,c('timeEnd1', 'uStar_qfqm')], by='timeEnd1')%>%
    left_join(P[,c('timeEnd1', 'airpress')], by='timeEnd1') %>% 
    left_join(P.qfqm[,c('timeEnd1', 'airpress_qfqm')], by='timeEnd1') %>% 
    left_join( temp[,c('timeEnd1', 'airtemp')], by='timeEnd1') %>% 
    left_join( SoniWind[,c('timeEnd1', 'uBar')], by='timeEnd1')%>% 
    left_join( SoniWind.qfqm[,c('timeEnd1', 'uBar_qfqm')], by='timeEnd1')%>% 
    left_join( MomRough[,c('timeEnd1', 'z0')], by='timeEnd1')%>% 
    left_join( Solar[,c('timeEnd1','radiSwIn' )], by='timeEnd1')%>% 
    left_join( Solar.qfqm[,c('timeEnd1', 'radiSwIn_qfqm')], by='timeEnd1')
  
  try(totF <- totF %>%left_join( temp.qfqm[,c('timeEnd1', 'airtemp_qfqm')], by='timeEnd1'), silent = T)
}