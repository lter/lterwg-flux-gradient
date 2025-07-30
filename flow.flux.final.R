
# summarize fluxes at the site level:


for( site in site.list){
  
  print(site)
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_ID_sub <- SITES_One2One_ID %>% filter(Site == site)
  
  if( sum( SITES_One2One_ID_sub$Good.CCC) > 0){
   
    MBR_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter( Good.CCC == 1)
    
    WP_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)
    
    AE_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)
    
    DF.FILTER_CCC <- rbind(MBR_9min_FILTER_CCC, WP_9min_FILTER_CCC, AE_9min_FILTER_CCC   ) %>% filter( Canopy_L1 != "WW") %>% mutate( timeEndA.local.round = lubridate::round_date(lubridate::ymd_hms(timeEndA.local), unit = "30 minutes")) # Create a half fourly timestamp:
    


# aggregate fluxes by the half hour
    DF.FILTER_CCC %>% names()
    DF.FILTER_CCC_30 <-  DF.FILTER_CCC %>% reframe( .by =timeEndA.local.round, 
                                                    FC_nee_interp = mean( FC_nee_interp, na.rm=T),
                                                    FG_mean= mean( FG_mean, na.rm=T),
                                                    FG_sd= mean(  FG_sd, na.rm=T),
                                                    CanopyHeight= mean(CanopyHeight, na.rm=T),
                                                    Cutoff05.SDH= mean( Cutoff05.SDH, na.rm=T),
                                                    EVI.mean= mean( EVI.mean, na.rm=T),
                                                    NDVI.mean= mean( NDVI.mean, na.rm=T),
                                                    LAI.mean= mean( LAI.mean, na.rm=T), 
                                                    RH= mean( RH, na.rm=T),
                                                    PAR= mean( PAR, na.rm=T),
                                                    VPD= mean( VPD, na.rm=T),
                                                    Tair_K= mean( Tair_K, na.rm=T) )
    
    
    
    DF.FILTER_CCC_30 %>% filter( PAR >=1500) %>%  ggplot() + geom_boxplot(aes( x= FC_nee_interp), outlier.colour="black", outlier.shape=16,
                                                 outlier.size=2, notch=FALSE)+ 
      geom_boxplot(aes( x= FG_mean),colour="orange",  outlier.colour="orange", outlier.shape=16, outlier.size=2, notch=FALSE, alpha= 0.4)
    
    
    DF.FILTER_CCC_30 %>% ggplot() + geom_point(aes( x=PAR, y= FG_mean)) + geom_smooth( aes( x=PAR, y= FC_nee_interp))
    
    
    # Mean Flux when PAR == 0 and >= 1500
    # canculate the diel and scale it to the daily rate. 
    # Q10
    # Do everything for the EC and FG
    
    
  }
  print('done')
  
}
