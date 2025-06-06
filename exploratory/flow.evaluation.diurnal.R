
source(fs::path(DirRepo,'exploratory/FUNCTION_DIURNAL.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results

message(" Currently the diel is fit for the peak growing season for each year using all measurement heights. Good data is where the CCC > 0.5, bad data is where the CCC is < 0.5")

for( site in site.list){
  
  print( site)
  
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_sub <- SITES_One2One %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                         Good.CCC = case_when( CCC >= 0.5 ~ 1,
                                                                                               CCC < 0.5 ~ 0)) %>% 
    select( Site, Good.CCC, dLevelsAminusB, Approach) %>% filter(Site == site)

  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'))
  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'))
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'))

  message( paste("Running DIEL functions- CO2 for ", site))
   # Calculate Diurnal Patterns by Year-month:
  DIEL.MBR.CO2 <- DIEL.COMPILE( dataframe = MBR_9min_FILTER_CCC, 
                                            FG_flux = 'FG_mean', 
                                            EC_flux = 'FC_turb_interp',
                                            Gas = "CO2") %>% mutate(TYP = "MBR", gas= "CO2")
  
  DIEL.WP.CO2 <- DIEL.COMPILE( dataframe =  WP_9min_FILTER_CCC, 
                                           FG_flux = 'FG_mean', 
                                           EC_flux = 'FC_turb_interp', 
                                           Gas = "CO2") %>% mutate(TYP = "WP", gas= "CO2")
  
  DIEL.AE.CO2 <-DIEL.COMPILE( dataframe = AE_9min_FILTER_CCC, 
                                           FG_flux = 'FG_mean', 
                                           EC_flux = 'FC_turb_interp', 
                                           Gas = "CO2") %>% mutate(TYP = "AE", gas= "CO2")
  

DIEL.CO2 <- rbind(  DIEL.MBR.CO2, DIEL.WP.CO2, DIEL.AE.CO2 ) %>% mutate( TYP = factor( TYP, levels= c('MBR', 'AE', 'WP')))
  
DIEL.CO2 %>% filter(data=="good") %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=Year))+ geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ TYP)

message( paste("Running DIEL functions- H2O for ", site))
  # Diel for H2O

DIEL.MBR.H2O <- DIEL.COMPILE( dataframe = MBR_9min_FILTER_CCC, 
                              FG_flux = 'FG_mean', 
                              EC_flux = 'FC_turb_interp',
                              Gas = "H2O") %>% mutate(TYP = "MBR", gas= "H2O")

DIEL.WP.H2O <- DIEL.COMPILE( dataframe =  WP_9min_FILTER_CCC, 
                             FG_flux = 'FG_mean', 
                             EC_flux = 'FC_turb_interp', 
                             Gas = "H2O") %>% mutate(TYP = "WP", gas= "H2O")

DIEL.AE.H2O <-DIEL.COMPILE( dataframe = AE_9min_FILTER_CCC, 
                            FG_flux = 'FG_mean', 
                            EC_flux = 'FC_turb_interp', 
                            Gas = "H2O") %>% mutate(TYP = "AE", gas= "H2O")

  
  DIEL.H2O <- rbind(  DIEL.MBR.H2O, DIEL.WP.H2O, DIEL.AE.H2O ) %>% mutate( TYP = factor( TYP, levels= c('MBR', 'AE', 'WP')))
  
  DIEL.H2O %>% filter(data=="good") %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=Year))+ geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ TYP)
  
  final.DIEL <- rbind(   DIEL.CO2,   DIEL.H2O)
  message( paste("Saving data for", site))
  save(final.DIEL, file=paste(localdir.site, "/", site, "_DIEL.Rdata", sep=""))
}
  
  # Plots : ####
  for ( site in site.list){
    
    # Import the data: 
    localdir.site <- paste(localdir,"/", site, sep = "")
    load(file=paste(localdir.site, "/", site, "_DIEL.Rdata", sep=""))
    
    message(paste("Lets make some plots for", site))
    
    
    try({
      
      p1 <- ggplot( data = final.DIEL %>% filter(TYP=="MBR", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +facet_wrap(~data)
      
      p2 <- ggplot( data =  final.DIEL %>% filter(TYP=="AE", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +facet_wrap(~data)
      
      p3 <-ggplot( data =  final.DIEL %>% filter(TYP=="WP", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +facet_wrap(~data)
    })
    
    print(ggarrange( p1, p2, p3, nrow=3))
    
    setwd(dir.diel)
    
    png(paste("Diel_CO2_", site,".png", sep=""), width=6, 
        height=6, units="in", res=1200)
    
    print(ggarrange( p1, p2, p3, nrow=3))
    dev.off()
    
    message("done with CO2 Diel")       
    
    try({
      p1 <- ggplot( data = final.DIEL %>% filter(TYP=="MBR", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +facet_wrap(~data)
      
      p2 <- ggplot( data =  final.DIEL %>% filter(TYP=="AE", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +facet_wrap(~data)
      
      p3 <-ggplot( data =  final.DIEL %>% filter(TYP=="WP", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +facet_wrap(~data)
    })
    
    print(ggarrange( p1, p2, p3, nrow=3))
    
    png(paste("Diel_H2O_", site,".png", sep=""), width=6, 
        height=5, units="in", res=1200)
    print(ggarrange( p1, p2, p3, nrow=3))
    dev.off()
  
    print("done with H2O diel")   
    
  }