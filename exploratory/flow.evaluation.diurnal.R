# The Diel analysis is currently set up by CCC threshold:

source(fs::path(DirRepo,'exploratory/FUNCTION_DIURNAL.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results

message(" Currently the diel is fit for the peak growing season for each year using all measurement heights. Good data is where the CCC > 0.5, bad data is where the CCC is < 0.5")
ccc.thresholds <- c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75)

# Calculate diels: ####
for( site in site.list){
  
  for(ccc in ccc.thresholds){

    print(paste(site, "CCC threshold =", ccc))
    
    message( paste("Importing the data for ", site))
    localdir.site <- paste(localdir,"/", site, sep = "")
    
    load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
    
    SITES_One2One_sub <- SITES_One2One %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                  Good.CCC = case_when( CCC >= ccc ~ 1,
                                                                        CCC < ccc ~ 0)) %>% 
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
    
    message( paste("Running DIEL functions- H2O for ", site, "CCC threshold = ", ccc))
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
    save(final.DIEL, file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
  }
    
  }
  
  
# Plots : ####

library(ggpubr)
library(ggplot2)


  for ( site in site.list){
    for(ccc in ccc.thresholds){
    
    print(paste(site, "CCC threshold =", ccc))
      
    # Import the data: 
    localdir.site <- paste(localdir,"/", site, sep = "")
    load(file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
    
    message(paste("Lets make some plots for", site))
    
    
    try({
      
      p1 <- ggplot( data = final.DIEL %>% filter(TYP=="MBR", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +facet_wrap(~data)
      
      p2 <- ggplot( data =  final.DIEL %>% filter(TYP=="AE", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +facet_wrap(~data)
      
      p3 <-ggplot( data =  final.DIEL %>% filter(TYP=="WP", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +facet_wrap(~data)
    })
    
    print(ggarrange( p1, p2, p3, nrow=3))
    
    setwd(dir.diel)
    
    png(paste("Diel_CO2_", site,"_", ccc,".png", sep=""), width=6, 
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
    
    png(paste("Diel_H2O_", site,"_", ccc,".png", sep=""), width=6, 
        height=5, units="in", res=1200)
    print(ggarrange( p1, p2, p3, nrow=3))
    dev.off()
  
    print("done with H2O diel")   
    
  }}


# Summary of diel fit by threshold:
diel.summary.year.gas <- data.frame(Year = as.character(),
                                    gas = as.character(),
                                    Site = as.character(),
                                    ccc = as.numeric(),
                                    count = as.numeric(),
                                    FG.total = as.numeric(),
                                    FG.SE = as.numeric(), 
                                    EC.total = as.numeric(), 
                                    EC.SE = as.numeric(),
                                    FG.min = as.numeric(),
                                    FG.max = as.numeric(),
                                    FG.mean = as.numeric(), 
                                    FG.SE = as.numeric(),
                                    EC.min = as.numeric(), 
                                    EC.max = as.numeric(), 
                                    EC.mean = as.numeric(), 
                                    EC.SE = as.numeric(),
                                    DIFF = as.numeric())

diel.summary.year.gas.typ <- data.frame(Year = as.character(),
                                        gas = as.character(),
                                        TYP = as.character(),
                                        Site = as.character(),
                                        ccc = as.numeric(),
                                        count = as.numeric(),
                                        FG.total = as.numeric(),
                                        FG.SE = as.numeric(), 
                                        EC.total = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        FG.min = as.numeric(),
                                        FG.max = as.numeric(),
                                        FG.mean = as.numeric(), 
                                        FG.SE = as.numeric(),
                                        EC.min = as.numeric(), 
                                        EC.max = as.numeric(), 
                                        EC.mean = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        DIFF = as.numeric() )



for( site in site.list){
  for(ccc in ccc.thresholds){
    localdir.site = paste( "/Volumes/MaloneLab/Research/FluxGradient/FluxData/", site, sep="")
    print(paste(site, "CCC threshold =", ccc))
    load(file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
    
    # Add the sit and CCC to the file
    final.DIEL$Site <- site
    final.DIEL$ccc.threshold <- ccc
    
    # Summarize results
    sub.year.gas <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas),
                                                                      ,
                                                                      Site = site,
                                                                      ccc=ccc,
                                                                      count = mean(count),
                                                                      FG.total = sum(abs(FG), na.rm=T), 
                                                                      FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                      EC.total = sum(abs(EC), na.rm=T), 
                                                                      EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                      FG.min = min(FG, na.rm=T),
                                                                      FG.max = max(FG, na.rm=T),
                                                                      FG.mean = mean(FG, na.rm=T), 
                                                                      FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                      EC.min = min(EC, na.rm=T), 
                                                                      EC.max = max(EC, na.rm=T), 
                                                                      EC.mean = mean(EC, na.rm=T), 
                                                                      EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                      DIFF=  mean( DIFF.DIEL, na.rm=T))
    
    sub.year.gas.typ <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas,TYP),
                                                                          Site = site,
                                                                          ccc=ccc,
                                                                          count = mean(count),
                                                                          FG.total = sum(abs(FG), na.rm=T), 
                                                                          FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                          EC.total = sum(abs(EC), na.rm=T), 
                                                                          EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                          FG.min = min(FG, na.rm=T),
                                                                          FG.max = max(FG, na.rm=T),
                                                                          FG.mean = mean(FG, na.rm=T), 
                                                                          FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                          EC.min = min(EC, na.rm=T), 
                                                                          EC.max = max(EC, na.rm=T), 
                                                                          EC.mean = mean(EC, na.rm=T), 
                                                                          EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                          DIFF=  mean( DIFF.DIEL, na.rm=T))
    
    diel.summary.year.gas <- rbind( diel.summary.year.gas,  sub.year.gas)
    diel.summary.year.gas.typ <- rbind( diel.summary.year.gas.typ,  sub.year.gas.typ)
    
    rm(final.DIEL,sub.year.gas,   sub.year.gas.typ)
   
  
  
  
  }}


diel.summary.year.gas.typ 

diel.summary.mean.line.co2 <- diel.summary.year.gas %>% filter(gas=="CO2") %>% reframe(.by=ccc,DIFF.total = mean(DIFF),
                                                                                           DIFF.max = mean(FG.max - EC.max),
                                                                                           DIFF.min = mean(FG.min - EC.min)) %>% fortify()
diel.summary.mean.line.h2o <- diel.summary.year.gas %>% filter(gas=="H2O") %>%  reframe(.by=ccc,DIFF.total = mean(DIFF),
                                                                                            DIFF.max = mean(FG.max - EC.max),
                                                                                            DIFF.min = mean(FG.min - EC.min)) %>% fortify()

# How does the diel change with CCC threshold

diel.summary.year.gas%>% filter(gas=="CO2") 


ggplot(data= diel.summary.year.gas.typ %>% filter(gas=="CO2") ) + geom_point(aes(x= ccc, y=DIFF, col=TYP), alpha= 0.3, size = 3) +scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))


ggplot(data= diel.summary.mean.line.co2) + geom_point(aes(x= ccc, y=DIFF.total), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.total)) +
  geom_point(aes(x= ccc, y=DIFF.max), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.max), col="blue") +
  geom_point(aes(x= ccc, y=DIFF.min), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.min), col="darkgreen") + theme_bw()

ggplot(data= diel.summary.mean.line.h2o) + geom_point(aes(x= ccc, y=DIFF.total), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.total)) +
  geom_point(aes(x= ccc, y=DIFF.max), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.max), col="blue") +
  geom_point(aes(x= ccc, y=DIFF.min), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.min), col="darkgreen") + theme_bw()


diel.summary.mean.line.typ.co2 <- diel.summary.year.gas.typ %>% filter(gas=="CO2") %>% reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                       DIFF.max = mean(FG.max - EC.max),
                                                                                       DIFF.min = mean(FG.min - EC.min)) %>% fortify()
diel.summary.mean.line.typ.h2o <- diel.summary.year.gas.typ %>% filter(gas=="H2O") %>%  reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                        DIFF.max = mean(FG.max - EC.max),
                                                                                        DIFF.min = mean(FG.min - EC.min)) %>% fortify()

# How does the diel change with CCC threshold
ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.total, col=TYP), alpha= 0.4, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.total), alpha=0.1, linetype="dotted", col="grey")+ theme_bw()+scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))

ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.max, col=TYP), alpha= 0.3, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.max), linetype="dotted", col="grey") + theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))

ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.min, col=TYP), alpha= 0.3, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.min), linetype="dotted", col="grey") + theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))


ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.total, col=TYP), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.total), linetype="dotted", col="grey")+ theme_bw()

ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.max, col=TYP), alpha= 0.3, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.max), linetype="dotted", col="grey") + theme_bw()

ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.min, col=TYP), alpha= 0.3, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.min), linetype="dotted", col="grey") + theme_bw()

  
# threshold for good and bad data:

#1. keep all MBR and further filter WP to 0.7 and AE 0.75....
