# flow.evaluation:

# flow.attr.map - makes a map of the sites for manuscript...
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

setwd(localdir)

load( "SITES_WP_30min.Rdata")
load( "SITES_AE_30min.Rdata")
load( "SITES_MBR_30min.Rdata")

# Subset list to only include sites of interest:
 sites <- c("KONZ", "HARV", "JORN", "GUAN")
 
 SITES_MBR_30min <- SITES_MBR_30min[ sites]
 SITES_AE_30min <- SITES_AE_30min[ sites]
 SITES_WP_30min <- SITES_WP_30min[ sites]

 # Application of Filter Functions: ####
 source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.filter.R')
 
 save( SITES_WP_30min_FILTER,SITES_AE_30min_FILTER, SITES_MBR_30min_FILTER ,
       file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 

 # Application of the One2One Analysis ####
 
 source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.One2One.R')
 
 save( SITES_One2One,
       file="/Volumes/MaloneLab/Research/FluxGradient/One2One_MS1Sites.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/One2One_MS1Sites.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 
 save( SITES_WP_30min_FILTER_BH,SITES_AE_30min_FILTER_BH, SITES_MBR_30min_FILTER_BH ,
       file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites_BH.Rdata")
 
 fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites_BH.Rdata")
 googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
 

# Plots fr MS1
dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/FIGURES'

sites <- names(SITES_WP_30min_FILTER_BH  )

for ( i in sites){
  print(i)
  
  plot.it.CO2 <- one2one.plots ( MBR.DF = SITES_MBR_30min_FILTER_BH[i] , 
                                AE.DF = SITES_AE_30min_FILTER_BH[i], 
                                WP.DF = SITES_WP_30min_FILTER_BH[i] , gas = 'CO2')
  print("Ready to plot") 
 
  print(plot.it.CO2)
  
  setwd(dir)
  
  png(paste("One2One_CO2", i,".png", sep=""), width=6, 
              height=5, units="in", res=1200)
          print(plot.it.CO2)
          dev.off()
          
   print("done")       
   
   try(plot.it.H2O <- one2one.plots ( MBR.DF = SITES_MBR_30min_FILTER_BH[i] , 
                                      AE.DF = SITES_AE_30min_FILTER_BH[i], 
                                      WP.DF = SITES_WP_30min_FILTER_BH[i] , gas = 'H2O'), silent = T)
   print("Ready to plot") 
   
   print(plot.it.H2O)
   
   setwd(dir)
   
   png(paste("One2One_H2O", i,".png", sep=""), width=6, 
       height=5, units="in", res=1200)
   print(plot.it.H2O)
   dev.off()
   
   print("done")       
}


# Fit Diurnal for that month:

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.diurnal.R')

save( diurnal.summary.H2O ,diurnal.summary.CO2, 
      file="/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_MS1Sites_BH.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_MS1Sites_BH.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# DIURNAL PLOTS:
for ( i in sites){
 
  df.MBR <-  Diurnal.MBR.CO2[i] %>% as.data.frame
  names( df.MBR ) <- substring( names(df.MBR ), 6)
  
  df.AE <-  Diurnal.AE.CO2[i] %>% as.data.frame
  names( df.AE ) <- substring( names(df.AE ), 6)
  
  df.WP <-  Diurnal.WP.CO2[i] %>% as.data.frame
  names( df.WP ) <- substring( names(df.WP ), 6)
  
  
  p1 <- ggplot( data = df.MBR) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +  facet_wrap(~TowerH, ncol = length(unique(df.MBR$TowerH)) ) 
  
  p2 <- ggplot( data = df.AE) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +  facet_wrap(~TowerH, ncol = length(unique(df.AE$TowerH)) )
  
  p3 <-ggplot( data = df.WP) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +  facet_wrap(~TowerH, ncol = length(unique(df.WP$TowerH)) )
  
  print(ggarrange( p1, p2, p3, nrow=3))
  
  setwd(dir)
  png(paste("Diurnal_", i,".png", sep=""), width=6, 
      height=5, units="in", res=1200)
  print(ggarrange( p1, p2, p3, nrow=3))
  dev.off()
  
  print("done")       

}

# DIURNAL DIFF PLOTS:

diurnal.summary <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.CO2, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.CO2, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.CO2, TYP='WP' ) )  

# Adjust the order of type:

#standardize across sites:

diurnal.summary$Type <- factor( diurnal.summary$Type, levels= c('MBR', 'AE', 'WP'))

for ( i in sites){
  
p1 <- diurnal.summary %>% filter( Site == i) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )
  
 
  print(ggarrange( p1, nrow=1))
  
  setwd(dir)
  png(paste("Diurnal_DIFF_", i,".png", sep=""), width=4, 
      height=4, units="in", res=1200)
  print(ggarrange( p1, nrow=1))
  dev.off()

  print("done")       
  
}


p1 <- diurnal.summary %>% filter( Site == 'HARV' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) ) + ylim(0, 300)

p2 <- diurnal.summary %>% filter( Site == 'KONZ' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

p3 <- diurnal.summary %>% filter( Site == 'GUAN' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

p4 <- diurnal.summary %>% filter( Site == 'JORN' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

png("Diurnal_Diff_sites.png", width=8, 
    height=8, units="in", res=1200)
print(ggarrange( p1,p2, p3, p4, nrow=2,ncol=2, labels=c("a.", "b.", "c.", "d")))
dev.off()

# Carbon Exchange PARMS: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/flow.evaluation.cparms.R')

save( SITES_MBR_30min_CPARMS_FG ,
      SITES_MBR_30min_CPARMS_EC ,
      SITES_AE_30min_CPARMS_FG,
      SITES_AE_30min_CPARMS_EC,
      SITES_WP_30min_CPARMS_EC, 
      SITES_WP_30min_CPARMS_FG ,
      MBR.CPARMS,
      AE.CPARMS ,
      WP.CPARMS,
      file= '/Volumes/MaloneLab/Research/FluxGradient/CarbonParms_MS1Sites.Rdata')

fileSave <- file.path('/Volumes/MaloneLab/Research/FluxGradient/CarbonParms_MS1Sites.Rdata')
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)




merge.CParms <- function( tibble1, tibble2,  TYP1, TYP2){
  
  tibble <- list()
  for( i in names(tibble1)){
    print(i)
    
    df.1 <- tibble1[i] %>% as.data.frame()
    names( df.1) <- substring( names(  df.1), 6)
    
    df.1n <-  df.1 %>% mutate( TYP = TYP1)
    
    
    df.2 <- tibble2[i] %>% as.data.frame()
    names( df.2) <- substring( names(  df.2), 6)
    
    df.2n <-  df.2 %>% mutate( TYP = TYP2)
    
    new.df <- rbind( df.1n, df.2n)
    
    tibble[i] <- list(  new.df)
  }
  
  return(tibble)
}

MBR.CPARMS <- merge.CParms(tibble1 = SITES_MBR_30min_CPARMS_EC,
                           tibble2 = SITES_MBR_30min_CPARMS_FG,
                          TYP1 = 'EC' , TYP2 = 'FG')

AE.CPARMS <- merge.CParms(tibble1 = SITES_AE_30min_CPARMS_EC,
                          tibble2 = SITES_AE_30min_CPARMS_FG,
                           TYP1 = 'EC' , TYP2 = 'FG')


WP.CPARMS <- merge.CParms(tibble1 = SITES_WP_30min_CPARMS_EC,
                          tibble2 = SITES_WP_30min_CPARMS_FG,
                          TYP1 = 'EC' , TYP2 = 'FG')


setwd(dir)

png("CarbonExchange_LRC_MBR.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 

  ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax"), 

  ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax"), ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()


png("CarbonExchange_LRC_AE.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 
  
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax") , 
  
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax"), ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()

png("CarbonExchange_LRC_WP.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 
  
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax"), 
  
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax") , ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()
