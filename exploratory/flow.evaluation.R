# flow.evaluation:
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)

# Import data from a  site:
localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data'

setwd(localdir)

load( "SITES_WP_30min.Rdata")
load( "SITES_AE_30min.Rdata")
load( "SITES_MBR_30min.Rdata")
load( "SITES_WP_9min.Rdata")
load( "SITES_AE_9min.Rdata")
load( "SITES_MBR_9min.Rdata")

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Function.Format_MBR.R' )
SITES_MBR_30min.r <- apply_format_MBR(SITES_MBR_30min)

# Add flags to dataframe:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/functions/flag.all.gas.stability.R' )

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Flow.CrossGradient.R' )

for ( i in 1:length(SITES_AE_30min )){
  SITES_WP_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_9min[[i]], L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
  SITES_WP_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_30min[[i]], L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
  SITES_AE_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_9min[[i]], L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
  SITES_AE_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_30min[[i]], L='L_obukhov', z='z_veg_aero', d='z_displ_calc')
  SITES_MBR_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_9min[[i]], L='L_obukhov_CO2', z='z_veg_aero_CO2', d='z_displ_calc_CO2')
  SITES_MBR_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_30min[[i]], L='L_obukhov_CO2', z='z_veg_aero_CO2', d='z_displ_calc_CO2')
  # Calculate the difference between EC and gradient FLux:
  SITES_AE_30min[[i]] <- SITES_AE_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
  SITES_WP_30min[[i]] <- SITES_WP_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
  
}

SITES_AE_9min <- crossGradientDF(SITES_AE_9min )
SITES_WP_9min <- crossGradientDF(SITES_WP_9min )
SITES_AE_30min <- crossGradientDF(SITES_AE_30min )
SITES_WP_30min <- crossGradientDF(SITES_WP_30min)

# Canopy grouping:
setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data')

dir <- c('HARV/data/HARV/HARV_attr.Rdata',
         'GUAN/data/GUAN/GUAN_attr.Rdata',
         'JORN/data/JORN/JORN_attr.Rdata',
         'KONZ/data/KONZ/KONZ_attr.Rdata')

site.att <- data.frame()

for( i in 1:length(dir)){
  print(i)
  load(dir[i])
  site.att <- site.att %>% rbind(attr.df )
  rm(attr.df)
}

# Application of Filter Functions: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_Filter_FG.R' )
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_SITELIST_FORMATTING.R' )

message('The current filtering is not gas dependent')

SITES_MBR_30min_FILTER <- Apply.filter ( site.tibble = SITES_MBR_30min.r,
                                         flux.limit = 50, 
                                         ustar.filter= 0.3, 
                                         FG_sd.limit = 3,
                                         diff.limit = 1000,
                                         dConc.limit = 3)  %>% TIME_TOWER_LEVEL_FORMAT( time='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_AE_30min_FILTER <- Apply.filter( site.tibble = SITES_AE_30min,
                 flux.limit = 50, 
                 ustar.filter= 0.3, 
                 FG_sd.limit = 3,
                 diff.limit = 1000,
                 dConc.limit = 3)  %>% TIME_TOWER_LEVEL_FORMAT( time='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_WP_30min_FILTER <- Apply.filter ( site.tibble = SITES_WP_30min,
                                          flux.limit = 50, 
                                          ustar.filter= 0.3, 
                                          FG_sd.limit = 4,
                                          diff.limit = 1000,
                                          dConc.limit = 3)  %>% TIME_TOWER_LEVEL_FORMAT( time='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_One2One.R' )

dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/FIGURES'

sites <- names(SITES_WP_30min_FILTER  )

for ( i in sites){
  print(i)
  
  plot.it.CO2 <- one2one.plots ( MBR.DF = SITES_MBR_30min_FILTER[i] , 
                                AE.DF = SITES_AE_30min_FILTER[i], 
                                WP.DF = SITES_WP_30min_FILTER[i] , gas = 'CO2')
  print("Ready to plot") 
 
  print(plot.it.CO2)
  
  setwd(dir)
  
  png(paste("One2One_CO2", i,".png", sep=""), width=6, 
              height=5, units="in", res=1200)
          print(plot.it.CO2)
          dev.off()
          
   print("done")       
   
   try(plot.it.H2O <- one2one.plots ( MBR.DF = SITES_MBR_30min_FILTER[i] , 
                                      AE.DF = SITES_AE_30min_FILTER[i], 
                                      WP.DF = SITES_WP_30min_FILTER[i] , gas = 'H2O'), silent = T)
   print("Ready to plot") 
   
   print(plot.it.H2O)
   
   setwd(dir)
   
   png(paste("One2One_H2O", i,".png", sep=""), width=6, 
       height=5, units="in", res=1200)
   print(plot.it.H2O)
   dev.off()
   
   print("done")       
}

SITES_One2One_CO2 <- one2one.parms.site(MBR.tibble = SITES_MBR_30min_FILTER,
                                    AE.tibble = SITES_AE_30min_FILTER,
                                    WP.tibble = SITES_WP_30min_FILTER, 
                                    gas="CO2")

SITES_One2One_H2O <- one2one.parms.site(MBR.tibble = SITES_MBR_30min_FILTER,
                                        AE.tibble = SITES_AE_30min_FILTER,
                                        WP.tibble = SITES_WP_30min_FILTER, 
                                        gas="H2O")

SITES_One2One_Best_Level <- SITES_One2One %>% 
  reframe(.by =c(Site, Approach ), maxR2 = max(R2)) %>%
  left_join(SITES_One2One, by=c('Site', 'Approach'))%>% 
  filter(maxR2 ==  R2)

SITES_MBR_30min_FILTER_BH <- list()
SITES_AE_30min_FILTER_BH <- list()
SITES_WP_30min_FILTER_BH <- list()

for( site in unique(SITES_One2One_Best_Level$Site) ){
  print(site)
  
  BH.MBR <- SITES_One2One_Best_Level %>% filter(Site == site, Approach=='MBR' )  %>% select(dLevelsAminusB)
  SITES_MBR_30min_FILTER_BH[[site]] <-  SITES_MBR_30min_FILTER[[site]] %>% 
    filter(dLevelsAminusB ==   BH.MBR$dLevelsAminusB)
  
  BH.AE <- SITES_One2One_Best_Level %>% filter(Site == site, Approach=='AE' )  %>% select(dLevelsAminusB)
  SITES_AE_30min_FILTER_BH[[site]] <-  SITES_AE_30min_FILTER[[site]] %>% 
    filter(dLevelsAminusB ==   BH.AE$dLevelsAminusB)
  
  BH.WP <- SITES_One2One_Best_Level %>% filter(Site == site, Approach=='WP' )  %>% select(dLevelsAminusB)
  SITES_WP_30min_FILTER_BH[[site]] <-  SITES_WP_30min_FILTER[[site]] %>% 
    filter(dLevelsAminusB ==   BH.WP$dLevelsAminusB)
}


# Fit Diurnal for that month:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_DIURNAL.R' )

# Calculate Diurnal Patterns by Year-month:
 
# add gas to the function!!!!
Diurnal.MBR <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_30min_FILTER, 
                               FG_flux = 'FG_mean', 
                               EC_flux = 'FC_turb_interp', gas = "CO2")

Diurnal.WP <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_30min_FILTER, 
                               FG_flux = 'FG_mean', 
                               EC_flux = 'FC_turb_interp', gas = "CO2")

Diurnal.AE <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_30min_FILTER, 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp', gas = "CO2")

# DIURNAL PLOTS:
for ( i in sites){
 
  df.MBR <-  Diurnal.MBR[i] %>% as.data.frame
  names( df.MBR ) <- substring( names(df.MBR ), 6)
  
  df.AE <-  Diurnal.AE[i] %>% as.data.frame
  names( df.AE ) <- substring( names(df.AE ), 6)
  
  df.WP <-  Diurnal.WP[i] %>% as.data.frame
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

diurnal.summary <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP, TYP='WP' ) )  

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
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_LRC_Parms.R' )

SITES_MBR_30min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_MBR_30min_FILTER_BH, 
                             iterations = 10000, 
                             priors.lrc= priors.lrc, 
                             priors.trc= priors.trc, 
                             idx = 'YearMon',
                             PAR = 'PAR',
                             nee = 'FC_turb_interp',
                             TA = 'Tair')

# Check that all the sites are in this thing!

SITES_MBR_30min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_MBR_30min_FILTER_BH, 
                                          iterations = 10000, 
                                          priors.lrc= priors.lrc, 
                                          priors.trc= priors.trc, 
                                          idx = 'YearMon',
                                          PAR = 'PAR_CO2',
                                          nee = 'FCH4_MBR_CO2trace_mean' ,
                                          TA = 'Tair')

SITES_AE_30min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_AE_30min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )


SITES_AE_30min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_AE_30min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )

SITES_WP_30min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_WP_30min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )


SITES_WP_30min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_WP_30min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )
save( SITES_MBR_30min_CPARMS_FG ,
      SITES_MBR_30min_CPARMS_EC ,
      SITES_AE_30min_CPARMS_FG,
      SITES_AE_30min_CPARMS_EC,
      SITES_WP_30min_CPARMS_EC, 
      SITES_WP_30min_CPARMS_FG , file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data/CarbonParms.Rdata')

# Calculate parms and compare the distribution of year_mon parms for the different approaches. 
load('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/data/CarbonParms.Rdata')

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
