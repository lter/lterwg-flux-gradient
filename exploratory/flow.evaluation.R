# flow.evaluation:
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)


# Import data from a  site:
localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/Data'

setwd(localdir)

load( "SITES_WP_30min.Rdata")
load( "SITES_AE_30min.Rdata")
load( "SITES_MBR_30min.Rdata")
load( "SITES_WP_9min.Rdata")
load( "SITES_AE_9min.Rdata")
load( "SITES_MBR_9min.Rdata")

# Add flags to dataframe:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/functions/flag.all.gas.stability.R' )

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/Flow.CrossGradient.R' )


for ( i in 1:length(SITES_AE_30min )){
  SITES_WP_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_9min[[i]], L='L_obukhov')
  SITES_WP_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_30min[[i]], L='L_obukhov')
  SITES_AE_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_9min[[i]], L='L_obukhov')
  SITES_AE_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_30min[[i]], L='L_obukhov')
  SITES_MBR_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_9min[[i]], L='L_obukhov_CO2')
  SITES_MBR_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_30min[[i]], L='L_obukhov_CO2')
  # Calculate the difference between EC and gradient FLux:
  SITES_AE_30min[[i]] <- SITES_AE_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
  SITES_WP_30min[[i]] <- SITES_WP_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
  
}

SITES_AE_9min <- crossGradientDF(SITES_AE_9min )
SITES_WP_9min <- crossGradientDF(SITES_WP_9min )
SITES_AE_30min <- crossGradientDF(SITES_AE_30min )
SITES_WP_30min <- crossGradientDF(SITES_WP_30min)

# Canopy grouping:
setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/Data')

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

# Application of Flitering Functions: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_Filter_FG.R' )

# Need to add in filter function by EC and add the SD of the gradient flux. 

SITES_MBR_30min_FILTER <- EC.filter.MBR ( site.tibble = SITES_MBR_30min,
                flux.limit = 50, 
                ustar.filter= 0.3, 
                FG_sd.limit = 3,
                diff.limit = 10, 
                AE.tibble = SITES_AE_30min)

SITES_AE_30min_FILTER <- EC.filter.AEWP ( site.tibble = SITES_AE_30min,
                 flux.limit = 50, 
                 ustar.filter= 0.3, 
                 FG_sd.limit = 3,
                 diff.limit = 10)

SITES_WP_30min_FILTER <- EC.filter.AEWP ( site.tibble = SITES_WP_30min,
                                          flux.limit = 50, 
                                          ustar.filter= 0.3, 
                                          FG_sd.limit = 3,
                                          diff.limit = 10)
# I need to perform filtering for AE:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_One2One.R' )

sites <- names( SITES_MBR_30min_FILTER)
for ( i in sites){
  print(i)
  
  try(plot.it <- one2one.plots.co2( MBR.DF = SITES_MBR_30min_FILTER[i] , 
                                AE.DF = SITES_AE_30min_FILTER[i], 
                                WP.DF = SITES_WP_30min_FILTER[i]) , silent = T)
  print("Ready to plot") 
 
  print(plot.it)
  
  setwd(dir)
  png(paste("One2One_", i,".png", sep=""), width=6, 
              height=5, units="in", res=1200)
          print(plot.it)
          dev.off()
          
   print("done")       
}

# Fit Diurnal for that month:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_DIURNAL.R' )


SITES_MBR_30min_FILTER <- TIME.MBR(df.list= SITES_MBR_30min_FILTER) 
SITES_WP_30min_FILTER  <- TIME.AEWP(df.list= SITES_WP_30min_FILTER)
SITES_AE_30min_FILTER  <- TIME.AEWP(df.list= SITES_AE_30min_FILTER)


# Calculate Diurnal Patterns by Year-month:
Diurnal.MBR <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_30min_FILTER, 
                               FG_flux = 'FCO2_MBR_H2Otrace_mean', 
                               EC_flux = 'FC_turb_interp_CO2')

Diurnal.WP <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_30min_FILTER, 
                               FG_flux = 'FG_mean', 
                               EC_flux = 'FC_turb_interp')

Diurnal.AE <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_30min_FILTER, 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp')

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

diurnal.summary$Type <- factor( diurnal.summary$Type, levels= c('MBR', 'AE', 'WP'))

for ( i in sites){
  
p1 <- diurnal.summary %>% filter( Site == i) %>%  ggplot( ) + geom_col( aes( y =DIFF.mean, x = TowerH)) + ylab('Diurnal Difference') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )
  
 
  print(ggarrange( p1, nrow=1))
  
  setwd(dir)
  png(paste("Diurnal_DIFF_", i,".png", sep=""), width=4, 
      height=4, units="in", res=1200)
  print(ggarrange( p1, nrow=1))
  dev.off()

  print("done")       
  
}


p1 <- diurnal.summary %>% filter( Site == 'HARV' ) %>%  ggplot( ) + geom_col( aes( y =DIFF.mean, x = TowerH)) + ylab('Diurnal Difference') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )

p2 <- diurnal.summary %>% filter( Site == 'KONZ' ) %>%  ggplot( ) + geom_col( aes( y =DIFF.mean, x = TowerH)) + ylab('Diurnal Difference') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )

p3 <- diurnal.summary %>% filter( Site == 'GUAN' ) %>%  ggplot( ) + geom_col( aes( y =DIFF.mean, x = TowerH)) + ylab('Diurnal Difference') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )

p4 <- diurnal.summary %>% filter( Site == 'JORN' ) %>%  ggplot( ) + geom_col( aes( y =DIFF.mean, x = TowerH)) + ylab('Diurnal Difference') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )


png("Diurnal_Diff_sites.png", width=8, 
    height=8, units="in", res=1200)
print(ggarrange( p1,p2, p3, p4, nrow=2,ncol=2, labels=c("a.", "b.", "c.", "d")))
dev.off()

# Carbon Exchange PARMS: 




