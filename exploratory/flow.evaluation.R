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

GUAN_H_FILTER <- SITES_AE_30min$GUAN$dLevelsAminusB %>% unique
HARV_H_FILTER <- SITES_AE_30min$HARV$dLevelsAminusB %>% unique
JORN_H_FILTER <- SITES_AE_30min$JORN$dLevelsAminusB %>% unique
KONZ_H_FILTER <- SITES_AE_30min$KONZ$dLevelsAminusB %>% unique

# Application of Flitering Functions: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_Filter_FG.R' )

# Adjust Heights in the file:



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

dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/FIGURES'
setwd(dir)

sites <- names( SITES_MBR_30min)
for ( i in sites){
  print(i)
  
  plot.it <- one2one.plots.co2( MBR.DF = MBR.tibble[i] , 
                                AE.DF = AE.tibble[i], 
                                WP.DF = WP.tibble[i]) 
  
  plot.it 
  setwd(dir)
  png(paste("One2One_", i,".png", sep=""), width=6, 
              height=5, units="in", res=1200)
          plot.it
          dev.off()
          
   print("done")       
}

# Fit Diurnal for that month:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_DIURNAL.R' )

SITES_MBR_30min_FILTER <- TIME.MBR(df.list= SITES_MBR_30min_FILTER) 
SITES_WP_30min_FILTER  <- TIME(df.list= SITES_WP_30min_FILTER)
SITES_AE_30min_FILTER  <- TIME(df.list= SITES_AE_30min_FILTER)


# Warning on this!!!!
Diurnal <- list()
Diurnal$HARV$MBR <- DIURNAL.COMPILE( dataframe=  SITES_MBR_30min_FILTER$HARV,
                                FG_flux = 'FCO2_MBR_H2Otrace_mean' , 
                                EC_flux = 'FC_turb_interp_CO2')


Diurnal$HARV$WP <- DIURNAL.COMPILE( dataframe=  SITES_WP_30min_FILTER$HARV %>% filter(gas == 'CO2'),
                                     FG_flux = 'FG_mean', 
                                    EC_flux = 'FC_turb_interp')

Diurnal$HARV$AE <- DIURNAL.COMPILE( dataframe=  SITES_AE_30min_FILTER$HARV %>% filter(gas == 'CO2'),
                                    FG_flux = 'FG_mean', 
                                    EC_flux = 'FC_turb_interp')


Diurnal$GUAN$MBR <- DIURNAL.COMPILE( dataframe=  SITES_MBR_30min_FILTER$GUAN,
                                     FG_flux = 'FCO2_MBR_H2Otrace_mean' , 
                                     EC_flux = 'FC_turb_interp_CO2')

test <- SITES_MBR_30min_FILTER$GUAN %>% group_by(YearMon, TowerH, Hour) %>% tally( FCO2_MBR_H2Otrace_mean, FC_turb_interp_CO2) 
test %>% summary
#EXAMPLE:

Diurnal$GUAN$MBR <- Diurnal$GUAN$MBR %>% mutate( DIFF = FG-EC)
ggplot( data = Diurnal$GUAN$MBR %>% filter(YearMon == '2023-07', TowerH == '5_3') ) + stat_smooth(aes(x = Hour , y = FG)) + stat_smooth(aes(x = Hour , y = EC), col="red") + stat_smooth(aes(x = Hour , y = DIFF), col="green") 

library(ggpubr)
ggarrange( 
ggplot( data = SITES_MBR_30min$GUAN %>% filter(YearMon == '2023-07', TowerH == '5_3') ) + stat_smooth(aes(x = Hour , y = FG)) + stat_smooth( aes(x = Hour , y = FC_turb_interp_CO2 ), col="red") + ylab("MBR") ,

ggplot( data = SITES_WP_30min$GUAN %>% filter(YearMon == '2023-07', TowerH == '5_3') ) + stat_smooth(aes(x = Hour , y = FG_mean)) + stat_smooth( aes(x = Hour , y = FC_turb_interp ), col="red") + ylab("WP") ,

ggplot( data = SITES_AE_30min$GUAN %>% filter(YearMon == '2023-07', TowerH == '5_3') ) + stat_smooth(aes(x = Hour , y = FG_mean)) + stat_smooth( aes(x = Hour , y = FC_turb_interp ), col="red") + ylab("AE") ,
ncol=1, nrow=3)

MBR$ustar_interp_CO2 %>%  hist
MBR$dConc_CO2
MBR$dConc_pvalue_CO2 %>% hist(n=100)
MBR$FC_nee_interp_CO2
MBR$L_obukhov_CO2 %>% hist(n=100)
MBR$Stability_100 %>% unique
MBR$dLevelsAminusB_CO2
MBR$diffTowerPosition_CO2
MBR$FC_turb_interp_H2O


MBR.sub$diff_H20 <- 
MBR.sub <- MBR %>% filter(dConc_pvalue_H2O <= 0.01, 
                          FH2O_MBR_CO2trace_mean < 60 & FH2O_MBR_CO2trace_mean > -60, ustar_interp_CO2 > 0.25)

MBR.sub %>% ggplot( ) + geom_point( aes(x = FC_turb_interp_H2O, y = FH2O_MBR_CO2trace_mean ) ) + facet_wrap(~dLevelsAminusB_CO2)




# Define Diurnal expectation based on EC data and measure the Diurnal at each height:

MBR %>% filter(dConc_pvalue_H2O <= 0.05, 
               FCH4_MBR_H2Otrace_mean < 60 & FCH4_MBR_H2Otrace_mean > -60) %>% select( dConc_pvalue_H2O , FCH4_MBR_H2Otrace_mean) %>% summary
