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

site.att %>% filter( Site == 'GUAN')

SITES_AE_30min$GUAN$TowerPosition_A
SITES_AE_30min$GUAN$TowerPosition_B

SITES_AE_30min$GUAN$dLevelsAminusB %>% unique
SITES_AE_30min$HARV$dLevelsAminusB %>% unique
SITES_AE_30min$JORN$dLevelsAminusB %>% unique
SITES_AE_30min$KONZ$dLevelsAminusB %>% unique

SITES_WP_30min$GUAN$dLevelsAminusB %>% unique
SITES_WP_30min$HARV$dLevelsAminusB %>% unique
SITES_WP_30min$JORN$dLevelsAminusB %>% unique
SITES_WP_30min$KONZ$dLevelsAminusB %>% unique

SITES_MBR_30min$GUAN$dLevelsAminusB_CO2 %>% unique
SITES_MBR_30min$HARV$dLevelsAminusB_CO2 %>% unique
SITES_MBR_30min$JORN$dLevelsAminusB_CO2 %>% unique
SITES_MBR_30min$KONZ$dLevelsAminusB_CO2 %>% unique

GUAN_H_FILTER <- c("5_4", "5_3", "5_2")
HARV_H_FILTER <- c("6_2", "6_3" , "6_4" ,"6_5")
JORN_H_FILTER <- c("4_2", "4_3" )
KONZ_H_FILTER <- c("4_2", "4_3" )

}

# Calculate the difference between EC and gradient FLux:
SITES_AE_30min[[i]] <- SITES_AE_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
SITES_WP_30min[[i]] <- SITES_WP_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )

# Application of Flitering Functions: ####

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_Filter_FG.R' )

# Need to add in filter function by EC and add the SD of the gradient flux. 

# MBR:

SITES_MBR_30min_FILTER$GUAN <- filter.MBR.CO2.FC( df = SITES_MBR_30min$GUAN,
                                               flux.limit = 30, 
                                               ustar.filter = 0.25, 
                                               H.filter.list = GUAN_H_FILTER,
                                               FG_sd.limit = 2,
                                               diff.limit = 1) 
SITES_MBR_30min_FILTER$HARV <- filter.MBR.CO2.FC( df = SITES_MBR_30min$HARV,
                                               flux.limit = 30, 
                                               ustar.filter = 0.25, 
                                               H.filter.list = HARV_H_FILTER,
                                               FG_sd.limit = 2,
                                               diff.limit = 1) 
SITES_MBR_30min_FILTER$KONZ <- filter.MBR.CO2.FC( df = SITES_MBR_30min$KONZ,
                                               flux.limit = 30, 
                                               ustar.filter = 0.25, 
                                               H.filter.list = KONZ_H_FILTER,
                                               FG_sd.limit = 2,
                                               diff.limit = 1) 
SITES_MBR_30min_FILTER$JORN <- filter.MBR.CO2.FC( df = SITES_MBR_30min$JORN,
                                               flux.limit = 30, 
                                               ustar.filter = 0.25, 
                                               H.filter.list = JORN_H_FILTER,
                                               FG_sd.limit = 2,
                                               diff.limit = 1) 

# WP
SITES_WP_30min_FILTER <- list()
SITES_WP_30min_FILTER$GUAN <- filter.AEWP( df =SITES_WP_30min$GUAN, 
                                           flux.limit=30, 
                                           ustar.filter=0.25, 
                                           H.filter.list = GUAN_H_FILTER, 
                                           diff.limit = 1,
                                           flux.sd.limit=3) 

SITES_WP_30min_FILTER$HARV <- filter.AEWP(df = SITES_WP_30min$HARV, 
                                          flux.limit=30, 
                                          ustar=0.25,
                                          H.filter.list = HARV_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3)

SITES_WP_30min_FILTER$KONZ <- filter.AEWP(df = SITES_WP_30min$KONZ, 
                                          flux.limit=30, 
                                          ustar=0.25, 
                                          H.filter.list = KONZ_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3) 

SITES_WP_30min_FILTER$JORN <- filter.AEWP(df = SITES_WP_30min$JORN, 
                                          flux.limit=30, 
                                          ustar=0.25, 
                                          H.filter.list = JORN_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3)

# AE
SITES_AE_30min_FILTER <- list()
SITES_AE_30min_FILTER$GUAN <- filter.AEWP(df = SITES_AE_30min$GUAN, 
                                          flux.limit=30, 
                                          ustar.filter=0.25, 
                                          H.filter.list = GUAN_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3) 
SITES_AE_30min_FILTER$HARV <- filter.AEWP(df = SITES_AE_30min$HARV, 
                                          flux.limit=30, 
                                          ustar.filter=0.25, 
                                          H.filter.list = HARV_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3)
SITES_AE_30min_FILTER$KONZ <- filter.AEWP(df = SITES_AE_30min$KONZ, 
                                          flux.limit=30, 
                                          ustar.filter=0.25, 
                                          H.filter.list = KONZ_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3) 
SITES_AE_30min_FILTER$JORN <- filter.AEWP(df = SITES_AE_30min$JORN, 
                                          flux.limit=30, 
                                          ustar=0.25, 
                                          H.filter.list = JORN_H_FILTER, 
                                          diff.limit = 1,
                                          flux.sd.limit=3)


# I need to perform filtering for AE:
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_One2One.R' )

one2one(MBR.DF=SITES_MBR_30min_FILTER$JORN , 
        AE.DF = SITES_WP_30min_FILTER$JORN, 
        WP.DF= SITES_WP_30min_FILTER$JORN)

one2one(MBR.DF=SITES_MBR_30min_FILTER$GUAN , 
        AE.DF = SITES_WP_30min_FILTER$GUAN, 
        WP.DF= SITES_WP_30min_FILTER$GUAN )

one2one(MBR.DF=SITES_MBR_30min_FILTER$HARV , 
        AE.DF = SITES_AE_30min_FILTER$HARV, 
        WP.DF= SITES_WP_30min_FILTER$HARV )

one2one(MBR.DF=SITES_MBR_30min_FILTER$KONZ , 
        AE.DF = SITES_AE_30min_FILTER$KONZ, 
        WP.DF= SITES_WP_30min_FILTER$KONZ )

# example of how to fit for sites:
one2one.parms.co2(MBR.DF=SITES_MBR_30min$JORN , 
                  AE.DF = SITES_WP_30min$JORN, 
                  WP.DF= SITES_WP_30min$JORN) %>% mutate( Site = 'JORN')


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
