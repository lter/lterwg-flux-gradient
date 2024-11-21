# flow.evaluation:
rm(list=ls())

library(tidyverse)
library(ggplot2)

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

for ( i in 1:length(SITES_AE_30min )){
  SITES_WP_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_9min[[i]], L='L_obukhov')
  SITES_WP_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_WP_30min[[i]], L='L_obukhov')
  SITES_AE_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_9min[[i]], L='L_obukhov')
  SITES_AE_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_AE_30min[[i]], L='L_obukhov')
  SITES_MBR_9min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_9min[[i]], L='L_obukhov_CO2')
  SITES_MBR_30min[[i]] <- flag.all.gas.stability(flux.df = SITES_MBR_30min[[i]], L='L_obukhov_CO2')
  
}

# Calculate the difference between EC and gradient FLux:
SITES_AE_30min[[i]] <- SITES_AE_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )
SITES_WP_30min[[i]] <- SITES_WP_30min[[i]] %>% mutate(Diff_EC_GF= FC_turb_interp - FG_mean )


# Observed versus Predicted: Run analysis by height to understand the gradient strength:
# R2; RMSE; Slope; force 0 intercept. What explains residuals?

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


