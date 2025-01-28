# Import the data:
rm(list=ls())

library(tidyverse)


# Do I need other calculations? I need the stability in this file for the analysis....
load( "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/Data/HARV/HARV_WP_9min.RDATA")
min9.FG.WP.list$CO2 %>% names()

# Canopy heights 6 - 3:

min9.FG.WP.list$CO2$dLevelsAminusB %>% unique
min9.FG.WP.list$CO2$dConc %>% summary()
min9.FG.WP.list$CO2$dConc_sd %>% summary()

min9.FG.WP.list$CO2 %>% filter( dConc_sd < 2 ) %>% ggplot() + geom_point( aes(x=FC_nee_interp , y=FG_mean)) + ylim(-100,100)


lm(FG_mean ~ FC_nee_interp, data = min9.FG.WP.list$CO2 %>% filter( dConc_sd < 2, dLevelsAminusB == "6_5" | dLevelsAminusB == "6_4"| dLevelsAminusB == "6_3") ) %>% summary


# Stability filter ....

# ustar filter....

