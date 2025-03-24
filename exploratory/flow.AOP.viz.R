library(ggplot2)

load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')

Sites.Summary

# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

Sites.Summary %>% names

# Range plots for the AOP ...
plot.range.evi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=EVI.mean - EVI.sd , 
                    xend= EVI.mean + EVI.sd, 
                    y=site), color="black") + 
  theme_bw() + ylab( "") + xlab("EVI")


plot.range.ndvi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=NDVI.mean - NDVI.sd , 
                    xend= NDVI.mean + NDVI.sd, 
                    y=site), color="black") + 
  theme_bw() + ylab( "") + xlab("NDVI")

plot.range.pri <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= PRI.mean - PRI.sd , 
                    xend= PRI.mean + PRI.sd, 
                    y=site), color="black") + 
  theme_bw() + ylab( "") + xlab("PRI")


