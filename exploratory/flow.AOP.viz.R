library(ggplot2)

load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')

Sites.Summary

# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)

Sites.Summary %>% names

# Range plots for the AOP ...
plot.range.evi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=EVI.mean - EVI.sd , 
                    xend= EVI.mean + EVI.sd, 
                    y=site), color="black") + 
  ylab( "") + xlab("EVI") + theme(text = element_text(size = 10),
                                               panel.background = element_rect(fill='transparent') ) 


plot.range.ndvi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=NDVI.mean - NDVI.sd , 
                    xend= NDVI.mean + NDVI.sd, 
                    y=site), color="black") + 
  ylab( "") + xlab("NDVI")+ theme(text = element_text(size = 10),
                                  panel.background = element_rect(fill='transparent') ) 

plot.range.pri <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= round(PRI.mean - PRI.sd,2) , 
                    xend= round(PRI.mean + PRI.sd,2), 
                    y=site), color="black") + 
   ylab( "") + xlab("PRI") + theme(text = element_text(size = 10),
                                   panel.background = element_rect(fill='transparent') ) 


ggarrange( plot.range.evi, 
           plot.range.ndvi,
           plot.range.pri, labels=c('a', 'b', 'c'), nrow=1)

# Multidimensional scaling: