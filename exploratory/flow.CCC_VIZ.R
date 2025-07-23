# CCC Viz and Evaluation:

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when( CCC >= 0.5 ~ 1,
                                                                                             CCC <0.5 ~ 0))

SITES_One2One_canopy.Highest <- SITES_One2One_canopy %>% filter( CCC== CCC.max)


SITES_One2One_canopy.Highest.100 <- SITES_One2One_canopy %>% filter( count > 100) %>% filter( CCC== CCC.max) 

# CO2: #####
plot.CCC.CO2.all <- SITES_One2One_canopy %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") 


plot.CCC.CO2.t0.5 <- SITES_One2One_canopy %>% filter(gas=="CO2", CCC> 0.50) %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= Canopy_L1), alpha= 0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")

#H2O: #####

plot.CCC.H2O.all <- SITES_One2One_canopy %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") 

plot.CCC.H2O.t0.5 <- SITES_One2One_canopy %>% filter(gas=="H2O", CCC> 0.50) %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= Canopy_L1), alpha= 0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


# Count Evaluation:

Summary.CCC.t0.5 <- SITES_One2One_canopy %>% filter( CCC> 0.50) %>% reframe( .by=c(Canopy_L2, gas),
                                                                             Count_ML= length(Canopy_L2),
                                                                             Count_Sites = length(Site %>% unique))

counts.CO2.CCC<- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="CO2") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)')    

counts.CO2.CCC.approach <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="CO2") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') + facet_wrap(~ Canopy_L2, ncol=4)

counts.H2O.CCC <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="H2O") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') 

counts.H2O.CCC.approach <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="H2O") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') + facet_wrap(~ Canopy_L2, ncol=4)

# Relationship betwee counts and CCC:
plot.countbyCCC <- SITES_One2One_canopy  %>% ggplot(aes( x=count, y = CCC)) + geom_point() + 
  theme_bw() + geom_smooth(method = "loess")

plot.countbyApproach <- SITES_One2One_canopy %>%  ggplot(aes( y=count, x = Approach)) + geom_violin() + 
  theme_bw() + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
setwd(localdir)
save(plot.CCC.CO2.all, plot.CCC.CO2.t0.5 ,
     plot.CCC.H2O.all, plot.CCC.H2O.t0.5 ,
     plot.countbyCCC, plot.countbyCCC,plot.countbyApproach,counts.CO2.CCC, counts.CO2.CCC.approach,counts.H2O.CCC, counts.H2O.CCC.approach,
     SITES_One2One_canopy, SITES_One2One_canopy.Highest, SITES_One2One_canopy.Highest.100,
      Summary.CCC.t0.5, file = "flow.CCC_VIZ.R" )
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
fileSave <- file.path(paste(localdir, "flow.CCC_VIZ.R", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
