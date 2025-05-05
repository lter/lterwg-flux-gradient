
rm(list=ls())

library(dplyr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))
load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/")))

load(file='/Volumes/MaloneLab/Research/FluxGradient/Structral_Diversity.Rdata' )




canopy %>% names
SITES_One2One
SITES_One2One_canopy <- SITES_One2One %>% mutate( TowerLevels = dLevelsAminusB ) %>% full_join(canopy, by=c("Site", "TowerLevels" ) )

Sites.Summary <- Sites.Summary %>% mutate(Site = site)

highest.ccc <- SITES_One2One_canopy %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC)) 
CCC.Summary <- SITES_One2One_canopy %>% full_join( highest.ccc,by = c("Site", "gas", "Approach")) %>% filter( CCC== CCC.max)



ccc_aop.summary <- Sites.Summary %>% full_join(CCC.Summary, by = "Site")

ccc_aop.summary %>% names

ccc_aop.summary %>% filter(gas=="CO2", Canopy_B == 0) %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach))

ccc_aop.summary %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach))


ccc_aop.summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col= as.factor(Canopy)))

ccc_aop.summary %>% names
ccc_aop.summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= canopyHeight_m, col= as.factor(Canopy)))

ccc_aop.summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= canopyHeight_m, col= as.factor(Canopy)))

ccc_aop.summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= canopyHeight_m, col= as.factor(Canopy_A)))

ccc_aop.summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= canopyHeight_m, col= as.factor(Canopy_B)))




