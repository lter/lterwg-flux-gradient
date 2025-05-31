# DirRepo <- "." # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
# localdir <- tempdir()
source(fs::path(DirRepo, 'exploratory/FUNCTION_One2One.CCC_testing.R'))
#source(fs::path(DirRepo, 'exploratory/FUNCTION_One2One.CCC.R'))
# Calculate CCC parameters for CO2
SITES_CCC_CO2 <- ccc.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                AE.tibble = SITES_AE_9min_FILTER,
                                WP.tibble = SITES_WP_9min_FILTER, 
                                gas = "CO2")

# Calculate CCC parameters for H2O
SITES_CCC_H2O <- ccc.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                AE.tibble = SITES_AE_9min_FILTER,
                                WP.tibble = SITES_WP_9min_FILTER, 
                                gas = "H2O")

# Find the best level based on maximum CCC for each site, approach, and gas
Best_Level_CCC_CO2 <- SITES_CCC_CO2 %>% 
  reframe(.by = c(Site, Approach), maxCCC = max(CCC, na.rm = TRUE)) %>% 
  mutate(gas = "CO2") %>% 
  rbind(
    SITES_CCC_H2O %>% 
      reframe(.by = c(Site, Approach), maxCCC = max(CCC, na.rm = TRUE)) %>% 
      mutate(gas = "H2O"))

# Combine all CCC data with gas indicator
SITES_One2One <- SITES_CCC_CO2 %>% 
  mutate(gas = "CO2") %>% 
  rbind(
    SITES_CCC_H2O %>% 
      mutate(gas = "H2O"))

# Determine the best height for each site, approach, and gas based on maximum CCC
Best_Level_CCC <- SITES_One2One %>% 
  left_join(Best_Level_CCC_CO2, by = c('Site', 'Approach', 'gas')) %>% 
  filter(CCC == maxCCC | (is.na(CCC) & is.na(maxCCC))) %>%
  mutate(BestHeight = dLevelsAminusB) %>% 
  select(Site, Approach, gas, BestHeight)

# Filter dataset to keep only measurements at the best height
SITES_MBR_9min_FILTER_BH <- list()
SITES_AE_9min_FILTER_BH <- list()
SITES_WP_9min_FILTER_BH <- list()



for(site in unique(SITES_One2One$Site)) {
  print(site)
  
  # Filter MBR data for best height
  BH.MBR <- Best_Level_CCC %>% 
    filter(Site == site, Approach == 'MBR') %>% 
    mutate(site = Site)
  
  SITES_MBR_9min_FILTER_BH[[site]] <- SITES_MBR_9min_FILTER[[site]] %>% 
    full_join(BH.MBR, by = c('site', 'gas')) %>% 
    filter(dLevelsAminusB == BestHeight)
  
  # Filter AE data for best height
  BH.AE <- Best_Level_CCC %>% 
    filter(Site == site, Approach == 'AE') %>% 
    mutate(site = Site)
  
  SITES_AE_9min_FILTER_BH[[site]] <- SITES_AE_9min_FILTER[[site]] %>% 
    full_join(BH.AE, by = c('site', 'gas')) %>% 
    filter(dLevelsAminusB == BestHeight)
  
  # Filter WP data for best height
  BH.WP <- Best_Level_CCC %>% 
    filter(Site == site, Approach == 'WP') %>% 
    mutate(site = Site)
  
  SITES_WP_9min_FILTER_BH[[site]] <- SITES_WP_9min_FILTER[[site]] %>% 
    full_join(BH.WP, by = c('site', 'gas')) %>% 
    filter(dLevelsAminusB == BestHeight)
}

# plots:
setwd(dir.one2one)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
for(site in site.list){
  
  print(site)
  
  mbr <- SITES_MBR_9min_FILTER[[site]] %>% filter(TowerPosition_A ==  max(TowerPosition_A))
  
  ae <- SITES_AE_9min_FILTER[[site]] %>% filter(TowerPosition_A ==  max(TowerPosition_A))
 
  wp <- SITES_WP_9min_FILTER[[site]] %>% filter(TowerPosition_A ==  max(TowerPosition_A))
  
  png(paste(site,"_One2One_CO2.png", sep=""),
      height= 1000, width = 1000) 
  print(ccc.plots(MBR.DF = mbr, 
            AE.DF = ae , 
            WP.DF = wp, 
            gas= "CO2"))
  dev.off() 
  
  png(paste(site,"_One2One_H2O.png", sep=""),
      height= 1000, width = 1000) 
  print(ccc.plots(MBR.DF = mbr, 
            AE.DF = ae , 
            WP.DF = wp, 
            gas= "H2O"))
  dev.off() 
  
  rm( mbr, ae, wp)
  
}
