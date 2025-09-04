# DirRepo <- "." # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
# localdir <- tempdir()

source(fs::path(DirRepo, 'exploratory/FUNCTION_One2One.CCC_testing.R'))
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

# Combine all CCC data with gas indicator
SITES_One2One <- SITES_CCC_CO2 %>% 
  mutate(gas = "CO2") %>% 
  rbind(
    SITES_CCC_H2O %>% 
      mutate(gas = "H2O"))

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
