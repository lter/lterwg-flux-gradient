
source(fs::path(DirRepo,'exploratory/FUNCTION_One2One.R' ))


sites <- names(SITES_WP_9min_FILTER  )


SITES_One2One_CO2 <- one2one.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                        AE.tibble = SITES_AE_9min_FILTER,
                                        WP.tibble = SITES_WP_9min_FILTER, 
                                        gas="CO2")

SITES_One2One_H2O <- one2one.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                        AE.tibble = SITES_AE_9min_FILTER,
                                        WP.tibble = SITES_WP_9min_FILTER, 
                                        gas="H2O")

# Need to add the gas to this table. The point is to get the r2 associated with the max level
Best_Level_CO2 <- SITES_One2One_CO2  %>% 
  reframe(.by =c(Site, Approach ), maxR2 = max(R2)) %>% mutate(gas = "CO2") %>% rbind(
    SITES_One2One_H2O %>% 
      reframe(.by =c(Site, Approach ), maxR2 = max(R2)) %>% mutate(gas = "H2O")) 

SITES_One2One <- SITES_One2One_CO2  %>% mutate(gas = "CO2") %>% rbind(
  SITES_One2One_H2O  %>% mutate(gas = "H2O"))


Best_Level <- SITES_One2One %>% left_join(  Best_Level_CO2, by =c( 'Site', 'Approach', 'gas')) %>% filter( R2 == maxR2 ) %>%  mutate( BestHeight = dLevelsAminusB) %>% select(Site, Approach, gas, BestHeight)


SITES_MBR_9min_FILTER_BH <- list()
SITES_AE_9min_FILTER_BH <- list()
SITES_WP_9min_FILTER_BH <- list()

for( site in unique(SITES_One2One$Site) ){
  print(site)
  
  BH.MBR <- Best_Level %>% filter(Site == site, Approach=='MBR') %>% mutate(site = Site)  
  SITES_MBR_9min_FILTER_BH[[site]] <-  SITES_MBR_9min_FILTER[[site]] %>% full_join( BH.MBR, by= c( 'site','gas') ) %>% filter(dLevelsAminusB == BestHeight) 
  
  
  BH.AE <- Best_Level %>% filter(Site == site, Approach=='AE') %>% mutate(site = Site)  
  SITES_AE_9min_FILTER_BH[[site]] <-  SITES_AE_9min_FILTER[[site]]  %>% full_join( BH.AE, by= c( 'site','gas') ) %>% filter(dLevelsAminusB == BestHeight) 
  
  BH.WP <- Best_Level %>% filter(Site == site, Approach=='WP') %>% mutate(site = Site)  
  
  SITES_WP_9min_FILTER_BH[[site]] <-  SITES_WP_9min_FILTER[[site]]  %>% full_join( BH.WP, by= c( 'site','gas') ) %>% filter(dLevelsAminusB == BestHeight) 
}
