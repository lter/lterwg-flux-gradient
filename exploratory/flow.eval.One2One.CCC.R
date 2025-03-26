# Simple implementation to replace R² with Lin's CCC
library(epiR)

DirRepo <- "." # Relative or absolute path to lterwg-flux-gradient git repo
source(fs::path(DirRepo,'exploratory/FUNCTION_One2One.R' ))
sites <- names(SITES_WP_9min_FILTER)

# Get one2one parameters for CO2 and H2O (this stays the same)
SITES_One2One_CO2 <- one2one.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                        AE.tibble = SITES_AE_9min_FILTER,
                                        WP.tibble = SITES_WP_9min_FILTER, 
                                        gas="CO2")
SITES_One2One_H2O <- one2one.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                        AE.tibble = SITES_AE_9min_FILTER,
                                        WP.tibble = SITES_WP_9min_FILTER, 
                                        gas="H2O")

# Calculate CCC for each site, approach, and height combination
calculate_ccc <- function(site, approach, gas, level) {
  # Select the appropriate data based on approach
  if (approach == "MBR") {
    data_tibble <- SITES_MBR_9min_FILTER[[site]]
  } else if (approach == "AE") {
    data_tibble <- SITES_AE_9min_FILTER[[site]]
  } else if (approach == "WP") {
    data_tibble <- SITES_WP_9min_FILTER[[site]]
  }
  
  # Filter data for this gas and level
  filtered_data <- data_tibble %>% 
    filter(gas == !!gas, dLevelsAminusB == level)
  
  # Calculate CCC if enough data
  if(nrow(filtered_data) > 2) {
    ccc_result <- epiR::epi.ccc(filtered_data$B, filtered_data$A)
    return(ccc_result$rho.c$est)
  } else {
    return(NA)
  }
}

# Add CCC to the one2one data frames
SITES_One2One_CO2$CCC <- mapply(
  calculate_ccc,
  SITES_One2One_CO2$Site,
  SITES_One2One_CO2$Approach,
  "CO2",
  SITES_One2One_CO2$dLevelsAminusB
)

SITES_One2One_H2O$CCC <- mapply(
  calculate_ccc,
  SITES_One2One_H2O$Site,
  SITES_One2One_H2O$Approach,
  "H2O",
  SITES_One2One_H2O$dLevelsAminusB
)

# Now select the best level based on CCC instead of R²
Best_Level_CO2 <- SITES_One2One_CO2 %>% 
  reframe(.by = c(Site, Approach), maxCCC = max(CCC, na.rm = TRUE)) %>% 
  mutate(gas = "CO2") 

Best_Level_H2O <- SITES_One2One_H2O %>% 
  reframe(.by = c(Site, Approach), maxCCC = max(CCC, na.rm = TRUE)) %>% 
  mutate(gas = "H2O")

# Combine the dataframes
SITES_One2One <- SITES_One2One_CO2 %>% mutate(gas = "CO2") %>% 
  rbind(SITES_One2One_H2O %>% mutate(gas = "H2O"))

Best_Level <- SITES_One2One %>% 
  left_join(rbind(Best_Level_CO2, Best_Level_H2O), by = c('Site', 'Approach', 'gas')) %>% 
  filter(CCC == maxCCC) %>%  
  mutate(BestHeight = dLevelsAminusB) %>% 
  select(Site, Approach, gas, BestHeight)

# Filter to keep only the best height data
SITES_MBR_9min_FILTER_BH <- list()
SITES_AE_9min_FILTER_BH <- list()
SITES_WP_9min_FILTER_BH <- list()

for(site in unique(SITES_One2One$Site)) {
  print(site)
  
  BH.MBR <- Best_Level %>% filter(Site == site, Approach=='MBR') %>% mutate(site = Site)  
  SITES_MBR_9min_FILTER_BH[[site]] <- SITES_MBR_9min_FILTER[[site]] %>% 
    full_join(BH.MBR, by= c('site','gas')) %>% 
    filter(dLevelsAminusB == BestHeight) 
  
  BH.AE <- Best_Level %>% filter(Site == site, Approach=='AE') %>% mutate(site = Site)  
  SITES_AE_9min_FILTER_BH[[site]] <- SITES_AE_9min_FILTER[[site]] %>% 
    full_join(BH.AE, by= c('site','gas')) %>% 
    filter(dLevelsAminusB == BestHeight) 
  
  BH.WP <- Best_Level %>% filter(Site == site, Approach=='WP') %>% mutate(site = Site)  
  SITES_WP_9min_FILTER_BH[[site]] <- SITES_WP_9min_FILTER[[site]] %>% 
    full_join(BH.WP, by= c('site','gas')) %>% 
    filter(dLevelsAminusB == BestHeight) 
}