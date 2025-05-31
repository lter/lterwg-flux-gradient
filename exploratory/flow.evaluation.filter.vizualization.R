#flow.evaluation.filter.Vizualization:

# This script is developed to understand the following:
# 1. How much data is left?
# 2. How much was lost to different filters?
# 3. How well does the remaining data sample hours*months?
# 4. What are the potential biases of the remaining data?

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(DirRepo)

# Compiles Dataframes into one list:
source(fs::path(DirRepo,'exploratory/flow.evaluation_SITELIST.R'))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct


SITES_MBR_9min_FILTER[[site]]
SITES_AE_9min_FILTER[[site]]
SITES_WP_9min_FILTER[[site]]

filter.report %>% summary

canopy <- canopy %>% mutate( dLevelsAminusB =TowerLevels,
                   site = Site)

total.report <- filter.report %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site'))

total.report$approach <- total.report$approach %>% as.factor
# We need to add in canopy information to the file:
total.report$Canopy

total.report %>% na.omit %>%  ggplot( ) + geom_violin( aes( x= approach, y = flag.interaction.ALL))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)")

total.report %>% na.omit  %>%  ggplot( ) + geom_violin( aes( x= approach, y = flag.dConcTSNR))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)")

total.report %>% na.omit %>% ggplot( ) + geom_violin( aes( x= approach, y = flag.dConcSNR))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)")

total.report %>% na.omit %>% ggplot( ) + geom_violin( aes( x= approach, y = flag.ustar_interp))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)")

total.report %>% na.omit %>% ggplot( ) + geom_violin( aes( x= approach, y = total))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)")





