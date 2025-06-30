#flow.evaluation.filter.Vizualization:

# This script is developed to understand the following:
# 1. How much data is left?
# 2. How much was lost to different filters?
# 3. How well does the remaining data sample hours*months?
# 4. What are the potential biases of the remaining data?

library(tidyverse)

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(DirRepo)

# Compiles Dataframes into one list:
source(fs::path(DirRepo,'exploratory/flow.evaluation_SITELIST.R'))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct  %>% mutate( site = Site)

total.report <- filter.report %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor)
total.report.stability <- filter.report.stability %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor)

# We need to add in canopy information to the file:
total.report %>% names
total.report$flag.interaction.ALL
total.report$Canopy_L2 %>% unique

# Change the order of the L2 factor:

total.report <- total.report %>% mutate( Canopy_L2 =  factor(Canopy_L2, levels = c( "AA", "AA+", "AW+" , "AW-" ,"AW+-", "AW", "WW", "WW-" )))

total.report %>% ggplot( aes( x= approach, y = flag.interaction.ALL)) + geom_violin() +
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)") + theme_bw() + ylim(0,100)

# How much data is lost by approach: 
total.report %>% reframe( .by = c(approach) ,flag.interaction.ALL = mean(flag.interaction.ALL)) 

total.report %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

# How much data is lost by level:
total.report %>% reframe( .by = c(Canopy_L2) ,flag.interaction.ALL = mean(flag.interaction.ALL)) 
total.report %>%  ggplot(aes( x= Canopy_L2, y = flag.interaction.ALL, col= Canopy_L2) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

total.report %>%  ggplot(aes( x= approach, y = flag.interaction.ALL, col= Canopy_L2) ) + geom_violin( ) + 
  ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() + facet_wrap(~Site)


# Which filter led to the most loss?
p.all.ustar <- total.report %>%  ggplot(aes( x= Canopy_L2, y = flag.ustar_interp, col= Canopy_L2) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + 
  geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

p.all.snr <- total.report %>%  ggplot(aes( x= Canopy_L2, y = flag.dConcSNR, col= Canopy_L2) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

p.all.tsnr <- total.report %>%  ggplot(aes( x= Canopy_L2, y = flag.dConcTSNR, col= Canopy_L2) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

ggpubr::ggarrange(p.all.ustar ,
                  p.all.snr,
                  p.all.tsnr, nrow=1, ncol=3, common.legend = TRUE)

total.report$flag.ustar_interp %>% mean
total.report$flag.ustar_interp %>% sd
total.report$flag.dConcSNR %>% mean
total.report$flag.dConcSNR %>% sd
total.report$flag.dConcTSNR %>% mean
total.report$flag.dConcTSNR %>% sd

p.all.approach <- total.report %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()


summary.apprach <- total.report %>% reframe( .by=approach,
                                             flag.ustar_interp = mean(flag.ustar_interp),
                                             flag.dConcSNR = mean(flag.dConcSNR),
                                             flag.dConcTSNR = mean(flag.dConcTSNR),
                                             flag.interaction.ALL = mean(flag.interaction.ALL))


# Summarize Canopy:

total.report %>%  filter( Canopy_L2 == "AA") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "AA+") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "AW+") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "AW+-") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "AW-") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "AW") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "WW") %>% select( site) %>% unique %>% count
total.report %>%  filter( Canopy_L2 == "WW-") %>% select( site) %>% unique %>% count



Canopy.summary <- total.report %>% reframe(.by= c(Canopy_L2) , Sites = site %>% unique %>% length)
 
total.report %>% ggplot( aes( Canopy_L2, col=Canopy_L2, fill=Canopy_L2)) + geom_bar() + theme_bw() + ylab('Measurement Levels') + xlab ("")

all.sites <-total.report$site %>% unique 

listofsites <- total.report %>%  filter( Canopy_L1 == "AA") %>% select( site) 
AA.sites <- listofsites$site%>% unique 

setdiff(all.sites, AA.sites)
