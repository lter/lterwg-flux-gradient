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

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

filter.report %>% summary

canopy <- canopy %>% mutate( dLevelsAminusB =TowerLevels,
                   site = Site)

total.report <- filter.report %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site'))

total.report$approach <- total.report$approach %>% as.factor
# We need to add in canopy information to the file:

total.report %>% na.omit %>%  ggplot( ) + geom_violin( aes( x= approach, y = flag.interaction.ALL))+
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)") + theme_bw() + ylim(0,100)

total.report %>% reframe( .by = c(Canopy),flag.interaction.ALL = mean(flag.interaction.ALL)) 

total.report %>% na.omit %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()


total.report %>% na.omit %>%  ggplot(aes( x= Canopy, y = flag.interaction.ALL, col= Canopy) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

total.report %>% na.omit %>%  ggplot(aes( x= approach, y = flag.interaction.ALL, col= Canopy) ) + geom_violin( ) + 
  ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() + facet_wrap(~Site)


# Which filter led to the most loss?
p.all.ustar <- total.report %>% na.omit %>%  ggplot(aes( x= Canopy, y = flag.ustar_interp, col= Canopy) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + 
  geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

p.all.snr <- total.report %>% na.omit %>%  ggplot(aes( x= Canopy, y = flag.dConcSNR, col= Canopy) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()

p.all.tsnr <- total.report %>% na.omit %>%  ggplot(aes( x= Canopy, y = flag.dConcTSNR, col= Canopy) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
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

p.all.approach <- total.report %>% na.omit %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw()


summary.apprach <- total.report %>% reframe( .by=approach,
                                             flag.ustar_interp = mean(flag.ustar_interp),
                                             flag.dConcSNR = mean(flag.dConcSNR),
                                             flag.dConcTSNR = mean(flag.dConcTSNR),
                                             flag.interaction.ALL = mean(flag.interaction.ALL))


# Summarize Canopy:

total.report %>% na.omit() %>%  filter( Canopy == "AA") %>% select( site) %>% unique %>% count

total.report %>%  filter( Canopy == "AA1") %>% select( site) %>% unique %>% count

total.report %>%  filter( Canopy == "AW") %>% select( site) %>% unique %>% count

total.report %>%  filter( Canopy == "AW1") %>% select( site) %>% unique %>% count

total.report %>%  filter( Canopy == "WW") %>% select( site) %>% unique %>% count

total.report %>%  filter( Canopy == "WW1") %>% select( site) %>% unique %>% count



Canopy.summary <- total.report %>% reframe(.by= c(Canopy) , Sites = site %>% unique %>% length)
 
total.report %>% ggplot( aes( Canopy, col=Canopy, fill=Canopy)) + geom_bar() + theme_bw() + ylab('Measurement Levels') + xlab ("")

all.sites <-total.report$site %>% unique 

listofsites <- total.report %>% na.omit() %>%  filter( Canopy == "AA") %>% select( site) 
AA.sites <- listofsites$site%>% unique 

setdiff(all.sites, AA.sites)
