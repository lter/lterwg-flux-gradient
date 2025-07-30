
# compare distributions in filtered and non-filtered conditions:

library(dispRity)
library(tidyverse)
library(sf)

# https://rdrr.io/bioc/immunoClust/man/bhattacharyya.html

# Bhatt:
# This script uses flow.temporalcoverage and flow.bhatt to evaluate the representativeness of the the twice filtered data:
source("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/calc_bhatt_coefficient.R" )

# The Diel analysis is currently set up by CCC threshold:
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)
googledrive::drive_auth(email = email)

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir, paste0("SITES_One2One.Rdata"))) # Import CCC results

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all 
site.list <- metadata$Site_Id.NEON %>% unique

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

SITES_One2One_ID <- SITES_One2One %>% mutate( Approach = factor(Approach, levels = c("MBR", "AE", "WP")),
                                              Good.CCC = case_when( CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                    CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                    CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                    CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                    CCC < 0.75 & Approach == "WP" ~ 0,
                                                                    CCC < 0.7 & Approach == "AE" ~ 0)) %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) 

# Calculate Bhatt Coefficient and Distance for Tair, PAR,and VPD: ####

total.bhatt.summary <- data.frame()

for( site in site.list){
  print(site)
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_ID_sub <- SITES_One2One_ID %>% filter(Site == site)
  
  if( sum( SITES_One2One_ID_sub$Good.CCC) > 0){
    
    MBR_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter( Good.CCC == 1) %>% select( Tair_K, PAR, VPD)
    
    WP_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)%>% select( Tair_K, PAR, VPD)
    
    AE_9min_FILTER_CCC <- SITES_One2One_ID_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")%>% filter( Good.CCC == 1)%>% select( Tair_K, PAR, VPD)
    
    DF.FILTER_CCC <- rbind(MBR_9min_FILTER_CCC, WP_9min_FILTER_CCC, AE_9min_FILTER_CCC   )
    
    try( bhatt <- bhatt.coeff.df(df= DF.FILTER_CCC , 
                               df.filter = MBR_9min_FILTER_CCC) %>% na.omit , silent=T)
    
    
    
    total.bhatt <- bhatt  %>% mutate(Site = site)
    
    total.bhatt.summary <- rbind( total.bhatt.summary, total.bhatt)
    
  }
  
  
}

site.list.df <-  data.frame(Site=site.list)

total.bhatt.summary <- total.bhatt.summary %>% full_join( site.list.df)
save(total.bhatt.summary, file=paste(localdir, "/", "Bhatt.Rdata", sep=""))
fileSave <- paste(localdir, "/","Bhatt.Rdata", sep="")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Viz Bhatt: ####
load(file=paste(localdir, "/", "Bhatt.Rdata", sep="") )


plot.bhatt.par <- total.bhatt.summary %>% ggplot() +  geom_point( aes (x= Bhatt.coe.PAR, y = Site),col='black', alpha=0.5)  + theme_bw() + xlim(0.95, 1.01) + xlab("PAR")+ ylab("")

plot.bhatt.tair <- total.bhatt.summary %>% ggplot() +  geom_point( aes (x= Bhatt.coe.Tair_K, y = Site),col='black', alpha=0.5)  + theme_bw() + xlim(0.95, 1.01)+ xlab("Tair")+ ylab("")

plot.bhatt.vpd <- total.bhatt.summary %>% ggplot() +  geom_point( aes (x= Bhatt.coe.VPD, y = Site) ,col='black', alpha=0.5)  + theme_bw() + xlim(0.95, 1.01)+ xlab("VPD") + ylab("")

library(ggpubr)

ggarrange(plot.bhatt.par, 
       plot.bhatt.tair,
       plot.bhatt.vpd, ncol=3)

# Temporal Coverage: ####


temporal.coverage.month <- data.frame()
temporal.coverage.diel <- data.frame()

for( site in site.list){
 
  print(site)
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_ID_sub <- SITES_One2One_ID %>% filter(Site == site)
  
  if( sum( SITES_One2One_ID_sub$Good.CCC) > 0){
    source(fs::path(DirRepo,'exploratory/flow.temporalCoverage.R' ))
    
    temporal.coverage.month <- rbind(temporal.coverage.month, sample.month)
    temporal.coverage.diel <- rbind(temporal.coverage.diel, sample.diel)
  }
  print('done')
  
}


save( temporal.coverage.month,
      temporal.coverage.diel, file = paste(localdir.site, "/", site, "_Temporal_Coverage.Rdata", sep=""))
fileSave <- paste(localdir.site, "/", site, "_Temporal_Coverage.Rdata", sep="")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
