
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

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir, paste0("SITES_One2One.Rdata"))) # Import CCC results

SITES_One2One_ID <- SITES_One2One %>% mutate( Approach = factor(Approach, levels = c("MBR", "AE", "WP")),
                                              Good.CCC = case_when( CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                    CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                    CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                    CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                    CCC < 0.75 & Approach == "WP" ~ 0,
                                                                    CCC < 0.7 & Approach == "AE" ~ 0))


# Calculate Bhatt Coefficient and Distance for Tair, PAR,and VPD: ####

total.bhatt.summary <- data.frame()

for( site in site.list){
  print(site)
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_ID_sub <- SITES_One2One_ID %>% filter(Site == site)
  SITES_One2One_ID_sub %>% summary
  
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

total.bhatt.summary %>% names
total.bhatt.summary %>% ggplot() +  geom_point( aes (x= hellinger.PAR, y = Site),col='goldenrod', alpha=0.2) +  geom_point( aes (x= hellinger.Tair_K, y = Site),col='darkblue', alpha=0.2) +  geom_point( aes (x= hellinger.VPD, y = Site) ,col='darkgreen', alpha=0.3) + theme_bw()

message( paste("Saving data for", site))

save(total.bhatt, file=paste(localdir, "/", "Bhatt.Rdata", sep=""))
fileSave <- paste(localdir, "/","Bhatt.Rdata", sep="")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Temporal Coverage: ####

source(fs::path(DirRepo,'exploratory/flow.temporalCoverage.R' ))

save( sample.diel ,
      sample.month, file = paste(localdir.site, "/", site, "_Temporal_Coverage.Rdata", sep=""))

localdir.site <- paste(localdir,"/", site, sep = "")

write.csv( SITE_9min.report.bhatt,  paste(localdir.site, "/", site,"_9min.report.csv", sep=""))
fileSave <- paste(localdir.site, "/", site, "_Temporal_Coverage.Rdata", sep="")
googledrive::drive_upload(media = fileSave, overwrite = T, path = site_folder)




AE.bhatt <- bhatt.coeff.df(df= AE_9min.df.final , 
                           df.filter = AE_9min_FILTER , 
                           approach = "AE")  %>% na.omit

WP.bhatt <- bhatt.coeff.df(df= WP_9min.df.final , 
                           df.filter = WP_9min_FILTER , 
                           approach = "WP")  %>% na.omit

MBR.bhatt <- bhatt.coeff.df(df= MBR_9min.df.final , 
                            df.filter = MBR_9min_FILTER , 
                            approach = "MBR") %>% na.omit

total.bhatt <- rbind( AE.bhatt, WP.bhatt, MBR.bhatt)








