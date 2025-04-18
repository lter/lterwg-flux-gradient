#install.packages("amerifluxr")
library("amerifluxr")
library(dplyr)
library(stringr)
#if(!require(devtools)){install.packages("devtools")}
#devtools::install_github("chuhousen/amerifluxr")


###########################################################

#download ameriflux data directly in your folder
site <- amf_site_info()

neon.sites <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv')

download <- neon.sites %>% filter(check == FALSE)

# if code does not run for all sites, you can select fewer sites

amf_download_fluxnet(user_id = "smalone",  # add your user id
                  user_email = "sparkle.malone@yale.edu",  # add your email
                  site_id = download$Site.Id.AF,
                  data_product = "FLUXNET",
                  data_policy = "CCBY4.0",
                  agree_policy = TRUE,
                  intended_use = "model",
                  intended_use_text = "GradientFlux calculation at NEON sites",
                  verbose = TRUE,
                  out_dir = "/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux")

###########################################################

# Extract information from zipped files:
setwd( '/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux/')
td <- "/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux/Files"

file_to_unzip <- list.files("/Volumes/MaloneLab/Research/FluxGradient/AmeriFlux", pattern =".zip*", full.names = T)

for( i in 1:length(file_to_unzip)){
  unzip(file_to_unzip[i], ex = td)
}

setwd( td)

AUXNEE.files <- list.files(td, pattern ="AUXNEE", full.names = T)
AUXNEE.file.names <- list.files(td, pattern ="AUXNEE", full.names = F)

UThreshold <- data.frame()

for( i in 1:length(AUXNEE.files)){
  print(i)
  
  Site <-  stringr::str_sub( AUXNEE.file.names[i],5,10) # Get the site

  import <- read.csv( AUXNEE.files[i])
  
  # Get the mean threshold and add to UThreshold:
  ustar <- import %>% filter(PARAMETER == "USTAR_THRESHOLD") %>% 
    reframe(.by = TIMESTAMP, VALUE = mean(VALUE, na.rm=T)) %>% 
    mutate(max = max(TIMESTAMP)) %>% filter(TIMESTAMP == max )                                                              
  new.data <- data.frame(Site.Id.AF = Site, Threshold=ustar$VALUE)
  UThreshold <- rbind( UThreshold , new.data)
  
}
  
neon.sites$check <- neon.sites$Site.Id.AF %in% UThreshold$Site.Id.AF

neon.sites <- neon.sites %>% left_join(UThreshold , by= 'Site.Id.AF')

summary <-neon.sites %>% reframe( .by= Vegetation.Abbreviation..IGBP., Threshold.mean= mean(Threshold , na.rm=T) ) 

mean(summary$Threshold.mean, na.rm=T )

neon.sites <- neon.sites %>% left_join(summary, by= 'Vegetation.Abbreviation..IGBP.') %>% 
  mutate( Threshold.final = case_when(is.na(Threshold) == TRUE ~ mean(summary$Threshold.mean, na.rm=T ),
                                      is.na(Threshold) == FALSE ~ Threshold))
neon.sites$Threshold.final

neon.sites$Site_Id.NEON

#* We used the threshold when present, if ot we used the mean threshod for an IGBP class and resorted to the mean across classes for two locations.
write.csv(neon.sites, "/Volumes/MaloneLab/Research/FluxGradient/UstarNeonSites.csv" )
