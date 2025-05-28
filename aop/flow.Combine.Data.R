rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))
load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

load(file='/Volumes/MaloneLab/Research/FluxGradient/Structral_Diversity.Rdata' )



# Canopy information
canopy# Has height information for the measurement level including what levels are within the canopy
Sites.Summary # AOP information for the sites
Structural.Diversity.mean.summary  # Diversity information for the differet sites.

Sites.Summary.AOP <- Sites.Summary %>% mutate(Site = site)
sd.names <- Structural.Diversity.mean.summary %>% names
SD.summary <- Structural.Diversity.mean.summary %>% select(sd.names[c(1, 4:30,85, 95:97)]) %>% filter(wedge == 8) # Structural
CanopyInfo <- canopy %>% full_join( Sites.Summary.AOP, by = "Site") %>% full_join( SD.summary , by = "Site")


# Save and upload the canopy information:

save( CanopyInfo,
      file="/Volumes/MaloneLab/Research/FluxGradient/CanopyInformation.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/CanopyInformation.Rdata")
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)





# Combine with the summary of CCC: 
SITES_One2One_canopy <- SITES_One2One %>% mutate( TowerLevels = dLevelsAminusB ) %>% full_join(CanopyInfo, by=c("Site", "TowerLevels" ) )


highest.ccc <- SITES_One2One_canopy %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

CCC.Summary <- SITES_One2One_canopy %>% full_join( highest.ccc,by = c("Site", "gas", "Approach")) %>% filter( CCC== CCC.max)
  
CCC.Summary$Approach <- factor(CCC.Summary$Approach, levels = c("MBR", "AE", "WP") )


CCC.Summary %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= as.factor(Canopy_B)), alpha=0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


CCC.Summary %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= as.factor(Canopy_B)), alpha=0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


# Which Sites BH is in the Canopy?
CCC.Summary$Site[CCC.Summary$Canopy_B == 1] %>% unique %>%  length

CCC.Summary$canopyHeight_m[CCC.Summary$Canopy_B == 1] %>% unique %>% ggplot() +geom_histogram() 

CCC.Summary %>% filter(Canopy_B == 1, gas =="CO2") %>% ggplot( aes( x=canopyHeight_m)) + geom_histogram() 

CCC.Summary <-CCC.Summary %>%  mutate( Diff.CH_MH_B = canopyHeight_m - MeasurementHeight_m_B)

CCC.Summary %>% filter(Canopy_B == 1, gas =="CO2") %>% ggplot( aes( x=Diff.CH_MH_B)) + geom_histogram() + xlab("Difference between Canopy and Measuremet Ht")

# How far is the top height from the canopy?
hist(CCC.Summary$canopyHeight_m[CCC.Summary$Canopy_B == 1] - CCC.Summary$MeasurementHeight_m_B [CCC.Summary$Canopy_B == 1])

# Filter within canopy then select the BH:

highest.ccc.2 <- SITES_One2One_canopy %>% filter( Canopy == 0) %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

canopy %>% filter(Site == "YELL")

CCC.Summary.SansCanopy <- SITES_One2One_canopy %>% full_join( highest.ccc.2, by = c("Site", "gas", "Approach")) %>% filter( Canopy == 0) %>% filter( CCC== CCC.max)

# Factor order:
CCC.Summary.SansCanopy$Approach <- factor(CCC.Summary.SansCanopy$Approach, levels = c("MBR", "AE", "WP") )


CCC.Summary.SansCanopy %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= as.factor(Canopy_B)), alpha=0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


# Filter within canopy for B then select the BH:
SITES_One2One_canopy$Canopy_B

highest.ccc.3 <- SITES_One2One_canopy %>% filter( Canopy_B == 0) %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

CCC.Summary.SansCanopyB <- SITES_One2One_canopy %>% full_join( highest.ccc.3, by = c("Site", "gas", "Approach")) %>% filter( Canopy_B == 0) %>% filter( CCC== CCC.max)

# Factor order:
CCC.Summary.SansCanopyB$Approach <- factor(CCC.Summary.SansCanopyB$Approach, levels = c("MBR", "AE", "WP") )


CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= as.factor(Canopy)), alpha=0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


# Canopy Dynamics: ####

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-Grain01-FHD`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-DGF`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-GFP`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-TopRugosity`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-MSDH`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-RumpleIndex`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-TopRugosity`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-VCI`, col= as.factor(Canopy)))


CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-VCI`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-MCH`, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= `Cutoff05-SDH`, col= as.factor(Canopy)))

# AOP

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= LAI.mean, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= PRI.mean, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= NDVI.mean, col= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y= EVI.mean, col= as.factor(Canopy)))


CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() +   geom_density_ridges( aes(y= as.factor(Canopy), x= CCC, fill= as.factor(Canopy)))


CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() +   geom_density_ridges( aes(y= as.factor(Canopy), x= `Cutoff05-Grain01-FHD`, fill = as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() +   geom_density_ridges( aes(y= as.factor(Canopy), x= `Cutoff05-MSDH`, fill= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() +   geom_density_ridges( aes(y= as.factor(Canopy), x= `Cutoff05-VCI`, fill= as.factor(Canopy)))

CCC.Summary.SansCanopyB %>% filter(gas=="CO2") %>%  ggplot() +   geom_density_ridges( aes(y= as.factor(Canopy), x= `Cutoff05-TopRugosity`, fill= as.factor(Canopy)))

