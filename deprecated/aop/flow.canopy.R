
# Summarize attribute and canopy information to understand where each height relative to the canopy:

library(tidyverse)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites
site.list <- metadata$Site_Id.NEON %>% unique

local <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

# Download and unzip: #### Only if you dont have data
for( site in site.list){
  print(site)
  
  localdir <- paste(local, site, sep="/" )
  
  data_folder_main <- googledrive::drive_ls(path =drive_url ) # need to go into the folder of each site:
  data_folder_site <- googledrive::drive_ls(path =  data_folder_main$id[data_folder_main$name == site] ) # need to go into the folder of each site:
  focal_file_attr <- paste(site,"_attr.zip", sep="")
  
  googledrive::drive_download(file = data_folder_site$id[data_folder_site$name == focal_file_attr ], 
                              path = fs::path(localdir, focal_file_attr),
                              overwrite = T)
  unzip(paste(localdir, focal_file_attr,sep="/"), exdir = localdir)
 

  print("Done")
}
# Import and build the file

# Build dataset
attr.data <- data.frame() # Build a DF:
for( site in site.list){
 
  print(site)
  localdir <- paste(local, site, sep="/" )
  file.new <-paste(site,"_attr.Rdata", sep="")
  localdir.new <- paste( localdir, "data", site, file.new, sep="/")
  load(localdir.new)
  attr.data <- rbind(attr.data, attr.df)
  print("Done")
}

# Canopy Height descriptors:
canopy_A <- attr.data %>% mutate(canopyHeight_m = DistZaxsCnpy %>% as.numeric, 
                     MeasurementHeight_m_A = DistZaxsLvlMeasTow%>% as.numeric,
                     TowerPosition_A = TowerPosition) %>% 
  select(canopyHeight_m, MeasurementHeight_m_A , TowerPosition_A, Site,
  DistZaxsDisp, DistZaxsGrndOfst)


# subset tower position and site to merge every combination:
canopy_B <- canopy_A %>% select( TowerPosition_A, Site, MeasurementHeight_m_A) %>% 
  rename( TowerPosition_B =TowerPosition_A ,
          MeasurementHeight_m_B = MeasurementHeight_m_A )


canopy_B.max <- canopy_B %>% reframe(.by= Site, Max1_Tower_Position = max(TowerPosition_B),
                                     Max2_Tower_Position = max(TowerPosition_B)-1)

canopy_B_final <-canopy_B %>% full_join( canopy_B.max, by="Site")

canopy_commbined <- canopy_A %>% full_join(canopy_B_final, by='Site') %>% 
  filter( TowerPosition_B != TowerPosition_A,
          TowerPosition_B > TowerPosition_A) %>% mutate(TowerLevels = paste(TowerPosition_B, "_", TowerPosition_A, sep="" ))

# I need to determine which height is closest above and closest below the canopy:
load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')
load(file='/Volumes/MaloneLab/Research/FluxGradient/Structral_Diversity.Rdata' )

Sites.Summary # AOP information for the sites
Structural.Diversity.mean.summary  # Diversity information for the different sites.

Sites.Summary.AOP <- Sites.Summary %>% rename(Site = site)
sd.names <- Structural.Diversity.mean.summary %>% names
SD.summary <- Structural.Diversity.mean.summary %>% select(sd.names[c(1, 4:30,85, 95:97)]) 

CanopyInformation <- canopy_commbined %>% full_join( Sites.Summary.AOP, by = "Site") %>% full_join( SD.summary , by = "Site") %>% 
  rename(dLevelsAminusB = TowerLevels) %>% mutate( CanopyHeight= coalesce(CHM.mean, canopyHeight_m )) %>% 
  mutate(Canopy_A= case_when(CanopyHeight - MeasurementHeight_m_A <= 0 ~ 0,
                             CanopyHeight - MeasurementHeight_m_A > 0 ~ 1),
         Canopy_B= case_when(CanopyHeight - MeasurementHeight_m_B <= 0 ~ 0,
                             CanopyHeight - MeasurementHeight_m_B > 0 ~ 1),
         Canopy_L1 = case_when(Canopy_B + Canopy_A == 1 ~ "AW",
                               Canopy_B + Canopy_A == 2 ~ "WW",
                               Canopy_B + Canopy_A == 0 ~ "AA"))

# Compare the canopy height to the CHM
CanopyInformation %>% names
canopyHeight_m
CHM.mean
lm( canopyHeight_m ~CHM.mean, data = CanopyInformation) %>% summary
CanopyInformation %>% ggplot(aes(x= CHM.mean, y = canopyHeight_m) ) + geom_point() + ylim(0, 50) + xlim(0,50) + geom_smooth( method='lm') + geom_abline (slope=1, linetype = "dashed", color="Red")

# Calculate the index for the nearest height above and below the canopy:

attr.h <- attr.data %>% mutate(canopyHeight_m = DistZaxsCnpy %>% as.numeric, 
                               MeasurementHeight_m = DistZaxsLvlMeasTow%>% as.numeric) %>% 
  select(canopyHeight_m, MeasurementHeight_m , TowerPosition, Site)  %>% 
  full_join(CanopyInformation %>% reframe( .by= Site, CHM.mean= mean(CHM.mean)), by="Site") %>% 
  mutate( CanopyHeight = coalesce(CHM.mean,  canopyHeight_m),
          Canopy= case_when(CanopyHeight - MeasurementHeight_m <= 0 ~ 0,
                            CanopyHeight- MeasurementHeight_m > 0 ~ 1))

attr.h %>% summary

nearest.measurement.height <- attr.h %>% select( Site, CanopyHeight, Canopy, MeasurementHeight_m) %>% 
  reframe( .by= c(Site, Canopy), 
           MH.min = min(MeasurementHeight_m),
           MH.max = max(MeasurementHeight_m)) %>% 
  mutate( MH.min =  replace_na(MH.min, 0),
          MH.max =  replace_na(MH.max, 0)) %>%
  mutate(closest.below.MH = case_when(Canopy == 1 ~ MH.max),
         closest.above.MH = case_when(Canopy == 0 ~ MH.max)) %>% 
  reframe(.by= Site, closest.below.MH = mean(closest.below.MH, na.rm=T), 
          closest.above.MH = mean(closest.above.MH, na.rm=T)) 

nearest.measurement.height[ is.na(nearest.measurement.height)] <- 0


CanopyInfo <- CanopyInformation %>% full_join(nearest.measurement.height, by="Site" ) %>% mutate(
  Closet.Below = case_when( closest.below.MH == MeasurementHeight_m_A ~ 1,
                            closest.below.MH == MeasurementHeight_m_B ~ 1,
                            closest.below.MH != MeasurementHeight_m_B & closest.below.MH != MeasurementHeight_m_A  ~ 0),
  Closet.Above = case_when( closest.above.MH == MeasurementHeight_m_A ~ 1,
                            closest.above.MH == MeasurementHeight_m_B ~ 1,
                            closest.above.MH != MeasurementHeight_m_A | closest.above.MH != MeasurementHeight_m_B  ~ 0)) %>% 
  mutate(Canopy_L2 = case_when(Canopy_L1 == "AA" & Closet.Above == 1 & Closet.Below == 1 ~ "AA+-",
                               Canopy_L1 == "AA" & Closet.Above == 1 & Closet.Below != 1 ~ "AA+",
                               Canopy_L1 == "AA" & Closet.Above != 1 & Closet.Below != 1 ~ "AA",
                               Canopy_L1 == "AA" & Closet.Above != 1 & Closet.Below == 1 ~ "AA-",
                               
                               Canopy_L1 == "AW" & Closet.Above == 1 & Closet.Below == 1 ~ "AW+-",
                               Canopy_L1 == "AW" & Closet.Above == 1 & Closet.Below != 1 ~ "AW+",
                               Canopy_L1 == "AW" & Closet.Above != 1 & Closet.Below != 1 ~ "AW",
                               Canopy_L1 == "AW" & Closet.Above != 1 & Closet.Below == 1 ~ "AW-",
                               
                               Canopy_L1 == "WW" & Closet.Above == 1 & Closet.Below == 1 ~ "WW+-",
                               Canopy_L1 == "WW" & Closet.Above == 1 & Closet.Below != 1 ~ "WW+",
                               Canopy_L1 == "WW" & Closet.Above != 1 & Closet.Below != 1 ~ "WW",
                               Canopy_L1 == "WW" & Closet.Above != 1 & Closet.Below == 1 ~ "WW-",) )

# Save and upload the canopy information:

save( CanopyInfo,
      file="/Volumes/MaloneLab/Research/FluxGradient/CanopyInformation.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/CanopyInformation.Rdata")
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
write.csv(CanopyInfo, paste(localdir, "canopy_commbined.csv", sep="/") )
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
fileSave <- file.path(paste(localdir, "canopy_commbined.csv", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

message(" Run flow.evaluation.filter.vizualization")

