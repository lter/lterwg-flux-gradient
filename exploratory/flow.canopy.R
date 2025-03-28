
# Summarize attribute information to understand where each height relative to the canopy:
library(tidyverse)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites
site.list <- metadata$Site_Id.NEON %>% unique

local <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
site.list <- c("TEAK", "TOOL", "TREE", "UKFS", "UNDE", "WOOD", "WREF", "YELL")

# Download and unzip:
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


attr.data$Site %>% unique %>% 
# Summarize the different tower height levels...
# we need to understand which heights are within and above the canopy:

canopy_A <- attr.data %>% mutate(canopyHeight_m = DistZaxsCnpy %>% as.numeric, 
                     MeasurementHeight_m_A = DistZaxsLvlMeasTow%>% as.numeric,
                     TowerPosition_A = TowerPosition) %>% 
  select(canopyHeight_m, MeasurementHeight_m_A , TowerPosition_A, Site) %>% 
  mutate(Canopy_A= case_when(canopyHeight_m - MeasurementHeight_m_A <= 0 ~ 0,
                               canopyHeight_m - MeasurementHeight_m_A > 0 ~ 1))

# subset tower position and site to merge every combination.
canopy_B <- canopy_A %>% select( TowerPosition_A, Site, Canopy_A) %>% 
  mutate( TowerPosition_B =TowerPosition_A , Canopy_B= Canopy_A) %>% select(TowerPosition_B, Site, Canopy_B )

canopy_commbined <- canopy_A %>% full_join(canopy_B, by='Site') %>% 
  filter( TowerPosition_B != TowerPosition_A) %>% mutate(TowerLevels = paste(TowerPosition_B, "_", TowerPosition_A, sep="" ),
                                                       Canopy = case_when(Canopy_B + Canopy_A > 0 ~ 1,
                                                                          Canopy_B + Canopy_A == 0 ~ 0))

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
write.csv(canopy_commbined, paste(localdir, "canopy_commbined.csv", sep="/") )

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
fileSave <- file.path(paste(localdir, "canopy_commbined.csv", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

