# Clean up google drive:
library(tidyverse)
library( googledrive)


# We no longer need the 30min flux data for the MBR, AE, and WP:

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites
all.site.list <- metadata$Site_Id.NEON %>% unique

for( site in site.list){
  print(site)
  data_folder_main <- googledrive::drive_ls(path =drive_url ) # need to go into the folder of each site:
  
  data_folder_site <- googledrive::drive_ls(path =  data_folder_main$id[data_folder_main$name == site] ) # need to go into the folder of each site:
  
  focal_file_AE.rdata <- paste(site,"_AE_30min.Rdata", sep="")
  focal_file_AE <- paste(site,"_AE_30min.zip", sep="")
  focal_file_WP <- paste(site,"_WP_30min.zip", sep="")
  focal_file_MBR <- paste(site,"_MBR_30min.zip", sep="")
  
  googledrive::drive_trash(file = data_folder_site$id[data_folder_site$name == focal_file_AE.rdata])
  googledrive::drive_trash(file = data_folder_site$id[data_folder_site$name == focal_file_AE])
  googledrive::drive_trash(file = data_folder_site$id[data_folder_site$name == focal_file_WP])
  googledrive::drive_trash(file = data_folder_site$id[data_folder_site$name == focal_file_MBR])
  
  rm(focal_file_AE.rdata,focal_file_AE, focal_file_WP, focal_file_MBR   )
  print("Done")
}
