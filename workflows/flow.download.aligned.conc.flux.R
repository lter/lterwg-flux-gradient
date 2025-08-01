# flow.download.aligned.conc.flux 
library(tidyverse)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# Authenticate with Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")

# Data on google drive
data_folder <- googledrive::drive_ls(path = drive_url)

# Flux Data Download:
# Local Directory:
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData' # MaloneLab Server

for( site in site.list){
  
  print(paste("Downloading aligned concentration data from", site, sep=" ") )
  # Download data
  dirTmp <- fs::path(localdir,site)
  dir.create(dirTmp)
  
  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  
  # Uncomment the next line and comment the following line if you want all the files
  fileDnld <- site_folder$name 
  fileDnld <- c(paste0(site,'_aligned_conc_flux_9min.zip'), 
                paste0(site,'_aligned_conc_flux_30min.zip'))
  
  message(paste0('Downloading aligned concentration & flux data for ',site))
  
  for(focal_file in fileDnld){
    
    # Find the file identifier for that file
    file_id <- subset(site_folder, name == focal_file)
    
    # Download that file
    pathDnld <- fs::path(dirTmp,focal_file)
    googledrive::drive_download(file = file_id$id, 
                                path = pathDnld,
                                overwrite = T)
    # Unzip
    if(grepl(pattern='.zip',focal_file)){
      utils::unzip(pathDnld,exdir=dirTmp)
    }
    
  }
}

# Attribute Data Download

# Local Directory:
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/Attributes' # MaloneLab Server

for( site in site.list){
  
  print(paste("Downloading attributes for", site, sep=" ") )
  
  # Download data
  dirTmp <-localdir 
  
  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  
  # Uncomment the next line and comment the following line if you want all the files
  fileDnld <- site_folder$name 
  fileDnld <- c(paste0(site,'_attr.zip'))
    
    # Find the file identifier for that file
    file_id <- subset(site_folder, name == fileDnld)
    
    # Download that file
    pathDnld <- fs::path(dirTmp,fileDnld)
    googledrive::drive_download(file = file_id$id, 
                                path = pathDnld,
                                overwrite = T)
    # Unzip
    if(grepl(pattern='.zip',fileDnld)){
      utils::unzip(pathDnld,exdir=dirTmp)
    }
    
}

message('Next run the flow.calc.flux.batch.R')