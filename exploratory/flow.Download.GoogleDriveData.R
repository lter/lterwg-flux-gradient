# Download data from google drive and run MDS analysis:

# Pull data from google drive
email <- 'sparklelmalone@gmail.com'
# ------ Prerequisites! Make sure these packages are installed ----
library(ggplot2)
library(fs)
library(googledrive)
# -------------------------------------------------------

# Authenticate with Google Drive
googledrive::drive_auth(email = TRUE) 

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data folder on google drive
data_folder <- googledrive::drive_ls(path = drive_url)

localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/Data'

download.googledrive <- function( drive_url,data_folder, localdir, site  ) {
  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  
  # Download data
  
  dirTmp <- fs::path(localdir,site)
  dir.create(dirTmp)
  
  # Uncomment the next line and comment the following line if you want all the files
  fileDnld <- site_folder$name 
  #fileDnld <- paste0(site,'_aligned_conc_flux_9min.zip')
  
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
  
  # Load the data 
  fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
  load(fileIn)
  
}

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='HARV'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='BONA'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='CPER'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='GUAN'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='JORN'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='KONZ'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='NIWO'  )

download.googledrive(drive_url=drive_url ,data_folder =data_folder  , localdir= localdir , site='TOOL'  )

