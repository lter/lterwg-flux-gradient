# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'CPER'

# ------ Prerequisites! Make sure these packages are installed ----
# Requires packages: fs, googledrive

# -------------------------------------------------------

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

# Uncomment the next line and comment the following line if you want all the files
#fileDnld <- site_folder$name 
fileDnld <- paste0(site,'_aligned_conc_flux_9min.zip')

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

# Load the data (uncomment those you want to load - must align with download choices made above)
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
load(fileIn)
# 
# fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_9min.Rdata'))
# load(fileIn)
# 
# fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_30min.Rdata'))
# load(fileIn)
# 
# fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_1min.Rdata'))
# load(fileIn)
# 
# fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_WS2D2min.Rdata'))
# load(fileIn)
# 
# fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_attr.Rdata'))
# load(fileIn)


# ------------------- Jackie makes the magic happen --------------



