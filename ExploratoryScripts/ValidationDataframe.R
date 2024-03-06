library(dplyr)
# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
user <- "AH"
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1uTshIqUXRarnqNqxiVVXL19GUCgwF7bk")
sitecode <- 'HARV'
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
site_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir(),sitecode)
dir.create(dirTmp)
#focal_file = "KONZ_30m.zip"
for(focal_file in site_folder$name){
  
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
#Load calculated fluxes
#problem loading in .Rdata objects currently being saved to 2 different directories?
fileIn <- fs::path(dirTmp, paste0(sitecode,'_MBRflux.Rdata'))
load(fileIn)
fileIn <- fs::path(dirTmp,'data',sitecode,paste0(sitecode,'_AE_AH_2024-03-05.Rdata'))
load(fileIn)
fileIn <- fs::path(dirTmp,'data',sitecode,paste0(sitecode,'_WP_AH_2024-03-05.Rdata'))
load(fileIn)
#add gas identifier column and site identifier column to AE and WP data frames
min9.FG.AE.list$H2O$gas <- "H2O"
min9.FG.AE.list$H2O$site <- paste0(sitecode)
min9.FG.AE.list$CO2$gas <- "CO2"
min9.FG.AE.list$CO2$site <- paste0(sitecode)
min9.FG.AE.list$CH4$gas <- "CH4"
min9.FG.AE.list$CH4$site <- paste0(sitecode)

min9.FG.WP.list$H2O$gas <- "H2O"
min9.FG.WP.list$H2O$site <- paste0(sitecode)
min9.FG.WP.list$CO2$gas <- "CO2"
min9.FG.WP.list$CO2$site <- paste0(sitecode)
min9.FG.WP.list$CH4$gas <- "CH4"
min9.FG.WP.list$CH4$site <- paste0(sitecode)
#bind list into df
AE.df <- bind_rows(min9.FG.AE.list)
WP.df <- bind_rows(min9.FG.WP.list)
#rename dataframes to be site specific so can combine into one list object that will then become large validation dataframe
BONA_MBR <- MBRflux_align
rm(MBRflux_align)
BONA_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
BONA_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

CPER_MBR <- MBRflux_align
rm(MBRflux_align)
CPER_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
CPER_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

GUAN_MBR <- MBRflux_align
rm(MBRflux_align)
GUAN_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
GUAN_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

HARV_MBR <- MBRflux_align
rm(MBRflux_align)
HARV_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
HARV_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

JORN_MBR <- MBRflux_align
rm(MBRflux_align)
JORN_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
JORN_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

KONZ_MBR <- MBRflux_align
rm(MBRflux_align)
KONZ_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
KONZ_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

NIWO_MBR <- MBRflux_align
rm(MBRflux_align)
NIWO_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
NIWO_WP <- WP.df
rm(min9.FG.WP.list, WP.df)

TOOL_MBR <- MBRflux_align
rm(MBRflux_align)
TOOL_AE <- AE.df
rm(min9.FG.AE.list, AE.df)
TOOL_WP <- WP.df
rm(min9.FG.WP.list, WP.df)
#build list of all sites
SITES_MBR <- list(KONZ = KONZ_MBR, BONA = BONA_MBR, CPER = CPER_MBR, GUAN = GUAN_MBR, HARV = HARV_MBR, JORN= JORN_MBR, NIWO = NIWO_MBR, TOOL = TOOL_MBR)
SITES_AE <- list(KONZ = KONZ_AE, BONA = BONA_AE, CPER = CPER_AE, GUAN = GUAN_AE, HARV = HARV_AE, JORN = JORN_AE, NIWO = NIWO_AE, TOOL = TOOL_AE)
SITES_WP <- list(KONZ = KONZ_WP, BONA = BONA_WP, CPER = CPER_WP, GUAN = GUAN_WP, HARV = HARV_WP, JORN = JORN_WP, NIWO = NIWO_WP, TOOL = TOOL_WP)
#save lists as .Rdata objects
save(SITES_MBR, file = file.path("data", "Validation", "SITES_MBR.Rdata"))
save(SITES_AE, file = file.path("data", "Validation", "SITES_AE.Rdata"))
save(SITES_WP, file = file.path("data", "Validation", "SITES_WP.Rdata"))
#zip
zip(zipfile = paste0("data/Validation/SITES_MBR.zip"), files = paste0("data/Validation/SITES_MBR.Rdata"))
zip(zipfile = paste0("data/Validation/SITES_AE.zip"), files = paste0("data/Validation/SITES_AE.Rdata"))
zip(zipfile = paste0("data/Validation/SITES_WP.zip"), files = paste0("data/Validation/SITES_WP.Rdata"))
#reset drive url to upload
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
#upload to g drive
googledrive::drive_upload(media = paste0("data/Validation/SITES_MBR.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/Validation/SITES_AE.zip"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/Validation/SITES_WP.zip"), overwrite = T, path = drive_url)
