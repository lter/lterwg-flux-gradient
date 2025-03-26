# Pull data from google drive
#email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'HARV'

# ------ Prerequisites! Make sure these packages are installed ----
# Requires packages: fs, googledrive, ggplot2
#library(ggplot2)

source(file.path("functions/calc.mbr.R"))
# -------------------------------------------------------

# Authenticate with Google Drive
#googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

# Uncomment the next line and comment the following line if you want all the files
#fileDnld <- site_folder$name 
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

# Load the data 
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
load(fileIn)
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_30min.RData'))
load(fileIn)

# Calculate MBR flux combos 9 min (CO2 with H2O trace, CH4 with H2O trace, etc)
MBRflux_align = calc.mbr(min9=min9Diff.list, bootstrap=1,
                         nsamp=1000)

# Calculate MBR flux combos 30 min (e.g. CO2 with H2O tracer ...)
MBRflux_align_30min = calc.mbr(min9=min30Diff.list, bootstrap=1, 
                               nsamp=1000)

# # FC with H2O as tracer
# data <- MBRflux_align[c("FC_turb_interp_CO2","FCO2_MBR_H2Otrace_mean")]
# dataComp <- data[complete.cases(data),]
# RFCO2 <- cor.test(data$FC_turb_interp_CO2,data$FCO2_MBR_H2Otrace_mean)
# print(paste0('RFCO2 R-squared = ',round(RFCO2$estimate^2,2)*100,' %'))

# ggplot(data=dplyr::filter(MBRflux_align, dConc_H2O_bin==0)) +
#   geom_point(aes(x=FC_turb_interp_CO2, y=FCO2_MBR_H2Otrace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-15,15)) +
#   xlim(c(-15,15)) +
#   labs(title=paste0(site, ' MBR method (levels 4-3); R-squared = ',round(RFCO2$estimate^2,2)*100,'%')) +
#   theme_minimal()
# 
# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FH2O_interp_H2O, y=FH2O_MBR_CO2trace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-1,5)) +
#   xlim(c(-1,5)) +
#   labs(title = site) +
#   theme_minimal()
# 
# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FCH4_turb_interp_CH4, y=FCH4_MBR_H2Otrace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=site) +
#   theme_minimal()

# #data <- dplyr::filter(MBRflux_align, dLevelsAminusB_CH4=="4_1")[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
# data <- MBRflux_align[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
# dataComp <- data[complete.cases(data),]
# RFCH4 <- cor.test(data$FCH4_turb_interp_CH4,data$FCH4_MBR_CO2trace_mean)
# print(paste0('FCH4 R-squared = ',round(RFCH4$estimate^2,2)*100,' %'))

# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FCH4_turb_interp_CH4, y=FCH4_MBR_CO2trace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-100,100)) +
#   xlim(c(-100,100)) +
#   labs(title=paste0(site, ' MBR method (levels 4-3); R-squared = ',round(RFCH4$estimate^2,2)*100,'%')) +
#   theme_minimal()

# -------- Save and zip the file to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp,paste0(site,'_MBR_9min.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_MBR_9min.zip'))
save(MBRflux_align,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_MBR_9min.RData'))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, 
                          path = data_folder$id[data_folder$name==site]) # path might need work

fileSave <- fs::path(dirTmp,paste0(site,'_MBR_30min.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_MBR_30min.zip'))
save(MBRflux_align_30min,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_MBR_30min.RData'))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, 
                          path = data_folder$id[data_folder$name==site]) # path might need work
