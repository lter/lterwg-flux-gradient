# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'TOOL' #'KONZ' BONA CPER GUAN HARV JORN NIWO TOOL

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

# Load the data 
fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.RData'))
load(fileIn)

# Calculate modified Bowen ratio (MBR) gradient fluxes:
# Filter for top-most tower level - use top two
topht_1 = max(min9Diff.list[["CO2"]]$TowerPosition_A)
topht = paste0(topht_1,"_",topht_1-1)
CO2 = dplyr::filter(min9Diff.list[["CO2"]], dLevelsAminusB == topht)
CH4 = dplyr::filter(min9Diff.list[["CH4"]], dLevelsAminusB == topht)
H2O = dplyr::filter(min9Diff.list[["H2O"]], dLevelsAminusB == topht)

# Add gas suffix to all column names to track into combined table
colnames(CO2) <- paste0(colnames(CO2), '_CO2')
colnames(H2O) <- paste0(colnames(H2O), '_H2O')
colnames(CH4) <- paste0(colnames(CH4), '_CH4')

# Keep match_time column for linking among gases 
CO2 = dplyr::rename(CO2, match_time = match_time_CO2)
H2O = dplyr::rename(H2O, match_time = match_time_H2O)
CH4 = dplyr::rename(CH4, match_time = match_time_CH4)

# Align CO2, H2O, CH4 conc diffs and fluxes by match_time
MBRflux_align = dplyr::full_join(CO2, H2O, by = "match_time") 
MBRflux_align = dplyr::full_join(MBRflux_align, CH4, by = "match_time") 
MBRflux_align = dplyr::mutate(MBRflux_align, 
                              month = lubridate::month(match_time), 
                              date = lubridate::date(match_time),
                              hour = lubridate::hour(match_time),
                              year = lubridate::year(match_time))


# Calculate MBR fluxes for all tracer combos
#TO DO: UPDATE TO INCLUDE CALCULATIONS WITH BOTH TURBULENT AND TOTAL FLUXES, ADD CALCULATION OF STORAGE FLUX
MBRflux_align$FCO2_MBR_H2Otrace = MBRflux_align$FH2O_interp_H2O*1000 * (MBRflux_align$dConc_CO2 / (MBRflux_align$dConc_H2O*1000))
MBRflux_align$FH2O_MBR_CO2trace = MBRflux_align$FC_turb_interp_CO2/1000 * (MBRflux_align$dConc_H2O / (MBRflux_align$dConc_CO2/1000))
MBRflux_align$FCH4_MBR_CO2trace = MBRflux_align$FC_turb_interp_CO2/1000 * (MBRflux_align$dConc_CH4 / (MBRflux_align$dConc_CO2/1000))
MBRflux_align$FCH4_MBR_H2Otrace = MBRflux_align$FH2O_interp_H2O*1000 * (MBRflux_align$dConc_CH4 / (MBRflux_align$dConc_H2O*1000))

# -------- Save and zip the file to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp,paste0(site,'_MBRflux.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_MBRflux.zip'))
save(MBRflux_align,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_MBRflux.RData'))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, 
                          path = data_folder$id[data_folder$name==site]) # path might need work

# # 1:1 plots with EC flux & gradient flux
# plot_FCO2 = ggplot(filter(MBRflux_align, hour>10, hour<15)) +
#   geom_point(aes(FC_interp.x, gradFCO2)) +
#   geom_abline(aes(intercept=0, slope=1), color="red", lty=2) +
#   labs(x = "EC CO2 Flux", y = "Gradient CO2 Flux", title = site) +
#   ylim(c(-10,10)) +
#   xlim(c(-10,10)) +
#   theme_minimal()
# 
# plot_FH2O = ggplot(MBRflux_align) +
#   geom_point(aes(FH2O_interp.x, gradFH2O)) +
#   geom_abline(aes(intercept=0, slope=1), color="red", lty=2) +
#   labs(x = "EC H2O Flux", y = "Gradient H2O Flux", title=site) +
#   ylim(c(-5,20)) +
#   xlim(c(-5,20)) +
#   theme_minimal()
# 
# # pdf(paste0("../",site,".pdf"),height=4, width=8)
# # cowplot::plot_grid(CO2, H2O)
# # dev.off()
