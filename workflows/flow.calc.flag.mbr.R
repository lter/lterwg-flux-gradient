# Pull data from google drive
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
email <- 'csturtevant@battelleecology.org'
site <- 'US-Uaf'

# ------ Prerequisites! Make sure these packages are installed ----
# Requires packages: fs, googledrive, ggplot2
library(ggplot2)
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

# Sample over concentration mean & variance 
FCO2_MBR_H2Otrace = FH2O_MBR_CO2trace = FCH4_MBR_CO2trace = FCH4_MBR_H2Otrace = vector()
FCH4_MBR_H2Otrace_mean = FCH4_MBR_H2Otrace_lo = FCH4_MBR_H2Otrace_hi = FCH4_MBR_H2Otrace_sd = vector()
FCO2_MBR_H2Otrace_mean = FCO2_MBR_H2Otrace_lo = FCO2_MBR_H2Otrace_hi = FCO2_MBR_H2Otrace_sd = vector()
FCH4_MBR_CO2trace_mean = FCH4_MBR_CO2trace_lo = FCH4_MBR_CO2trace_hi = FCH4_MBR_CO2trace_sd = vector()
FH2O_MBR_CO2trace_mean = FH2O_MBR_CO2trace_lo = FH2O_MBR_CO2trace_hi = FH2O_MBR_CO2trace_sd = vector()
dConc_CO2_mean = dConc_CO2_sd = dConc_H2O_mean = dConc_H2O_sd = dConc_CH4_mean = dConc_CH4_sd = vector()

nsamp = 1000

for(i in 1:nrow(MBRflux_align)){ # loop over time to sample conc 
  
  # Draw nsamp from normal with mean & sd of concentration
  cConc_CO2_A = rnorm(n = nsamp, mean = MBRflux_align$mean_A_CO2[i],
                      sd = sqrt(MBRflux_align$vari_A_CO2[i]))
  cConc_CO2_B = rnorm(n = nsamp, mean = MBRflux_align$mean_B_CO2[i],
                      sd = sqrt(MBRflux_align$vari_B_CO2[i]))
  dConc_CO2 = cConc_CO2_A-cConc_CO2_B
  
  cConc_H2O_A = rnorm(n = nsamp, mean = MBRflux_align$mean_A_H2O[i],
                      sd = sqrt(MBRflux_align$vari_A_H2O[i]))
  cConc_H2O_B = rnorm(n = nsamp, mean = MBRflux_align$mean_B_H2O[i],
                      sd = sqrt(MBRflux_align$vari_B_H2O[i]))
  dConc_H2O = cConc_H2O_A-cConc_H2O_B
  
  cConc_CH4_A = rnorm(n = nsamp, mean = MBRflux_align$mean_A_CH4[i],
                      sd = sqrt(MBRflux_align$vari_A_CH4[i]))
  cConc_CH4_B = rnorm(n = nsamp, mean = MBRflux_align$mean_B_CH4[i],
                      sd = sqrt(MBRflux_align$vari_B_CH4[i]))
  dConc_CH4 = cConc_CH4_A-cConc_CH4_B
  
  # Save dConc mean & var
  dConc_CO2_mean[i] = mean(dConc_CO2)
  dConc_CO2_sd[i] = sd(dConc_CO2)
  dConc_CH4_mean[i] = mean(dConc_CH4)
  dConc_CH4_sd[i] = sd(dConc_CH4)
  dConc_H2O_mean[i] = mean(dConc_H2O)
  dConc_H2O_sd[i] = sd(dConc_H2O)
  
  for(j in 1:nsamp){ # loop over sampled conc to calculate flux
    # Calculate MBR fluxes for all tracer combos
    FCO2_MBR_H2Otrace[j] = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O[i] * 
      (dConc_CO2[j] / dConc_H2O[j])),MBRflux_align$FH2O_interp_H2O[i] * 
        (dConc_CO2[j] / dConc_H2O[j]),NA)
    
    FH2O_MBR_CO2trace[j] = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2[i] *
      (dConc_H2O[j] / dConc_CO2[j])),MBRflux_align$FC_turb_interp_CO2[i] *
        (dConc_H2O[j] / dConc_CO2[j]), NA)
    
    FCH4_MBR_CO2trace[j] = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2[i] *
      (dConc_CH4[j] /dConc_CO2[j])), MBRflux_align$FC_turb_interp_CO2[i] *
        (dConc_CH4[j] /dConc_CO2[j]), NA)
    
    FCH4_MBR_H2Otrace[j] = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O[i] * 
      (dConc_CH4[j] / dConc_H2O[j])), MBRflux_align$FH2O_interp_H2O[i] * 
        (dConc_CH4[j] / dConc_H2O[j]), NA)
  }
  
  #calculate lower and upper bounds of confidence interval
  FCH4_MBR_H2Otrace_mean[i] = mean(FCH4_MBR_H2Otrace)
  FCH4_MBR_H2Otrace_lo[i] = mean(FCH4_MBR_H2Otrace) -
    qt(0.95,df=nsamp-1)*sd(FCH4_MBR_H2Otrace)/sqrt(nsamp)
  FCH4_MBR_H2Otrace_hi[i] = mean(FCH4_MBR_H2Otrace) +
    qt(0.95,df=nsamp-1)*sd(FCH4_MBR_H2Otrace)/sqrt(nsamp)
  FCH4_MBR_H2Otrace_sd[i] = sd(FCH4_MBR_H2Otrace)
  
  FCH4_MBR_CO2trace_mean[i] = mean(FCH4_MBR_CO2trace)
  FCH4_MBR_CO2trace_lo[i] = mean(FCH4_MBR_CO2trace) -
    qt(0.95,df=nsamp-1)*sd(FCH4_MBR_CO2trace)/sqrt(nsamp)
  FCH4_MBR_CO2trace_hi[i] = mean(FCH4_MBR_CO2trace) +
    qt(0.95,df=nsamp-1)*sd(FCH4_MBR_CO2trace)/sqrt(nsamp)
  FCH4_MBR_CO2trace_sd[i] = sd(FCH4_MBR_CO2trace)
  
  FCO2_MBR_H2Otrace_mean[i] = mean(FCO2_MBR_H2Otrace)
  FCO2_MBR_H2Otrace_lo[i] = mean(FCO2_MBR_H2Otrace) -
    qt(0.95,df=nsamp-1)*sd(FCO2_MBR_H2Otrace)/sqrt(nsamp)
  FCO2_MBR_H2Otrace_hi[i] = mean(FCO2_MBR_H2Otrace) +
    qt(0.95,df=nsamp-1)*sd(FCO2_MBR_H2Otrace)/sqrt(nsamp)
  FCO2_MBR_H2Otrace_sd[i] = sd(FCO2_MBR_H2Otrace)
  
  FH2O_MBR_CO2trace_mean[i] = mean(FH2O_MBR_CO2trace)
  FH2O_MBR_CO2trace_lo[i] = mean(FH2O_MBR_CO2trace) -
    qt(0.95,df=nsamp-1)*sd(FH2O_MBR_CO2trace)/sqrt(nsamp)
  FH2O_MBR_CO2trace_hi[i] = mean(FH2O_MBR_CO2trace) +
    qt(0.95,df=nsamp-1)*sd(FH2O_MBR_CO2trace)/sqrt(nsamp)
  FH2O_MBR_CO2trace_sd[i] = sd(FH2O_MBR_CO2trace)
  
}

MBRflux_align$FCH4_MBR_H2Otrace_mean = FCH4_MBR_H2Otrace_mean
MBRflux_align$FCH4_MBR_H2Otrace_lo = FCH4_MBR_H2Otrace_lo
MBRflux_align$FCH4_MBR_H2Otrace_hi = FCH4_MBR_H2Otrace_hi
MBRflux_align$FCH4_MBR_H2Otrace_sd = FCH4_MBR_H2Otrace_sd

MBRflux_align$FCH4_MBR_CO2trace_mean = FCH4_MBR_CO2trace_mean
MBRflux_align$FCH4_MBR_CO2trace_lo = FCH4_MBR_CO2trace_lo
MBRflux_align$FCH4_MBR_CO2trace_hi = FCH4_MBR_CO2trace_hi
MBRflux_align$FCH4_MBR_CO2trace_sd = FCH4_MBR_CO2trace_sd

MBRflux_align$FCO2_MBR_H2Otrace_mean = FCO2_MBR_H2Otrace_mean
MBRflux_align$FCO2_MBR_H2Otrace_lo = FCO2_MBR_H2Otrace_lo
MBRflux_align$FCO2_MBR_H2Otrace_hi = FCO2_MBR_H2Otrace_hi
MBRflux_align$FCO2_MBR_H2Otrace_sd = FCO2_MBR_H2Otrace_sd

MBRflux_align$FH2O_MBR_CO2trace_mean = FH2O_MBR_CO2trace_mean
MBRflux_align$FH2O_MBR_CO2trace_lo = FH2O_MBR_CO2trace_lo
MBRflux_align$FH2O_MBR_CO2trace_hi = FH2O_MBR_CO2trace_hi
MBRflux_align$FH2O_MBR_CO2trace_sd = FH2O_MBR_CO2trace_sd

MBRflux_align$dConc_CO2_mean = dConc_CO2_mean
MBRflux_align$dConc_CO2_sd = dConc_CO2_sd
MBRflux_align$dConc_H2O_mean = dConc_H2O_mean
MBRflux_align$dConc_H2O_sd = dConc_H2O_sd
MBRflux_align$dConc_CH4_mean = dConc_CH4_mean
MBRflux_align$dConc_CH4_sd = dConc_CH4_sd

MBRflux_align$dConc_CO2_bin = ifelse((MBRflux_align$dConc_CO2_mean-MBRflux_align$dConc_CO2_sd*2)<0 &
                                       (MBRflux_align$dConc_CO2_mean+MBRflux_align$dConc_CO2_sd*2)>0,1,0)

MBRflux_align$dConc_H2O_bin = ifelse((MBRflux_align$dConc_H2O_mean-MBRflux_align$dConc_H2O_sd*2)<0 &
                                       (MBRflux_align$dConc_H2O_mean+MBRflux_align$dConc_H2O_sd*2)>0,1,0)

MBRflux_align$dConc_CH4_bin = ifelse((MBRflux_align$dConc_CH4_mean-MBRflux_align$dConc_CH4_sd*2)<0 &
                                       (MBRflux_align$dConc_CH4_mean+MBRflux_align$dConc_CH4_sd*2)>0,1,0)

# FC with H2O as tracer
data <- MBRflux_align[c("FC_turb_interp_CO2","FCO2_MBR_H2Otrace_mean")]
dataComp <- data[complete.cases(data),]
RFCO2 <- cor.test(data$FC_turb_interp_CO2,data$FCO2_MBR_H2Otrace_mean)
print(paste0('RFCO2 R-squared = ',round(RFCO2$estimate^2,2)*100,' %'))

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


#data <- dplyr::filter(MBRflux_align, dLevelsAminusB_CH4=="4_1")[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
data <- MBRflux_align[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
dataComp <- data[complete.cases(data),]
RFCH4 <- cor.test(data$FCH4_turb_interp_CH4,data$FCH4_MBR_CO2trace_mean)
print(paste0('FCH4 R-squared = ',round(RFCH4$estimate^2,2)*100,' %'))

# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FCH4_turb_interp_CH4, y=FCH4_MBR_CO2trace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-100,100)) +
#   xlim(c(-100,100)) +
#   labs(title=paste0(site, ' MBR method (levels 4-3); R-squared = ',round(RFCH4$estimate^2,2)*100,'%')) +
#   theme_minimal()

# -------- Save and zip the file to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp,paste0(site,'_MBRflux_bootstrap.RData'))
fileZip <- fs::path(dirTmp,paste0(site,'_MBRflux_bootstrap.zip'))
save(MBRflux_align,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,'_MBRflux_bootstrap.RData'))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, 
                          path = data_folder$id[data_folder$name==site]) # path might need work

