# Compute diel averages after filtering the MBR fluxes for tracer concentrations that are 
# close to zero
#rm(list=ls())
# Authors: Cove Sturtevant

# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'KONZ' #'KONZ' BONA CPER GUAN HARV JORN NIWO TOOL
#PairLvl <- '4_3' # Not needed

Stat <- c('mean','median')[2] # Which statistic for the diel bin
Ucrt <- c('sd','var','mad')[3] # Which uncertainty measure for each diel bin
NumSampMin <- 3 # min number of samples to compute statistics for each diel bin
sdQf <- 2 # how many standard deviations form the mean should be used to determine if the tracer flux difference is not significantly different than zero (1 ~ 66% confidence interval, 2 ~ 95% confidence, 3 ~ 99% confidence)

# Choose a period over which to compute the diel averages
TimeBgn <- as.POSIXct('2023-04-01',tz='GMT')
TimeEnd <- as.POSIXct('2023-10-01',tz='GMT')

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

fileDnld <- paste0(site,'_MBRflux_bootstrap.zip')

message(paste0('Downloading MBR bootstrapped data for ',site))
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
fileIn <- fs::path(dirTmp,paste0(site,'_MBRflux_bootstrap.RData'))
load(fileIn)

# CO2 flux computed with H2O tracer
time <- MBRflux_align$match_time
flux_pred_CO2 <- MBRflux_align$FCO2_MBR_H2Otrace_mean
flux_meas_CO2 <- MBRflux_align$FC_turb_interp_H2O
dConc_mean_CO2 <- MBRflux_align$dConc_CO2_mean
dConc_tracer_mean_CO2 <- MBRflux_align$dConc_H2O_mean
dConc_tracer_sd_CO2 <- MBRflux_align$dConc_H2O_sd
tower_pair <- MBRflux_align$dLevelsAminusB_CO2

# Filter for tracer concentration is not significantly different from zero
qfConc_CO2 <- rep(0,length(time))
qfConc_CO2[((dConc_tracer_mean_CO2-dConc_tracer_sd_CO2*sdQf) < 0 & 
  (dConc_tracer_mean_CO2+dConc_tracer_sd_CO2*sdQf) > 0) ] <- 1
  #| (sign(dConc_mean_CO2) != sign(flux_meas_CO2))
    
flux_meas_CO2[qfConc_CO2==1 | is.na(flux_pred_CO2)] <- NA
flux_pred_CO2[qfConc_CO2==1| is.na(flux_meas_CO2)] <- NA
flux_resid_CO2 <- flux_pred_CO2-flux_meas_CO2

# Select time range & tower level pair
setUse <- time >= TimeBgn & time <= TimeEnd # & tower_pair == PairLvl
numUse <- length(setUse)


# Plot the 1:1
library(plotly)
fig <- plot_ly(x = flux_meas_CO2, y = flux_pred_CO2, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FCO2',
         xaxis = list(title = 'measured'), 
         yaxis = list(title = 'predicted'))
print(fig)

# Compute Diel Patterns
flux_meas_diel_CO2 <- def.diel.ptrn(time=time[setUse],data=flux_meas_CO2[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (EC measured flux)'))
flux_pred_diel_CO2 <- def.diel.ptrn(time=time[setUse],data=flux_pred_CO2[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (MBR predicted flux)'))
if (Stat == 'mean'){
  flux_resid_diel_CO2 <- def.diel.ptrn(time=time[setUse],data=flux_resid_CO2[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (MBR residual)'))
} else {
  flux_resid_diel_CO2 <- flux_pred_diel_CO2-flux_meas_diel_CO2
  flux_resid_diel_CO2 <- def.diel.ptrn(time=as.POSIXct('2010-01-01',tz='GMT')+flux_meas_diel_CO2$time,data=flux_resid_diel_CO2$stat,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=1,TitlPlot=paste0(site,' FCO2 Diel pattern residual (predicted-measured)'))
  
}



# H2O flux computed with CO2 tracer
time <- MBRflux_align$match_time
flux_pred_H2O <- MBRflux_align$FH2O_MBR_CO2trace_mean
flux_meas_H2O <- MBRflux_align$FH2O_interp_CO2
dConc_mean_H2O <- MBRflux_align$dConc_H2O_mean
dConc_tracer_mean_H2O <- MBRflux_align$dConc_CO2_mean
dConc_tracer_sd_H2O <- MBRflux_align$dConc_CO2_sd
tower_pair <- MBRflux_align$dLevelsAminusB_H2O

# Plot the 1:1
library(plotly)
fig <- plot_ly(x = flux_meas_H2O, y = flux_pred_H2O, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FH2O',
         xaxis = list(title = 'measured'), 
         yaxis = list(title = 'predicted'))
print(fig)

# Filter for tracer concentration is not significantly different from zero
qfConc_H2O <- rep(0,length(time))
qfConc_H2O[((dConc_tracer_mean_H2O-dConc_tracer_sd_H2O*sdQf) < 0 & 
             (dConc_tracer_mean_H2O+dConc_tracer_sd_H2O*sdQf) > 0)] <- 1
flux_meas_H2O[qfConc_H2O==1 | is.na(flux_pred_H2O)] <- NA
flux_pred_H2O[qfConc_H2O==1| is.na(flux_meas_H2O)] <- NA
flux_resid_H2O <- flux_pred_H2O-flux_meas_H2O

# Compute Diel Patterns
flux_meas_diel_H2O <- def.diel.ptrn(time=time[setUse],data=flux_meas_H2O[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (EC measured flux)'))
flux_pred_diel_H2O <- def.diel.ptrn(time=time[setUse],data=flux_pred_H2O[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (MBR predicted flux)'))
if (Stat == 'mean'){
  flux_resid_diel_H2O <- def.diel.ptrn(time=time[setUse],data=flux_resid_H2O[setUse],Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (MBR residual)'))
} else {
  flux_resid_diel_H2O <- flux_pred_diel_H2O-flux_meas_diel_H2O
  flux_resid_diel_H2O <- def.diel.ptrn(time=as.POSIXct('2010-01-01',tz='GMT')+flux_meas_diel_H2O$time,data=flux_resid_diel_H2O$stat,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=1,TitlPlot=paste0(site,' FH2O Diel pattern residual (predicted-measured)'))
  
}
