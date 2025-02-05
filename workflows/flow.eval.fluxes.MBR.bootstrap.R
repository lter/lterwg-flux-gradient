# Compute diel averages after filtering the MBR fluxes for tracer concentrations that are 
# close to zero
rm(list=ls())
# Authors: Cove Sturtevant

# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'KONZ' #'KONZ' BONA CPER GUAN HARV JORN NIWO TOOL
PairLvl <- c('6_5','6_4','6_3','6_2','6_1','5_4','5_3','5_2','5_1','4_3','4_2','4_1','3_2','3_1','2_1')
# PairLvl <- c('6_5','6_4','6_3','5_4','5_3','4_3')
# PairLvl <- c('5_4','5_3','5_2','4_3','4_2','3_2')
# PairLvl <- c('4_3','5_3','5_4','6_3','6_4','6_5')

Stat <- c('mean','median')[2] # Which statistic for the diel bin
Ucrt <- c('sd','var','mad','se')[4] # Which uncertainty measure for each diel bin
NumSampMin <- 3 # min number of samples to compute statistics for each diel bin
sdQf <- 0 # how many standard deviations form the mean should be used to determine if the tracer flux difference is not substantially different than zero (1 ~ 66% confidence interval, 2 ~ 95% confidence, 3 ~ 99% confidence)
pQf <- 1 # Maximum p-value of concentration difference (testing statistical difference from zero). This is in addition to the sdQf filter. (A value of 1 includes all values)

# Choose a period over which to compute the diel averages
TimeBgn <- as.POSIXct('2022-04-01',tz='GMT')
TimeEnd <- as.POSIXct('2022-11-01',tz='GMT')

# ------ Prerequisites! Make sure these packages are installed ----
# Requires packages: fs, googledrive
library(plotly)
source('./functions/calculate.diel.ptrn.R')
source('./functions/MO_Length_CRS.R')
# -------------------------------------------------------

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

fileDnld <- paste0(site,c('_MBRflux_bootstrap.zip','_attr.zip'))

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
fileIn <- fs::path(dirTmp,'data',site,paste0(site,'_attr.Rdata'))
load(fileIn)
  

# Add Obukhov length
press <- MBRflux_align$P_kPa_H2O
press[is.na(press)] <- MBRflux_align$P_kPa_CO2[is.na(press)]
temp <- MBRflux_align$Tair_K_H2O
press[is.na(temp)] <- MBRflux_align$Tair_K_CO2[is.na(temp)]
H <- MBRflux_align$H_turb_interp_H2O
H[is.na(H)] <- MBRflux_align$H_turb_interp_CO2[is.na(H)]
LE <- MBRflux_align$LE_turb_interp_H2O
LE[is.na(LE)] <- MBRflux_align$LE_turb_interp_CO2[is.na(LE)]
velofric <- MBRflux_align$ustar_interp_H2O
velofric[is.na(velofric)] <- MBRflux_align$ustar_interp_CO2[is.na(velofric)]
MOlength <- MOlength(press,temp,H,LE,velofric)$L
MBRflux_align$zoL <- as.numeric(attr.df$DistZaxsTow[1])/MOlength

# Select time range & tower level pair
time <- MBRflux_align$match_time
tower_pair <- MBRflux_align$dLevelsAminusB_CO2
tower_pair[is.na(tower_pair)] <- MBRflux_align$dLevelsAminusB_H2O[is.na(tower_pair)]
tower_pair[is.na(tower_pair)] <- MBRflux_align$dLevelsAminusB_CH4[is.na(tower_pair)]

setUse <- time >= TimeBgn & time <= TimeEnd & (tower_pair %in% PairLvl)
numUse <- length(setUse)
time <- time[setUse]
MBRflux_align <- MBRflux_align[setUse,]
tower_pair <- tower_pair[setUse]


# CO2 flux computed with H2O tracer
flux_pred <- MBRflux_align$FCO2_MBR_H2Otrace_mean
flux_meas <- MBRflux_align$FC_turb_interp_H2O
dConc <- MBRflux_align$dConc_CO2
dConc_mean <- MBRflux_align$dConc_CO2_mean
dConc_sd <- MBRflux_align$dConc_CO2_sd
dConc_pvalue <- MBRflux_align$dConc_pvalue_CO2
dConc_tracer <- MBRflux_align$dConc_H2O
dConc_tracer_mean <- MBRflux_align$dConc_H2O_mean
dConc_tracer_sd <- MBRflux_align$dConc_H2O_sd
dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_H2O

# Plot the 1:1
fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FCO2 (unfiltered)',
         xaxis = list(title = 'measured',range=c(-100,100)), 
         yaxis = list(title = 'predicted',range=c(-100,100)),
         shapes = list(list(
           type = "line", 
           x0 = -100, 
           x1 = 100, 
           xref = "x",
           y0 = -100, 
           y1 = 100, 
           yref = "y",
           line = list(color = "black")
         )))
print(fig)

# Filter for tracer concentration difference not greater than sdQf standard deviations
qf <- rep(0,length(time))
qf[((dConc_tracer-dConc_tracer_sd*sdQf) < 0 & 
  (dConc_tracer+dConc_tracer_sd*sdQf) > 0) ] <- 1

# Filter for target concentration difference not greater than sdQf standard deviations
qf[((dConc-dConc_sd*sdQf) < 0 & 
    (dConc+dConc_sd*sdQf) > 0) ] <- 1

# Filter for tracer concentration difference not statistically different from zero
qf[dConc_pvalue_tracer > pQf] <- 1

# Filter for target concentration difference not statistically different from zero
qf[dConc_pvalue > pQf] <- 1

flux_meas[qf==1 | is.na(flux_pred)] <- NA
flux_pred[qf==1 | is.na(flux_meas)] <- NA
flux_resid <- flux_pred-flux_meas

# Plot the 1:1
fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FCO2 (filtered)',
         xaxis = list(title = 'measured',range=c(-100,100)), 
         yaxis = list(title = 'predicted',range=c(-100,100)),
         shapes = list(list(
            type = "line", 
            x0 = -100, 
            x1 = 100, 
            xref = "x",
            y0 = -100, 
            y1 = 100, 
            yref = "y",
            line = list(color = "black")
    )))
print(fig)

# Plot the residuals against some likely culprits
if(FALSE){
    fig <- plot_ly(x=flux_meas, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs flux magnitude',
           xaxis = list(title = 'Measured flux magnitude'), 
           yaxis = list(title = 'FCO2 residual')
    )
  print(fig)
  fig <- plot_ly(x=flux_pred, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs flux magnitude',
           xaxis = list(title = 'Predicted flux magnitude'), 
           yaxis = list(title = 'FCO2 residual')
    )
  print(fig)
}
fig <- plot_ly(x=MBRflux_align$zoL, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FCO2 residual vs stability',
         xaxis = list(title = 'z/L'), 
         yaxis = list(title = 'FCO2 residual')
  )
print(fig)
fig <- plot_ly(x=dConc_tracer/dConc_tracer_sd, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FCO2 residual vs normalized tracer concentration',
         xaxis = list(title = 'dConc_tracer/dConc_tracer_sd'), 
         yaxis = list(title = 'FCO2 residual')
  )
print(fig)

if(FALSE){
  fig <- plot_ly(x=dConc_pvalue, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs stat. significance of target gradient',
           xaxis = list(title = 'p-value of conc. diff for target'), 
           yaxis = list(title = 'FCO2 residual')
    )
  print(fig)
  fig <- plot_ly(x=dConc_pvalue_tracer, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs stat. significance of tracer gradient',
           xaxis = list(title = 'p-value of conc. diff for tracer'), 
           yaxis = list(title = 'FCO2 residual')
    )
}
if(FALSE){
  fig <- plot_ly(x=dConc_tracer, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs H2O (tracer) concentation difference',
           xaxis = list(title = 'dconc_H2O'), 
           yaxis = list(title = 'FCO2 residual')
    )
  print(fig)
  fig <- plot_ly(x=dConc, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs CO2 (target) concentation difference',
           xaxis = list(title = 'dconc_CO2'), 
           yaxis = list(title = 'FCO2 residual')
    )
  print(fig)
}


# Compute Diel Patterns
flux_meas_diel <- calculate.diel.ptrn(time=time,data=flux_meas,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (EC measured flux)'))
flux_pred_diel <- calculate.diel.ptrn(time=time,data=flux_pred,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (MBR predicted flux)'))
if (Stat == 'mean'){
  flux_resid_diel <- calculate.diel.ptrn(time=time,data=flux_resid,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern (MBR residual)'))
} else {
  flux_resid_diel <- flux_pred_diel-flux_meas_diel
  flux_resid_diel <- calculate.diel.ptrn(time=as.POSIXct('2010-01-01',tz='GMT')+flux_meas_diel$time,data=flux_resid_diel$stat,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=1,TitlPlot=paste0(site,' FCO2 Diel pattern residual (predicted-measured)'))
  
}



# H2O flux computed with CO2 tracer
flux_pred <- MBRflux_align$FH2O_MBR_CO2trace_mean
flux_meas <- MBRflux_align$FH2O_interp_CO2
dConc <- MBRflux_align$dConc_H2O
dConc_mean <- MBRflux_align$dConc_H2O_mean
dConc_sd <- MBRflux_align$dConc_H2O_sd
dConc_pvalue <- MBRflux_align$dConc_pvalue_H2O
dConc_tracer <- MBRflux_align$dConc_CO2
dConc_tracer_mean <- MBRflux_align$dConc_CO2_mean
dConc_tracer_sd <- MBRflux_align$dConc_CO2_sd
dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_CO2

# Plot the 1:1
fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FH2O (unfiltered)',
         xaxis = list(title = 'measured',range=c(-5,15)), 
         yaxis = list(title = 'predicted',range=c(-5,15)),
         shapes = list(list(
           type = "line", 
           x0 = -5, 
           x1 = 15, 
           xref = "x",
           y0 = -5, 
           y1 = 15, 
           yref = "y",
           line = list(color = "black")
         )))
print(fig)

# Filter for tracer concentration difference not greater than sdQf standard deviations
qf <- rep(0,length(time))
qf[((dConc_tracer-dConc_tracer_sd*sdQf) < 0 & 
      (dConc_tracer+dConc_tracer_sd*sdQf) > 0) ] <- 1

# Filter for target concentration difference not greater than sdQf standard deviations
qf[((dConc-dConc_sd*sdQf) < 0 & 
      (dConc+dConc_sd*sdQf) > 0) ] <- 1

# Filter for tracer concentration difference not statistically different from zero
qf[dConc_pvalue_tracer > pQf] <- 1

# Filter for target concentration difference not statistically different from zero
qf[dConc_pvalue > pQf] <- 1


flux_meas[qf==1 | is.na(flux_pred)] <- NA
flux_pred[qf==1 | is.na(flux_meas)] <- NA
flux_resid <- flux_pred-flux_meas

fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FH2O (filtered)',
         xaxis = list(title = 'measured',range=c(-5,15)), 
         yaxis = list(title = 'predicted',range=c(-5,15)),
         shapes = list(list(
           type = "line", 
           x0 = -5, 
           x1 = 15, 
           xref = "x",
           y0 = -5, 
           y1 = 15, 
           yref = "y",
           line = list(color = "black")
         )))
print(fig)

# Plot the residuals against some likely culprits
if(FALSE){
  fig <- plot_ly(x=flux_meas, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs flux magnitude',
           xaxis = list(title = 'Measured flux magnitude'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
  fig <- plot_ly(x=flux_pred, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs flux magnitude',
           xaxis = list(title = 'Predicted flux magnitude'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
}
fig <- plot_ly(x=MBRflux_align$zoL, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FH2O residual vs stability',
         xaxis = list(title = 'z/L'), 
         yaxis = list(title = 'FH2O residual')
  )
print(fig)
fig <- plot_ly(x=dConc_tracer/dConc_tracer_sd, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  layout(title = 'FH2O residual vs normalized tracer concentration',
         xaxis = list(title = 'dConc_tracer/dConc_tracer_sd'), 
         yaxis = list(title = 'FH2O residual')
  )
print(fig)

if(FALSE){
  fig <- plot_ly(x=dConc_pvalue, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs stat. significance of target gradient',
           xaxis = list(title = 'p-value of conc. diff for target'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
  fig <- plot_ly(x=dConc_pvalue_tracer, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs stat. significance of tracer gradient',
           xaxis = list(title = 'p-value of conc. diff for tracer'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
}

if(FALSE){
  fig <- plot_ly(x=dConc_tracer, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs CO2 (tracer) concentation difference',
           xaxis = list(title = 'Tracer concentration difference'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
  fig <- plot_ly(x=dConc, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O residual vs H2O (target) concentation difference',
           xaxis = list(title = 'Target concentration difference'), 
           yaxis = list(title = 'FH2O residual')
    )
  print(fig)
}

# Compute Diel Patterns
flux_meas_diel <- calculate.diel.ptrn(time=time,data=flux_meas,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (EC measured flux)'))
flux_pred_diel <- calculate.diel.ptrn(time=time,data=flux_pred,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (MBR predicted flux)'))
if (Stat == 'mean'){
  flux_resid_diel <- calculate.diel.ptrn(time=time,data=flux_resid,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (MBR residual)'))
} else {
  flux_resid_diel <- flux_pred_diel-flux_meas_diel
  flux_resid_diel <- calculate.diel.ptrn(time=as.POSIXct('2010-01-01',tz='GMT')+flux_meas_diel$time,data=flux_resid_diel$stat,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=TRUE,NumSampMin=1,TitlPlot=paste0(site,' FH2O Diel pattern residual (predicted-measured)'))
  
}



# CH4 flux computed with H2O tracer
plotCH4 <- FALSE # Produce plots?

flux_pred <- MBRflux_align$FCH4_MBR_H2Otrace_mean
dConc <- MBRflux_align$dConc_CH4
dConc_mean <- MBRflux_align$dConc_CH4_mean
dConc_sd <- MBRflux_align$dConc_CH4_sd
dConc_pvalue <- MBRflux_align$dConc_pvalue_CH4
dConc_tracer <- MBRflux_align$dConc_H2O
dConc_tracer_mean <- MBRflux_align$dConc_H2O_mean
dConc_tracer_sd <- MBRflux_align$dConc_H2O_sd
dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_H2O


# Filter for tracer concentration difference not greater than sdQf standard deviations
qf <- rep(0,length(time))
qf[((dConc_tracer-dConc_tracer_sd*sdQf) < 0 & 
      (dConc_tracer+dConc_tracer_sd*sdQf) > 0) ] <- 1

# Filter for target concentration difference not greater than sdQf standard deviations
qf[((dConc-dConc_sd*sdQf) < 0 & 
      (dConc+dConc_sd*sdQf) > 0) ] <- 1

# Filter for tracer concentration difference not statistically different from zero
qf[dConc_pvalue_tracer > pQf] <- 1

# Filter for target concentration difference not statistically different from zero
qf[dConc_pvalue > pQf] <- 1


flux_pred[qf==1] <- NA

# Compute Diel Patterns
flux_pred_diel <- calculate.diel.ptrn(time=time,data=flux_pred,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=plotCH4,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCH4 Diel Pattern (MBR predicted flux with H2O tracer)'))



# CH4 flux computed with CO2 tracer
flux_pred <- MBRflux_align$FCH4_MBR_CO2trace_mean
dConc <- MBRflux_align$dConc_CH4
dConc_mean <- MBRflux_align$dConc_CH4_mean
dConc_sd <- MBRflux_align$dConc_CH4_sd
dConc_pvalue <- MBRflux_align$dConc_pvalue_CH4
dConc_tracer <- MBRflux_align$dConc_CO2
dConc_tracer_mean <- MBRflux_align$dConc_CO2_mean
dConc_tracer_sd <- MBRflux_align$dConc_CO2_sd
dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_CO2

# Filter for tracer concentration difference not greater than sdQf standard deviations
qf <- rep(0,length(time))
qf[((dConc_tracer-dConc_tracer_sd*sdQf) < 0 & 
      (dConc_tracer+dConc_tracer_sd*sdQf) > 0) ] <- 1

# Filter for target concentration difference not greater than sdQf standard deviations
qf[((dConc-dConc_sd*sdQf) < 0 & 
      (dConc+dConc_sd*sdQf) > 0) ] <- 1

# Filter for tracer concentration difference not statistically different from zero
qf[dConc_pvalue_tracer > pQf] <- 1

# Filter for target concentration difference not statistically different from zero
qf[dConc_pvalue > pQf] <- 1

flux_pred[qf==1] <- NA

# Compute Diel Patterns
flux_pred_diel <- calculate.diel.ptrn(time=time,data=flux_pred,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=plotCH4,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCH4 Diel Pattern (MBR predicted flux with CO2 tracer)'))
