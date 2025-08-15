# Compute diel averages after filtering the MBR fluxes for tracer concentrations that are 
# close to zero
rm(list=ls())
# Authors: Cove Sturtevant

# Pull data from google drive
email <- 'csturtevant@battelleecology.org'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'KONZ' #'KONZ' BONA CPER GUAN HARV JORN NIWO TOOL
# PairLvl <- NULL # Use all tower pairs in the evaluation. Warning - may include same-same tower pairs.
PairLvl <- c('8_7','8_6','8_5','8_4','8_3','8_2','8_1','7_6','7_5','7_4','7_3','7_2','7_1','6_5','6_4','6_3','6_2','6_1','5_4','5_3','5_2','5_1','4_3','4_2','4_1','3_2','3_1','2_1')
# PairLvl <- c('6_5','6_4','6_3','5_4','5_3','4_3')
# PairLvl <- c('5_4','5_3','5_2','4_3','4_2','3_2')
# PairLvl <- c('4_3','5_3','5_4','6_3','6_4','6_5')

Stat <- c('mean','median')[2] # Which statistic for the diel bin
Ucrt <- c('sd','var','mad','se')[4] # Which uncertainty measure for each diel bin
NumSampMin <- 3 # min number of samples to compute statistics for each diel bin
SNRMin <- seq(0,5,0.2) # how many standard deviations from the mean should be used to determine if the concentration difference is not substantially different than zero. 
ustarMin <- 0.247
range_FC <- c(-100, 100) # umol m-2 s-1
range_FH2O <- c(-50, 50) # 
range_FCH4 <- c(-0.5,2) # umol m-2 s-1
crossGradFilt <- FALSE # FALSE IS ADVISABLE. TRUE will filter values for which the tracer flux and concentration gradient are inconsistent. Setting to TRUE will cherry pick data for tower level pairs for which most of the data are filtered and obviously not suitable. 

# Choose a period over which to compute the diel averages
TimeBgn <- as.POSIXct('2021-01-01',tz='GMT') # Overall start time
TimeEnd <- as.POSIXct('2025-12-01',tz='GMT') # Overall end time
Mnths <- c(4:10) # Seasonal months to include
cccMin <- 0.5 # minimum ccc value to have the tower pair included in the diel computation


# ------ Prerequisites! Make sure these packages are installed ----
# Requires packages: fs, googledrive
library(plotly)
source('./functions/calculate.diel.ptrn.R')
source('./functions/calc.MO.length.R')
source("./functions/calc.lins.ccc.R")
# -------------------------------------------------------

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

fileDnld <- paste0(site,c('_MBR_9min.zip','_attr.zip'))

message(paste0('Downloading MBR data for ',site))
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
fileIn <- fs::path(dirTmp,paste0(site,'_MBR_9min.RData'))
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
MOlength <- calc.MO.length(press,temp,H,LE,velofric)$L
MBRflux_align$zoL <- as.numeric(attr.df$DistZaxsTow[1])/MOlength

# Select time range & tower level pair
time <- MBRflux_align$match_time
tower_pair <- MBRflux_align$dLevelsAminusB_CO2
tower_pair[is.na(tower_pair)] <- MBRflux_align$dLevelsAminusB_H2O[is.na(tower_pair)]
tower_pair[is.na(tower_pair)] <- MBRflux_align$dLevelsAminusB_CH4[is.na(tower_pair)]

if (!is.null(PairLvl)){
  setUse <- time >= TimeBgn & time <= TimeEnd & (tower_pair %in% PairLvl) & (lubridate::month(time) %in% Mnths)
} else {
  setUse <- time >= TimeBgn & time <= TimeEnd & (lubridate::month(time) %in% Mnths)
}
numUse <- length(setUse)
time <- time[setUse]
MBRflux_align <- MBRflux_align[setUse,]
tower_pair <- tower_pair[setUse]
tower_pairs <- unique(tower_pair)

RMSE_CO2 <- rep(NA,length(SNRMin))
RMSE_norm_CO2 <- RMSE_CO2
nRMSE_CO2 <- RMSE_CO2
RMSE_H2O <- rep(NA,length(SNRMin))
RMSE_norm_H2O <- RMSE_H2O
nRMSE_H2O <- RMSE_H2O

for(idxSNR in seq_len(length(SNRMin))){

  SNRIdx <- SNRMin[idxSNR]
  
  # CO2 flux computed with H2O tracer
  flux_pred <- MBRflux_align$FCO2_MBR_H2Otrace_mean
  flux_meas <- MBRflux_align$FC_turb_interp_CO2
  flux_tracer <- MBRflux_align$FH2O_turb_interp_CO2
  dConc <- MBRflux_align$dConc_CO2
  dConc_mean <- MBRflux_align$dConc_CO2_mean
  dConc_sd <- MBRflux_align$dConc_CO2_sd
  dConc_pvalue <- MBRflux_align$dConc_pvalue_CO2
  dConc_tracer <- MBRflux_align$dConc_H2O
  dConc_tracer_mean <- MBRflux_align$dConc_H2O_mean
  dConc_tracer_sd <- MBRflux_align$dConc_H2O_sd
  dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_H2O
  ustar <-  MBRflux_align$ustar_interp_CO2
  
  # Plot the 1:1
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 (unfiltered)',
           xaxis = list(title = 'Measured FCO2 (umol m-2 s-1)',range=c(-100,100)), 
           yaxis = list(title = 'Predicted FCO2 (umol m-2 s-1)',range=c(-100,100)),
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
  if (FALSE){print(fig)}
  
  # Filter for tracer concentration difference not greater than SNRMin standard deviations
  qfMeas <- rep(0,length(time))
  qfPred <- rep(0,length(time))
  qf <- rep(0,length(time))
  
  qfPred[((dConc_tracer-dConc_tracer_sd*SNRIdx) < 0 &
          (dConc_tracer+dConc_tracer_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for target concentration difference not greater than SNRMin standard deviations
  qfPred[((dConc-dConc_sd*SNRIdx) < 0 & 
          (dConc+dConc_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for enough turbulence
  qfMeas[is.na(ustar) | (ustar < ustarMin)] <- 1
  qfPred[is.na(ustar) | (ustar < ustarMin)] <- 1
  
  # Filter for acceptable range
  qfMeas[(flux_meas < range_FC[1]) | (flux_meas > range_FC[2])] <- 1
  qfPred[(flux_pred < range_FC[1]) | (flux_pred > range_FC[2])] <- 1
  
  # Filter for tracer cross-gradient
  if(crossGradFilt == TRUE){
    qfPred[dConc_tracer/flux_tracer > 0] <- 1
  }
  
  # Combine measured and predicted quality flags
  qf[qfMeas == 1] <- 1
  qf[qfPred == 1] <- 1
  
  # Filter data
  flux_meas[qfMeas==1] <- NA
  flux_pred[qfPred==1] <- NA
  flux_resid <- flux_pred-flux_meas
  RMSE_CO2[idxSNR] <- sqrt(mean(flux_resid^2,na.rm=T))
  nRMSE_CO2[idxSNR] <- sum(!is.na(flux_resid))
  RMSE_norm_CO2[idxSNR] <- RMSE_CO2[idxSNR]/sqrt(mean(flux_meas[!is.na(flux_resid)]^2,na.rm=T))
  
  # Compute CCC 
  ccc <- data.frame(tower_pair=tower_pairs,
                    nMeas=as.numeric(NA),
                    nPred=as.numeric(NA),
                    IQRmeas=as.numeric(NA),
                    IQRpred=as.numeric(NA),
                    ccc=as.numeric(NA),
                    r=as.numeric(NA))
  tower_pair_plot <- tower_pair
  for (idxTp in tower_pairs){
    setTp <- tower_pair == idxTp
    
    # Compute ccc
    cccIdx <- calculate_lins_ccc(flux_pred[setTp], flux_meas[setTp])$rho.c$est
    
    ccc[ccc$tower_pair==idxTp,"ccc"] <- cccIdx
    
    # Compute r (correlation coef)
    try({
      rIdx <- cor(flux_pred[setTp], flux_meas[setTp],use='complete.obs')
      ccc[ccc$tower_pair==idxTp,"r"] <- rIdx
    },silent=TRUE)
    
    # Compute sample sizes
    nMeas <- sum(!is.na(flux_meas[setTp]))
    nPred <- sum(!is.na(flux_resid[setTp])) # Sample size of available comparison
    ccc[ccc$tower_pair==idxTp,"nMeas"] <- nMeas
    ccc[ccc$tower_pair==idxTp,"nPred"] <- nPred
    
    # Compute IQR
    iqrMeas <- stats::IQR(flux_meas[setTp],na.rm=TRUE)
    iqrPred <- stats::IQR(flux_pred[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"IQRmeas"] <- iqrMeas
    ccc[ccc$tower_pair==idxTp,"IQRpred"] <- iqrPred
    
    # Annotate for plotting
    tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp) # Add in ccc and sample size
    # tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp, ": CCC=",round(cccIdx,2),"; n=",nPred) # Add in ccc and sample size
    
  }
  ccc_FC <- ccc # Save
  
  # Plot the 1:1
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair_plot, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 (filtered)',
           xaxis = list(title = 'Measured FCO2 (umol m-2 s-1)',range=c(-100,100)), 
           yaxis = list(title = 'Predicted FCO2 (umol m-2 s-1)',range=c(-100,100)),
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
  if (FALSE){print(fig)}

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
  if (FALSE){print(fig)}
  fig <- plot_ly(x=dConc_tracer/dConc_tracer_sd, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FCO2 residual vs normalized tracer concentration',
           xaxis = list(title = 'dConc_tracer/dConc_tracer_sd'), 
           yaxis = list(title = 'FCO2 residual')
    )
  if (FALSE){print(fig)}
  
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
  PlotCO2 <- FALSE
  flux_meas[qf==1 | is.na(flux_pred)] <- NA
  flux_pred[qf==1 | is.na(flux_meas)] <- NA
  
  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  setDiel <- tower_pair %in% tower_pair_diel
  flux_diel <- calculate.diel.ptrn(time=time[setDiel],data=data.frame(flux_meas=flux_meas[setDiel],flux_pred=flux_pred[setDiel]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=PlotCO2,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCO2 Diel Pattern'))

  

  # H2O flux computed with CO2 tracer
  flux_pred <- MBRflux_align$FH2O_MBR_CO2trace_mean
  flux_meas <- MBRflux_align$FH2O_interp_H2O
  flux_tracer <- MBRflux_align$FC_turb_interp_H2O
  dConc <- MBRflux_align$dConc_H2O
  dConc_mean <- MBRflux_align$dConc_H2O_mean
  dConc_sd <- MBRflux_align$dConc_H2O_sd
  dConc_pvalue <- MBRflux_align$dConc_pvalue_H2O
  dConc_tracer <- MBRflux_align$dConc_CO2
  dConc_tracer_mean <- MBRflux_align$dConc_CO2_mean
  dConc_tracer_sd <- MBRflux_align$dConc_CO2_sd
  dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_CO2
  ustar <-  MBRflux_align$ustar_interp_H2O
  
  # Plot the 1:1
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O (unfiltered)',
           xaxis = list(title = 'Measured FH2O (mmol m-2 s-1)',range=c(-5,15)), 
           yaxis = list(title = 'Predicted FH2O (mmol m-2 s-1)',range=c(-5,15)),
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
  if (FALSE){print(fig)}
  
  # Filter for tracer concentration difference not greater than SNRMin standard deviations
  qfMeas <- rep(0,length(time))
  qfPred <- rep(0,length(time))
  qf <- rep(0,length(time))
  
  qfPred[((dConc_tracer-dConc_tracer_sd*SNRIdx) < 0 &
            (dConc_tracer+dConc_tracer_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for target concentration difference not greater than SNRMin standard deviations
  qfPred[((dConc-dConc_sd*SNRIdx) < 0 & 
            (dConc+dConc_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for enough turbulence
  qfMeas[is.na(ustar) | (ustar < ustarMin)] <- 1
  qfPred[is.na(ustar) | (ustar < ustarMin)] <- 1
  
  # Filter for acceptable range
  qfMeas[(flux_meas < range_FH2O[1]) | (flux_meas > range_FH2O[2])] <- 1
  qfPred[(flux_pred < range_FH2O[1]) | (flux_pred > range_FH2O[2])] <- 1
  
  # Filter for tracer cross-gradient
  if(crossGradFilt == TRUE){
    qfPred[dConc_tracer/flux_tracer > 0] <- 1
  }
  
  # Combine measured and predicted quality flags
  qf[qfMeas == 1] <- 1
  qf[qfPred == 1] <- 1
  
  # Filter data
  flux_meas[qfMeas==1] <- NA
  flux_pred[qfPred==1] <- NA
  flux_resid <- flux_pred-flux_meas
  RMSE_H2O[idxSNR] <- sqrt(mean(flux_resid^2,na.rm=T))
  nRMSE_H2O[idxSNR] <- sum(!is.na(flux_resid))
  RMSE_norm_H2O[idxSNR] <- RMSE_H2O[idxSNR]/sqrt(mean(flux_meas[!is.na(flux_resid)]^2,na.rm=T))
  
  # Compute CCC 
  ccc <- data.frame(tower_pair=tower_pairs,
                    nMeas=as.numeric(NA),
                    nPred=as.numeric(NA),
                    IQRmeas=as.numeric(NA),
                    IQRpred=as.numeric(NA),
                    ccc=as.numeric(NA),
                    r=as.numeric(NA))
  tower_pair_plot <- tower_pair
  for (idxTp in tower_pairs){
    setTp <- tower_pair == idxTp
    
    # Compute ccc
    cccIdx <- calculate_lins_ccc(flux_pred[setTp], flux_meas[setTp])$rho.c$est
    
    ccc[ccc$tower_pair==idxTp,"ccc"] <- cccIdx
    
    # Compute r (correlation coef)
    try({
      rIdx <- cor(flux_pred[setTp], flux_meas[setTp],use='complete.obs')
      ccc[ccc$tower_pair==idxTp,"r"] <- rIdx
    },silent=TRUE)
    
    # Compute sample sizes
    nMeas <- sum(!is.na(flux_meas[setTp]))
    nPred <- sum(!is.na(flux_resid[setTp])) # Sample size of available comparison
    ccc[ccc$tower_pair==idxTp,"nMeas"] <- nMeas
    ccc[ccc$tower_pair==idxTp,"nPred"] <- nPred
    
    # Compute IQR
    iqrMeas <- stats::IQR(flux_meas[setTp],na.rm=TRUE)
    iqrPred <- stats::IQR(flux_pred[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"IQRmeas"] <- iqrMeas
    ccc[ccc$tower_pair==idxTp,"IQRpred"] <- iqrPred
    
    # Annotate for plotting
    tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp) # Add in ccc and sample size
    # tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp, ": CCC=",round(cccIdx,2),"; n=",nPred) # Add in ccc and sample size
    
  }
  ccc_FH2O <- ccc # Save

  # Plot filtered 1:1  
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair_plot, type = 'scatter', mode = 'markers') %>%
    layout(title = 'FH2O (filtered)',
           xaxis = list(title = 'Measured FH2O (mmol m-2 s-1)',range=c(-5,15)), 
           yaxis = list(title = 'Predicted FH2O (mmol m-2 s-1)',range=c(-5,15)),
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
  if (FALSE){print(fig)}
  
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
  # fig <- plot_ly(x=MBRflux_align$zoL, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  #   layout(title = 'FH2O residual vs stability',
  #          xaxis = list(title = 'z/L'), 
  #          yaxis = list(title = 'FH2O residual')
  #   )
  # print(fig)
  # fig <- plot_ly(x=dConc_tracer/dConc_tracer_sd, y=flux_resid, color=tower_pair, type = 'scatter', mode = 'markers') %>%
  #   layout(title = 'FH2O residual vs normalized tracer concentration',
  #          xaxis = list(title = 'dConc_tracer/dConc_tracer_sd'), 
  #          yaxis = list(title = 'FH2O residual')
  #   )
  # print(fig)
  
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
  PlotH2O <- FALSE
  flux_meas[qf==1 | is.na(flux_pred)] <- NA
  flux_pred[qf==1 | is.na(flux_meas)] <- NA

  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  setDiel <- tower_pair %in% tower_pair_diel
  flux_diel <- calculate.diel.ptrn(time=time[setDiel],data=data.frame(flux_meas=flux_meas[setDiel],flux_pred=flux_pred[setDiel]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=PlotH2O,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern'))
  
  # if (Stat == 'mean'){
  #   flux_resid_diel <- calculate.diel.ptrn(time=time,data=flux_resid,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=PlotH2O,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FH2O Diel Pattern (MBR residual)'))
  # } else {
  #   flux_resid_diel <- flux_pred_diel-flux_meas_diel
  #   flux_resid_diel <- calculate.diel.ptrn(time=as.POSIXct('2010-01-01',tz='GMT')+flux_meas_diel$time,data=flux_resid_diel$stat,Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=PlotH2O,NumSampMin=1,TitlPlot=paste0(site,' FH2O Diel pattern residual (predicted-measured)'))
  #   
  # }
  
  
  
  # CH4 flux computed with H2O tracer
  plotCH4 <- FALSE # Produce plots?
  
  flux_pred <- MBRflux_align$FCH4_MBR_H2Otrace_mean
  flux_tracer <- MBRflux_align$FH2O_turb_interp_CH4
  dConc <- MBRflux_align$dConc_CH4
  dConc_mean <- MBRflux_align$dConc_CH4_mean
  dConc_sd <- MBRflux_align$dConc_CH4_sd
  dConc_pvalue <- MBRflux_align$dConc_pvalue_CH4
  dConc_tracer <- MBRflux_align$dConc_H2O
  dConc_tracer_mean <- MBRflux_align$dConc_H2O_mean
  dConc_tracer_sd <- MBRflux_align$dConc_H2O_sd
  dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_H2O
  ustar <-  MBRflux_align$ustar_interp_CH4
  
  
  # Filter for tracer concentration difference not greater than SNRMin standard deviations
  qf <- rep(0,length(time))
  qf[((dConc_tracer-dConc_tracer_sd*SNRIdx) < 0 &
        (dConc_tracer+dConc_tracer_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for target concentration difference not greater than SNRMin standard deviations
  qf[((dConc-dConc_sd*SNRIdx) < 0 & 
        (dConc+dConc_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for acceptable range
  qf[(flux_pred < range_FCH4[1]) | (flux_pred > range_FCH4[2])] <- 1
  
  # Filter for enough turbulence
  qf[is.na(ustarMin) | (ustar < ustarMin)] <- 1
  
  # Filter for tracer cross-gradient
  if(crossGradFilt == TRUE){
    qf[dConc_tracer/flux_tracer > 0] <- 1
  }
  
  flux_pred[qf==1] <- NA
  flux_pred_FH2Otrace <- flux_pred
  
  # Comput for Diel Patterns
  ccc <- ccc_FC # Use the tower pairs that were good when this tracer was used (e.g. ccc_FC is when H2O was the tracer)
  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  setDielNA <- !(tower_pair %in% tower_pair_diel)
  flux_pred_FH2Otrace[setDielNA] <- NA

  
  
  # CH4 flux computed with CO2 tracer
  flux_pred <- MBRflux_align$FCH4_MBR_CO2trace_mean
  flux_tracer <- MBRflux_align$FC_turb_interp_CH4
  dConc <- MBRflux_align$dConc_CH4
  dConc_mean <- MBRflux_align$dConc_CH4_mean
  dConc_sd <- MBRflux_align$dConc_CH4_sd
  dConc_pvalue <- MBRflux_align$dConc_pvalue_CH4
  dConc_tracer <- MBRflux_align$dConc_CO2
  dConc_tracer_mean <- MBRflux_align$dConc_CO2_mean
  dConc_tracer_sd <- MBRflux_align$dConc_CO2_sd
  dConc_pvalue_tracer <- MBRflux_align$dConc_pvalue_CO2
  ustar <-  MBRflux_align$ustar_interp_H2O
  
  # Filter for tracer concentration difference not greater than SNRMin standard deviations
  qf <- rep(0,length(time))
  # qf[((dConc_tracer-dConc_tracer_sd*SNRIdx) < 0 & 
  #       (dConc_tracer+dConc_tracer_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for target concentration difference not greater than SNRMin standard deviations
  qf[((dConc-dConc_sd*SNRIdx) < 0 & 
        (dConc+dConc_sd*SNRIdx) > 0) ] <- 1
  
  # Filter for enough turbulence
  qf[is.na(ustarMin) | (ustar < ustarMin)] <- 1
  
  # Filter for acceptable range
  qf[(flux_pred < range_FCH4[1]) | (flux_pred > range_FCH4[2])] <- 1
  
  # Filter for tracer cross-gradient
  if(crossGradFilt == TRUE){
    qf[dConc_tracer/flux_tracer > 0] <- 1
  }
  
  flux_pred[qf==1] <- NA
  flux_pred_FCtrace <- flux_pred
  
  # Compute Diel Patterns
  ccc <- ccc_FH2O # Use the tower pairs that were good when this tracer was used (e.g. ccc_FC is when H2O was the tracer)
  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  setDielNA <- !(tower_pair %in% tower_pair_diel)
  flux_pred_FCtrace[setDielNA] <- NA
  flux_pred_diel <- calculate.diel.ptrn(time=time,data=data.frame(flux_pred_FH2Otrace=flux_pred_FH2Otrace,flux_pred_FCtrace=flux_pred_FCtrace),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=plotCH4,NumSampMin=NumSampMin,TitlPlot=paste0(site,' FCH4 Diel Pattern (MBR predicted flux with CO2 tracer)'))

}

# Plot the reduction in RMSE with increasing SNRMin
if(TRUE){
  # statSNR <- data.frame(nSD=SNRMin,RMSE_CO2=RMSE_CO2,RMSE_H2O=RMSE_H2O)
  statSNR <- data.frame(nSD=SNRMin,RMSE_CO2=RMSE_norm_CO2,RMSE_H2O=RMSE_norm_H2O)
  statSNRMelt <- reshape2::melt(statSNR,id.vars='nSD')
  fig <- plot_ly(data=statSNR,x=~SNRMin, y=~RMSE_CO2, type = 'scatter',name="FCO2",
                 mode = 'lines',line = list(color = 'rgb(255,140,0)', width = 2)) %>%
    add_trace(y=~RMSE_H2O, type = 'scatter', name="FH2O",
              mode = 'lines',line = list(color = 'rgb(70,130,180)', width = 2)) %>%
    layout(title = 'Filtering by SNR',
           xaxis = list(title = 'Minimum SNR',
                        showgrid=FALSE), 
           yaxis = list(title = 'Normalized RMSE (MBR vs. EC)',
                        range=c(0,2.3),
                        showgrid=FALSE)
    )
  print(fig)
}
