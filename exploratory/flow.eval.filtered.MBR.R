##############################################################################################
#' Characterize all-site data for the MBR method making it through the filtering 
#' and best-height characterization
#' 
##############################################################################################
rm(list=ls())
library(plotly)
library(dplyr)
source("./functions/calc.lins.ccc.R")
source('./functions/calculate.diel.ptrn.R')

# ----- SETUP -----

# Pull data from google drive
email <- 'csturtevant@battelleecology.org'

localdir <- '/scratch/FluxGradient'
fileTowerInfo <- 'Site_Attributes.csv'

DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir
site.list <- c('ABBY','BARR','BART','BLAN','BONA','CLBJ','CPER','DCFS','DEJU','DELA','DSNY','GRSM','GUAN',
               'HARV','HEAL','JERC','JORN','KONA','KONZ','LAJA','LENO','MLBS','MOAB','NIWO','NOGP','OAES',
               'ONAQ','ORNL','OSBS','PUUM','RMNP','SCBI','SERC','SJER','SOAP','SRER','STEI','STER','TALL',
               'TEAK','TOOL','TREE','UKFS','UNDE','WOOD','WREF','YELL')[45]
PlotOne2One <- TRUE
PlotDiel <- TRUE

Mnths <- c(4:10) # Seasonal months to include

cccMin <- 0.5 # minimum ccc value to have the tower pair included in the diel computation
Stat <- c('mean','median')[2] # Which statistic for the diel bin
Ucrt <- c('sd','var','mad','se')[4] # Which uncertainty measure for each diel bin
NumSampMin <- 3 # min number of samples to compute statistics for each diel bin

save <- FALSE # Save the output to localdir/AllSites_MBR_Eval.Rdata

# ----- Download -----
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)

# Load site attributes file
towerInfo <- read.csv(file=fs::path(localdir,fileTowerInfo))

# Get time zone per site
tz <- towerInfo %>% group_by(Site) %>% summarise(tz=head(ZoneTime,1))
tz$tz[tz$tz=='CST'] <- 'CST6CDT'

# Run through each site
for( site in site.list){
  print(site)

  dnld_files <- paste0(site, c("_FILTER.Rdata",'_9min.report.csv','_MBR_9min.zip'))
  # dnld_files <- paste0(site, c("_FILTER.Rdata"))
  
  localdir.site <- paste(localdir,"/", site, sep = "")
    
  if(DnldFromGoogleDrive == TRUE){
    if(!dir.exists(localdir.site)){
      dir.create(localdir.site,recursive=TRUE)
    }
    
    for (focal_file in dnld_files){
      
      message('Downloading ',focal_file, ' to ',localdir)
      
      site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
      file_id <- subset(site_folder, name == focal_file)
      pathDnld <- fs::path(localdir.site,focal_file)
      
      googledrive::drive_download(file = file_id$id, 
                                  path = pathDnld,
                                  overwrite = T)
      
    }
  }
    
  # Load the file(s)
  for (focal_file in dnld_files){
    message('Loading ',focal_file, ' from ',localdir.site)
    
    # Unzip if necessary
    if(fs::path_ext(focal_file) == 'zip'){
      zip.file <- fs::path(localdir.site,focal_file)
      unzip(zip.file, exdir=localdir.site)
      focal_file <- paste0(fs::path_ext_remove(focal_file),'.RData')
    }
    
    if(fs::path_ext(focal_file) %in% c("RDATA","Rdata","RData")){
      load(fs::path(localdir.site,focal_file))
      try(rm('AE_9min_FILTER','WP_9min_FILTER'),silent=TRUE)
    } else if (fs::path_ext(focal_file) %in% c("csv")){
      dataCsv <- read.csv(fs::path(localdir.site,focal_file))
    }
    gc()
  }
    
  # Adjust time zone to local standard time
  time_local <- lubridate::with_tz(MBR_9min_FILTER$match_time.x,tz$tz[tz$Site==site])
  
  # CO2 flux computed with H2O tracer
  gas="CO2"
  tracer="H2O"
  time <- time_local
  setUse <- MBR_9min_FILTER$gas==gas & MBR_9min_FILTER$tracer==tracer & (lubridate::month(time) %in% Mnths)
  time <- time_local[setUse]
  flux_pred <- MBR_9min_FILTER$FG_mean[setUse]
  flux_meas <- MBR_9min_FILTER$FC_turb_interp[setUse]
  flux_tracer <- MBR_9min_FILTER$FH2O_interp[setUse]
  zoL <-  MBR_9min_FILTER$zoL[setUse] 
  ustar_threshold <- MBR_9min_FILTER$ustar_threshold[setUse]
  tower_pair <- MBR_9min_FILTER$dlvl[setUse]
  tower_pairs <- unique(tower_pair)
  tower_pair_raw <- MBRflux_align$dLevelsAminusB_CO2
  

  # Pre-filtered data
  time_raw <- lubridate::with_tz(MBRflux_align$match_time_CO2,tz$tz[tz$Site==site])
  setUse_raw <- (lubridate::month(time_raw) %in% Mnths)
  time_raw <- time_raw[setUse_raw]
  flux_pred_raw <- MBRflux_align$FCO2_MBR_H2Otrace_mean[setUse_raw]
  flux_meas_raw <- MBRflux_align$FC_turb_interp_CO2[setUse_raw]
  flux_meas_raw[MBRflux_align$ustar_interp_CO2[setUse_raw] < median(MBRflux_align$ustar_interp_CO2[setUse_raw],na.rm=TRUE)] <- NA # Filter for ustar
  tower_pair_raw <- MBRflux_align$dLevelsAminusB_H2O[setUse_raw]
  
  # Compute CCC 
  ccc <- data.frame(site=site,
                    gas=gas,
                    tracer=tracer,
                    tower_pair=tower_pairs,
                    n=as.numeric(NA),
                    nraw=as.numeric(NA),
                    ccc=as.numeric(NA),
                    r=as.numeric(NA),
                    IQRpred=as.numeric(NA),
                    zoL_median=as.numeric(NA),
                    zoL_25=as.numeric(NA),
                    zoL_75=as.numeric(NA),
                    ustar_threshold=as.numeric(NA))
  tower_pair_plot <- tower_pair
  for (idxTp in tower_pairs){
    setTp <- tower_pair == idxTp
    setTp_raw <- tower_pair_raw == idxTp
    
    # Compute ccc
    cccIdx <- calculate_lins_ccc(flux_pred[setTp], flux_meas[setTp])$rho.c$est
    
    ccc[ccc$tower_pair==idxTp,"ccc"] <- cccIdx
    
    # Compute r (correlation coef)
    try({
      rIdx <- cor(flux_pred[setTp], flux_meas[setTp],use='complete.obs')
      ccc[ccc$tower_pair==idxTp,"r"] <- rIdx
    },silent=TRUE)
    
    # Compute sample sizes
    n <- sum(!is.na(flux_pred[setTp]+flux_meas[setTp])) # Sample size of available comparison
    ccc[ccc$tower_pair==idxTp,"n"] <- n

    nraw <- sum(!is.na(flux_pred_raw[setTp_raw]+flux_meas_raw[setTp_raw])) # Sample size of unfiltered data
    ccc[ccc$tower_pair==idxTp,"nraw"] <- nraw
    
    # Compute IQR
    iqrPred <- stats::IQR(flux_pred[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"IQRpred"] <- iqrPred
    
    # Compute ancillary statistics
    zoL_median <- median(zoL[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_median"] <- zoL_median
    zoL_25 <- quantile(zoL[setTp],0.25,na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_25"] <- zoL_25
    zoL_75 <- quantile(zoL[setTp],0.75,na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_75"] <- zoL_75
    
    ustar_threshold_median <- median(ustar_threshold[setTp],na.rm=TRUE) # It's all a single values
    ccc[ccc$tower_pair==idxTp,"ustar_threshold"] <- ustar_threshold_median
    
    # Annotate for plotting
    tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp, ": CCC=",round(cccIdx,2),"; n=",n)
    
  }
  if(exists('cccAll')){
    cccAll <- rbind(cccAll,ccc) # Save
  } else {
    cccAll <- ccc
  }
  
  # Plot the 1:1
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair_plot, type = 'scatter', mode = 'markers') %>%
    layout(title = paste0(site,' F',gas,' (filtered)'),
           xaxis = list(title = 'measured',showgrid=F), 
           yaxis = list(title = 'predicted',showgrid=F),
           shapes = list(list(
             type = "line", 
             x0 = -50, 
             x1 = 25, 
             xref = "x",
             y0 = -50, 
             y1 = 25, 
             yref = "y",
             line = list(color = "black")
           )))
  if (PlotOne2One){print(fig)}
  
  # Compute Diel Patterns
  flux_meas[is.na(flux_pred)] <- NA
  flux_pred[is.na(flux_meas)] <- NA
  flux_meas_raw[is.na(flux_pred_raw)] <- NA
  flux_pred_raw[is.na(flux_meas_raw)] <- NA
  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  if(length(tower_pair_diel) > 0){
    setDiel <- tower_pair %in% tower_pair_diel
    flux_diel <- calculate.diel.ptrn(time=time[setDiel],data=data.frame(flux_meas=flux_meas[setDiel],flux_pred=flux_pred[setDiel]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=FALSE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' F',gas,' Diel Pattern'))
    setDiel_raw <- tower_pair_raw %in% tower_pair_diel
    flux_diel_raw <- calculate.diel.ptrn(time=time_raw[setDiel_raw],data=data.frame(flux_meas=flux_meas_raw[setDiel_raw],flux_pred=flux_pred_raw[setDiel_raw]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=FALSE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' F',gas,' Unfiltered Diel Pattern'))
    
    # Save
    flux_diel_site <- data.frame(time=c(flux_diel$flux_meas$time,
                                   flux_diel$flux_pred$time,
                                   flux_diel_raw$flux_meas$time,
                                   flux_diel_raw$flux_pred$time),
                            flux=c(flux_diel$flux_meas$stat,
                                   flux_diel$flux_pred$stat,
                                   flux_diel_raw$flux_meas$stat,
                                   flux_diel_raw$flux_pred$stat),
                            ucrt=c(flux_diel$flux_meas$ucrt,
                                   flux_diel$flux_pred$ucrt,
                                   flux_diel_raw$flux_meas$ucrt,
                                   flux_diel_raw$flux_pred$ucrt),
                            n=c(flux_diel$flux_meas$n,
                                flux_diel$flux_pred$n,
                                flux_diel_raw$flux_meas$n,
                                flux_diel_raw$flux_pred$n),
                            site=site,
                            gas=gas,
                            tracer=tracer,
                            type=c(rep('measured',48),rep('predicted',48),rep('measured',48),rep('predicted',48)),
                            filtering=c(rep('filtered',96),rep('ustar only',96)),
                            typeComb=c(c(rep('filtered EC',48),rep('filtered MBR',48),rep('unfiltered EC',48),rep('unfiltered MBR',48)))
    )

    # Plot all 4 diel patterns (unfiltered measured, unfiltered, predicted, filtered measured, filtered predicted )
    if(PlotDiel){
      fig <- plot_ly(data = flux_diel_site, x = ~time, y = ~flux, color = ~typeComb, type = 'scatter', mode = 'markers+lines',
                     error_y = ~list(array = flux_diel_site$ucrt,
                                     color = '#999999'))
      fig <- fig %>%
        layout(title = paste0(site,' F',gas,' Diel Pattern'),
               xaxis = list(title = 'Time (hours)',
                            range=c(0,24),
                            showgrid=FALSE), 
               yaxis = list(title = Stat,
                            showgrid=FALSE))
      print(fig)      
      
      # Plot just filtered/unfiltered EC and filtered MBR for the paper
      if(TRUE){
        setA <- flux_diel_site$gas==gas & flux_diel_site$typeComb == "filtered EC"
        setB <- flux_diel_site$gas==gas & flux_diel_site$typeComb == "filtered MBR"
        setC <- flux_diel_site$gas==gas & flux_diel_site$typeComb == "unfiltered EC"
        fig <- plot_ly(data=flux_diel_site[setA,], x = ~time, y = ~flux, type = 'scatter', name="filtered EC",
                       mode = 'lines',line = list(color = 'rgb(110,110,110)', width = 2), #rgb(81,188,34)
                       error_y = ~list(array = ucrt,color = 'rgb(200,200,200)')) %>% #rgb(145,230,60)
          add_trace(data=flux_diel_site[setB,], x = ~time, y = ~flux, type = 'scatter', name="filtered MBR",
                    mode = 'lines',line = list(color = 'rgb(255,140,0)', width = 2), #rgb(176,18,158)
                    error_y = ~list(array = ucrt,color = 'rgb(255,200,100)')) %>% #rgb(255,110,235)
          add_trace(data=flux_diel_site[setC,], x = ~time, y = ~flux, type = 'scatter', name="all EC",
                    mode = 'lines',line = list(color = 'rgb(25,25,25)', width = 2),
                    error_y = ~list(array = ucrt,color = 'rgb(110,110,110)'))
        fig <- fig %>%
          layout(title = paste0(site, ' FCO2 Diel Pattern'),
                 xaxis = list(title = 'Time (hours)',
                              range=c(0,24),
                              showgrid=FALSE), 
                 yaxis = list(title = "FCO2 (umol m-2 s-1)",
                              showgrid=FALSE))
        
        print(fig)
        
      }
    }
    
    
    if(exists('flux_diel_all')){
      flux_diel_all <- rbind(flux_diel_all,flux_diel_site)
    } else {
      flux_diel_all <- flux_diel_site 
    }
    
  }
  
  # H2O flux computed with CO2 tracer
  gas="H2O"
  tracer="CO2"
  time <- time_local
  setUse <- MBR_9min_FILTER$gas==gas & MBR_9min_FILTER$tracer==tracer & (lubridate::month(time) %in% Mnths)
  time <- time_local[setUse]
  flux_pred <- MBR_9min_FILTER$FG_mean[setUse]
  flux_meas <- MBR_9min_FILTER$FH2O_interp[setUse]
  flux_tracer <- MBR_9min_FILTER$FC_turb_interp[setUse]
  zoL <-  MBR_9min_FILTER$zoL[setUse]
  ustar_threshold <- MBR_9min_FILTER$ustar_threshold[setUse]
  tower_pair <- MBR_9min_FILTER$dlvl[setUse]
  tower_pairs <- unique(tower_pair)
  
  # Pre-filtered data
  time_raw <- lubridate::with_tz(MBRflux_align$match_time_H2O,tz$tz[tz$Site==site])
  setUse_raw <- (lubridate::month(time_raw) %in% Mnths)
  time_raw <- time_raw[setUse_raw]
  flux_pred_raw <- MBRflux_align$FH2O_MBR_CO2trace_mean[setUse_raw]
  flux_meas_raw <- MBRflux_align$FH2O_interp_H2O[setUse_raw]
  flux_meas_raw[MBRflux_align$ustar_interp_H2O[setUse_raw] < median(MBRflux_align$ustar_interp_H2O[setUse_raw],na.rm=TRUE)] <- NA # Filter for ustar
  tower_pair_raw <- MBRflux_align$dLevelsAminusB_H2O[setUse_raw]
  
  # Compute CCC 
  ccc <- data.frame(site=site,
                    gas=gas,
                    tracer=tracer,
                    tower_pair=tower_pairs,
                    n=as.numeric(NA),
                    nraw=as.numeric(NA),
                    ccc=as.numeric(NA),
                    r=as.numeric(NA),
                    IQRpred=as.numeric(NA),
                    zoL_median=as.numeric(NA),
                    zoL_25=as.numeric(NA),
                    zoL_75=as.numeric(NA),
                    ustar_threshold=as.numeric(NA))
  tower_pair_plot <- tower_pair
  for (idxTp in tower_pairs){
    setTp <- tower_pair == idxTp
    setTp_raw <- tower_pair_raw == idxTp
    
    # Compute ccc
    cccIdx <- calculate_lins_ccc(flux_pred[setTp], flux_meas[setTp])$rho.c$est
    
    ccc[ccc$tower_pair==idxTp,"ccc"] <- cccIdx
    
    # Compute r (correlation coef)
    try({
      rIdx <- cor(flux_pred[setTp], flux_meas[setTp],use='complete.obs')
      ccc[ccc$tower_pair==idxTp,"r"] <- rIdx
    },silent=TRUE)
    
    # Compute sample sizes
    n <- sum(!is.na(flux_pred[setTp]+flux_meas[setTp])) # Sample size of available comparison
    ccc[ccc$tower_pair==idxTp,"n"] <- n
    
    nraw <- sum(!is.na(flux_pred_raw[setTp_raw]+flux_meas_raw[setTp_raw])) # Sample size of unfiltered data
    ccc[ccc$tower_pair==idxTp,"nraw"] <- nraw
    
    # Compute IQR
    iqrPred <- stats::IQR(flux_pred[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"IQRpred"] <- iqrPred
    
    # Compute ancillary statistics
    zoL_median <- median(zoL[setTp],na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_median"] <- zoL_median
    zoL_25 <- quantile(zoL[setTp],0.25,na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_25"] <- zoL_25
    zoL_75 <- quantile(zoL[setTp],0.75,na.rm=TRUE)
    ccc[ccc$tower_pair==idxTp,"zoL_75"] <- zoL_75
    ustar_threshold_median <- median(ustar_threshold[setTp],na.rm=TRUE) # It's all a single values
    ccc[ccc$tower_pair==idxTp,"ustar_threshold"] <- ustar_threshold_median
    
    # Annotate for plotting
    tower_pair_plot[tower_pair==idxTp] <- paste0(idxTp, ": CCC=",round(cccIdx,2),"; n=",n)
    
  }
  cccAll <- rbind(cccAll,ccc) # Save
  
  # Plot the 1:1
  fig <- plot_ly(x = flux_meas, y = flux_pred, color=tower_pair_plot, type = 'scatter', mode = 'markers') %>%
    layout(title = paste0(site,' F',gas,' (filtered)'),
           xaxis = list(title = 'measured'), 
           yaxis = list(title = 'predicted'),
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
  if (PlotOne2One){print(fig)}
    
  # Compute Diel Patterns
  flux_meas[is.na(flux_pred)] <- NA
  flux_pred[is.na(flux_meas)] <- NA
  flux_meas_raw[is.na(flux_pred_raw)] <- NA
  flux_pred_raw[is.na(flux_meas_raw)] <- NA
  tower_pair_diel <- ccc$tower_pair[ccc$ccc >= cccMin] 
  if(length(tower_pair_diel) > 0){
    setDiel <- tower_pair %in% tower_pair_diel
    flux_diel <- calculate.diel.ptrn(time=time[setDiel],data=data.frame(flux_meas=flux_meas[setDiel],flux_pred=flux_pred[setDiel]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=FALSE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' F',gas,' Diel Pattern'))
    setDiel_raw <- tower_pair_raw %in% tower_pair_diel
    flux_diel_raw <- calculate.diel.ptrn(time=time_raw[setDiel_raw],data=data.frame(flux_meas=flux_meas_raw[setDiel_raw],flux_pred=flux_pred_raw[setDiel_raw]),Int=as.difftime(30,units='mins'),Stat=Stat,Ucrt=Ucrt,Plot=FALSE,NumSampMin=NumSampMin,TitlPlot=paste0(site,' F',gas,' Unfiltered Diel Pattern'))
    
    # Save
    flux_diel_site <- data.frame(time=c(flux_diel$flux_meas$time,
                                        flux_diel$flux_pred$time,
                                        flux_diel_raw$flux_meas$time,
                                        flux_diel_raw$flux_pred$time),
                                 flux=c(flux_diel$flux_meas$stat,
                                        flux_diel$flux_pred$stat,
                                        flux_diel_raw$flux_meas$stat,
                                        flux_diel_raw$flux_pred$stat),
                                 ucrt=c(flux_diel$flux_meas$ucrt,
                                        flux_diel$flux_pred$ucrt,
                                        flux_diel_raw$flux_meas$ucrt,
                                        flux_diel_raw$flux_pred$ucrt),
                                 n=c(flux_diel$flux_meas$n,
                                        flux_diel$flux_pred$n,
                                        flux_diel_raw$flux_meas$n,
                                        flux_diel_raw$flux_pred$n),
                                 site=site,
                                 gas=gas,
                                 tracer=tracer,
                                 type=c(rep('measured',48),rep('predicted',48),rep('measured',48),rep('predicted',48)),
                                 filtering=c(rep('filtered',96),rep('ustar only',96)),
                                 typeComb=c(c(rep('filtered EC',48),rep('filtered MBR',48),rep('unfiltered EC',48),rep('unfiltered MBR',48)))
    )
    
    # Plot all 4 diel patterns (unfiltered measured, unfiltered, predicted, filtered measured, filtered predicted )
    if(PlotDiel){
      fig <- plot_ly(data = flux_diel_site, x = ~time, y = ~flux, color = ~typeComb, type = 'scatter', mode = 'markers+lines',
                     error_y = ~list(array = flux_diel_site$ucrt,
                                     color = '#999999'))
      fig <- fig %>%
        layout(title = paste0(site,' F',gas,' Diel Pattern'),
               xaxis = list(title = 'Time (hours)',
                            range=c(0,24)), 
               yaxis = list(title = Stat))
      
      print(fig)
    }
    
    if(exists('flux_diel_all')){
      flux_diel_all <- rbind(flux_diel_all,flux_diel_site)
    } else {
      flux_diel_all <- flux_diel_site 
    }
    
  }
  # readline(prompt = "Hit Enter to Continue")
}

# Save the CCC results
if(save == TRUE){
  save(cccAll,flux_diel_all,file=fs::path(localdir,'AllSites_MBR_Eval.Rdata'))
}



