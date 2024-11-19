# #' calc.mbr.R
#'
#' @param sitecode NEON site code
#' @param min9 9min interpolated data file for given site
#'
#' @author Jackie Matthes, Cove Sturtevant
#'
#' @return list of gas concentration dataframes containing variables associated with modified bowen ratio calculation
#' 
calc.mbr <- function(min9, bootstrap, nsamp){
  
  # Calculate modified Bowen ratio (MBR) gradient fluxes:
  CO2 = min9Diff.list[["CO2"]]
  CH4 = min9Diff.list[["CH4"]]
  H2O = min9Diff.list[["H2O"]]
  
  # Add gas suffix to all column names to track into combined table
  colnames(CO2) <- paste0(colnames(CO2), '_CO2')
  colnames(H2O) <- paste0(colnames(H2O), '_H2O')
  colnames(CH4) <- paste0(colnames(CH4), '_CH4')
  
  # Keep match_time column for linking among gases 
  CO2$match_time = CO2$match_time_CO2
  H2O$match_time = H2O$match_time_H2O
  CH4$match_time = CH4$match_time_CH4
  
  # Align CO2, H2O, CH4 conc diffs and fluxes by match_time
  MBRflux_align = merge(CO2, CH4, by.x = "match_time", by.y = "match_time") 
  MBRflux_align = merge(MBRflux_align, H2O, by.x = "match_time", by.y = "match_time") 
  
  if(bootstrap == 1){
    # Sample over concentration mean & variance 
    dmmyNrow <- rep(as.numeric(NA),nrow(MBRflux_align))
    FCH4_MBR_H2Otrace_mean = FCH4_MBR_H2Otrace_lo = FCH4_MBR_H2Otrace_hi = FCH4_MBR_H2Otrace_sd = dmmyNrow
    FCO2_MBR_H2Otrace_mean = FCO2_MBR_H2Otrace_lo = FCO2_MBR_H2Otrace_hi = FCO2_MBR_H2Otrace_sd = dmmyNrow
    FCH4_MBR_CO2trace_mean = FCH4_MBR_CO2trace_lo = FCH4_MBR_CO2trace_hi = FCH4_MBR_CO2trace_sd = dmmyNrow
    FH2O_MBR_CO2trace_mean = FH2O_MBR_CO2trace_lo = FH2O_MBR_CO2trace_hi = FH2O_MBR_CO2trace_sd = dmmyNrow
    dConc_CO2_mean = dConc_CO2_sd = dConc_H2O_mean = dConc_H2O_sd = dConc_CH4_mean = dConc_CH4_sd = dmmyNrow
    
    nsamp = nsamp
    dmmyNsamp <- rep(as.numeric(NA),nsamp)
    
    rptInt <- seq(from=1,to=nrow(MBRflux_align),by=round(nrow(MBRflux_align)/10))
    for(i in 1:nrow(MBRflux_align)){ # loop over time to sample conc 
      if(i %in% rptInt){
        message(paste0(Sys.time(), ': Processing ... ',round(i/nrow(MBRflux_align)*100),'%'))
      }
      
      
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
      
      FCO2_MBR_H2Otrace = FH2O_MBR_CO2trace = FCH4_MBR_CO2trace = FCH4_MBR_H2Otrace = dmmyNsamp # re-initialize
      
      FCO2_MBR_H2Otrace = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O[i] * 
                                          (dConc_CO2 / dConc_H2O)),MBRflux_align$FH2O_interp_H2O[i] * 
                                   (dConc_CO2 / dConc_H2O),NA)
      
      FH2O_MBR_CO2trace = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2[i] *
                                          (dConc_H2O[j] / dConc_CO2[j])),MBRflux_align$FC_turb_interp_CO2[i] *
                                   (dConc_H2O[j] / dConc_CO2[j]), NA)
      
      FCH4_MBR_CO2trace = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2[i] *
                                          (dConc_CH4 /dConc_CO2)), MBRflux_align$FC_turb_interp_CO2[i] *
                                   (dConc_CH4 /dConc_CO2), NA)
      
      FCH4_MBR_H2Otrace = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O[i] * 
                                          (dConc_CH4 / dConc_H2O)), MBRflux_align$FH2O_interp_H2O[i] * 
                                   (dConc_CH4 / dConc_H2O), NA)
      
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
  } else {
    
    MBRflux_align$FCO2_MBR_H2Otrace = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O * 
                                                      (MBRflux_align$dConc_CO2 / MBRflux_align$dConc_H2O)),
                                             MBRflux_align$FH2O_interp_H2O * 
                                               (MBRflux_align$dConc_CO2 / MBRflux_align$dConc_H2O),NA)
    
    MBRflux_align$FH2O_MBR_CO2trace = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2 *
                                                      (MBRflux_align$dConc_H2O / MBRflux_align$dConc_CO2)),
                                             MBRflux_align$FC_turb_interp_CO2 *
                                               (MBRflux_align$dConc_H2O / MBRflux_align$dConc_CO2), NA)
    
    MBRflux_align$FCH4_MBR_CO2trace = ifelse(!is.na(MBRflux_align$FC_turb_interp_CO2 *
                                                      (MBRflux_align$dConc_CH4 /MBRflux_align$dConc_CO2)), 
                                             MBRflux_align$FC_turb_interp_CO2 *
                                               (MBRflux_align$dConc_CH4 /MBRflux_align$dConc_CO2), NA)
    
    MBRflux_align$FCH4_MBR_H2Otrace = ifelse(!is.na(MBRflux_align$FH2O_interp_H2O * 
                                                      (MBRflux_align$dConc_CH4 / MBRflux_align$dConc_H2O)), 
                                             MBRflux_align$FH2O_interp_H2O * 
                                               (MBRflux_align$dConc_CH4 / MBRflux_align$dConc_H2O), NA)
  }
  return(MBRflux_align)
}
