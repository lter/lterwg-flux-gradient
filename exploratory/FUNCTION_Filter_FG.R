
# Filtering Functions ####

filter.MBR.CO2 <- function( df, flux.limit, 
                            ustar.filter,
                            diff.limit,
                            FG_sd.limit){
  df <- as.data.frame(df) 
  names(df) <- substring( names(df), 6)
  H.filter.list = df$dLevelsAminusB_CO2 %>% unique 

  df.new <- df %>% mutate(diff.flux = abs(FCO2_MBR_H2Otrace_mean - FC_nee_interp_CO2)  ) %>% 
    filter(dConc_pvalue_CO2 <= 0.1, 
           FCO2_MBR_H2Otrace_mean < flux.limit & FCO2_MBR_H2Otrace_mean > - flux.limit, 
           ustar_interp_CO2 >  ustar.filter,
           dLevelsAminusB_CO2  %in% H.filter.list,
           abs(FCH4_MBR_CO2trace_sd) < FG_sd.limit,
           diff.flux <  diff.limit)
  
  return(df.new)
}

filter.AEWP.CO2 <- function( df, flux.limit, ustar.filter, diff.limit, FG_sd.limit){
  df <- as.data.frame(df) 
  names(df) <- substring( names(df), 6)

  H.filter.list = df$dLevelsAminusB %>% unique
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_nee_interp)  ) %>%  filter(dConc_pvalue <= 0.1, 
                          FG_mean < flux.limit & FG_mean > -flux.limit,
                          ustar_interp >  ustar.filter,
                          Stability_100 == 'unstable',
                          cross_grad_flag != 1,
                          dLevelsAminusB  %in% H.filter.list,
                          diff.flux <  diff.limit,
                          abs(FG_sd) < FG_sd.limit)
  
  return(df.new)
}


# Application :
EC.filter.MBR <- function( site.tibble,
                           flux.limit, 
                           ustar.filter, 
                           FG_sd.limit,
                           diff.limit,
                           AE.tibble){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    
    print(i)
    
    filtered.data <- filter.MBR.CO2( df =site.tibble[i],
                                        flux.limit = flux.limit, 
                                        ustar.filter = ustar.filter,
                                        FG_sd.limit = FG_sd.limit,
                                        diff.limit = diff.limit) 
    
    ae.df <- AE.tibble[i] %>% as.data.frame
    names(ae.df) <- substring( names(ae.df), 6)
    ae.heights <- ae.df$dLevelsAminusB %>% unique
    
    site.tibble_FILTER[i] <- list(filtered.data %>%
                                    filter( dLevelsAminusB_CO2 %in% ae.heights) )
    
    
  }
  
  return(site.tibble_FILTER) 
  
}

EC.filter.AEWP <- function( site.tibble,
                           flux.limit, 
                           ustar.filter, 
                           FG_sd.limit,
                           diff.limit){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    print(i)
    filtered.data <- filter.AEWP.CO2 ( df = site.tibble[i],
                                        flux.limit = flux.limit, 
                                        ustar.filter = ustar.filter, 
                                        FG_sd.limit = FG_sd.limit,
                                        diff.limit = diff.limit) 
    
    site.tibble_FILTER[i] <- list(filtered.data )
    
  }
  
  return(site.tibble_FILTER )  
}


