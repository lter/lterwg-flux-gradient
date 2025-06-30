
# Filter for MBR 

filter_fluxes <- function( df, 
                           dConcSNR.min,
                           approach){
  df <- as.data.frame(df) 
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach !="MBR"){
  
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  
  df.new <- df %>% mutate(dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
                              filter(dLevelsAminusB  %in% H.filter.list,
                                     TowerPosition_A != TowerPosition_B,
                                     dConcSNR >= dConcSNR.min,
                                     dConcTSNR >= dConcSNR.min,
                                     ustar_interp >= ustar_threshold )
  return(df.new)
}


Apply.filter <- function( site.tibble,
                          dConcSNR.min, 
                          approach){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    print(i)
    filtered.data <- filter_fluxes( df = site.tibble[i],
                                    dConcSNR.min = dConcSNR.min,
                                    approach = approach) 
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}

# Filter Report:
filter_report <- function( df, 
                           dConcSNR.min, 
                           approach){
  df <- as.data.frame(df) 
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach !="MBR"){
    
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>% 
    mutate(
      flag.dConcSNR = case_when( dConcSNR >= dConcSNR.min ~ 0, dConcSNR < dConcSNR.min ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR >= dConcSNR.min ~ 0, dConcTSNR < dConcSNR.min ~ 1, is.na(dConcTSNR ) ~ 0),
           flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0),   
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR ,
      
           flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = dLevelsAminusB,
             total =  length(timeEnd_A), 
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}

# Stability filter report:
WP_9min.df.final$Stability_500
filter_report_stability <- function( df, 
                           dConcSNR.min, 
                           approach){
  df <- as.data.frame(df) 
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach !="MBR"){
    
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>% 
    mutate(
      flag.dConcSNR = case_when( dConcSNR >= dConcSNR.min ~ 0, dConcSNR < dConcSNR.min ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR >= dConcSNR.min ~ 0, dConcTSNR < dConcSNR.min ~ 1, is.na(dConcTSNR ) ~ 0),
      flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0),   
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR ,
      
      flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = c(dLevelsAminusB,Stability_500),
             total =  length(timeEnd_A), 
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}


Generate.filter.report <- function( site.tibble,
                                    dConcSNR.min,
                                    approach){
  
  sites <- names(site.tibble)
  
  REPORT <- data.frame()
  
  for ( i in sites){
    print(i)
    
    filtered.data <- filter_report ( df = site.tibble[i],
                                     dConcSNR.min =  dConcSNR.min,
                                     approach = approach) %>% mutate(site = i)
    
    REPORT <-  REPORT %>% rbind(filtered.data )
    
  }
  
  
  return(REPORT  )  
}


# Old Version of functions
filter_fluxes_old <- function( df, 
                           flux.limit, 
                           ustar.filter, 
                           diff.limit, 
                           FG_sd.limit,
                           dConcSNR.min,
                           rmvCrossGrad, # Set to TRUE to remove data where cross_grad_flag == 1. Applies only to AE and WP methods
                           rmvEddyOutlier, # Set to TRUE to remove data where Eddy_outlier == 1. Applies only to AE and WP methods
                           approach){
  df <- as.data.frame(df) 
  #names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 
  } else {
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  if(rmvCrossGrad == FALSE){
    df$cross_grad_flag <- 0
  }
  if(rmvEddyOutlier == FALSE){
    df$Eddy_outlier <- 0
  }
  
  
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B,
           dConcSNR <= dConcSNR.min,
           dConcTSNR <= dConcSNR.min,
           #dConc_pvalue <= 0.1, 
           #FG_mean < flux.limit & FG_mean > -flux.limit,
           ustar_interp >=  ustar_threshold )
  #Stability_100 == 'unstable',
  #Eddy_outlier != 1,
  #cross_grad_flag != 1,
  #diff.flux <  diff.limit,
  #abs(FG_sd) < FG_sd.limit,)
  return(df.new)
}

Apply.filter <- function( site.tibble,
                          flux.limit, 
                          ustar.filter, 
                          FG_sd.limit,
                          diff.limit,
                          rmvCrossGrad, # Set to TRUE to remove data where cross gradient flag == 1. Applies only to AE and WP methods
                          rmvEddyOutlier, # Set to TRUE to remove data where Eddy_outlier == 1. Applies only to AE and WP methods
                          dConcSNR.min, approach){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    print(i)
    filtered.data <- filter_fluxes( df = site.tibble[i],
                                    flux.limit = flux.limit, 
                                    ustar.filter = ustar.filter, 
                                    FG_sd.limit = FG_sd.limit,
                                    diff.limit = diff.limit,
                                    dConcSNR.min = dConcSNR.min,
                                    rmvCrossGrad = rmvCrossGrad, 
                                    rmvEddyOutlier = rmvEddyOutlier, 
                                    approach = approach) 
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}

# Filter Report:

filter_report_old <- function( df, 
                           dConcSNR.min, 
                           rmvCrossGrad, 
                           rmvEddyOutlier,
                           approach){
  df <- as.data.frame(df) 
  
  #names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 
  } else {
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  if(rmvCrossGrad == FALSE){
    df$cross_grad_flag <- 0
  }
  if(rmvEddyOutlier == FALSE){
    df$Eddy_outlier <- 0
  }
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>% 
    mutate(
      #flag.dConc_pvalue = case_when( dConc_pvalue <= 0.1 ~ 0, dConc_pvalue > 0.1 ~ 1, is.na(dConc_pvalue ) ~ 0),
      # flag.FG_mean = case_when(abs(FG_mean) <= flux.limit ~ 0 , abs(FG_mean) > flux.limit ~ 1, is.na(FG_mean) ~ 0),
      flag.dConcSNR = case_when( dConcSNR <= dConcSNR.min ~ 0, dConcSNR > dConcSNR.min ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR <= dConcSNR.min ~ 0, dConcTSNR > dConcSNR.min ~ 1, is.na(dConcTSNR ) ~ 0),
      flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0), 
      #flag.cross_grad_flag = case_when(cross_grad_flag != 1 ~ 0, cross_grad_flag == 1 ~ 1, is.na(cross_grad_flag) ~ 0),
      # flag.Eddy_outlier = case_when(Eddy_outlier !=1 ~ 0, Eddy_outlier ==1 ~ 1, is.na( Eddy_outlier) ~ 0),
      #flag.FG_sd = case_when( abs(FG_sd) <= FG_sd.limit ~ 0 ,abs(FG_sd) > FG_sd.limit ~ 1, is.na( FG_sd) ~ 0),
      #flag.dConc = case_when( (dConcSNR.filter >= dConcSNR.min) & (dConcTSNR.filter >= dConcSNR.min) ~ 0 , (dConcSNR.filter < dConcSNR.min) | (dConcTSNR.filter < dConcSNR.min) ~ 1, is.na( FG_sd) ~ 0),
      
      # interaction.ALL= flag.dConc_pvalue + flag.FG_mean + flag.ustar_interp +flag.cross_grad_flag + flag.Eddy_outlier+flag.FG_sd + flag.dConc  ,
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR ,
      
      flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = dLevelsAminusB,
             total =  length(timeEnd_A), 
             #flag.dConc_pvalue = sum(flag.dConc_pvalue, na.rm=T)/ total *100,
             #flag.FG_mean  = sum(flag.FG_mean, na.rm=T)/ total *100,
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             #flag.cross_grad_flag  = sum(flag.cross_grad_flag, na.rm=T) /total *100,
             #flag.Eddy_outlier  = sum( flag.Eddy_outlier, na.rm=T)/ total *100,
             #flag.FG_sd  = sum(flag.FG_sd, na.rm=T)/ total *100,
             #flag.dConc  = sum(flag.dConc, na.rm=T) /total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}

Generate.filter.report_old <- function( site.tibble,
                                    flux.limit, 
                                    ustar.filter, 
                                    FG_sd.limit,
                                    diff.limit,
                                    dConcSNR.min,
                                    rmvCrossGrad, 
                                    rmvEddyOutlier,
                                    approach){
  
  sites <- names(site.tibble)
  
  REPORT <- data.frame()
  
  for ( i in sites){
    print(i)
    
    filtered.data <- filter_report ( df = site.tibble[i],
                                     flux.limit = flux.limit, 
                                     ustar.filter = ustar.filter, 
                                     FG_sd.limit = FG_sd.limit,
                                     diff.limit = diff.limit,
                                     dConcSNR.min =  dConcSNR.min,
                                     rmvCrossGrad = rmvCrossGrad, 
                                     rmvEddyOutlier = rmvEddyOutlier,
                                     approach = approach) %>% mutate(site = i)
    
    REPORT <-  REPORT %>% rbind(filtered.data )
    
  }
  
  
  return(REPORT  )  
}
