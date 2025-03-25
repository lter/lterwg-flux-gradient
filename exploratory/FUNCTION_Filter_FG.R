
# Filter for MBR 

filter_fluxes <- function( df, 
                           flux.limit, 
                           ustar.filter, 
                           diff.limit, 
                           FG_sd.limit,
                           dConcSNR.min, approach){
  df <- as.data.frame(df) 
  names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 
  } 
  
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR.filter = abs(dConc)/dConc_sd ) %>%  filter(dConc_pvalue <= 0.1, 
                                                                      FG_mean < flux.limit & FG_mean > -flux.limit,
                                                                      ustar_interp >  ustar.filter,
                                                                      #Stability_100 == 'unstable',
                                                                      Eddy_outlier !=1,
                                                                      cross_grad_flag != 1,
                                                                      dLevelsAminusB  %in% H.filter.list,
                                                                      diff.flux <  diff.limit,
                                                                      abs(FG_sd) < FG_sd.limit,
                                                                      dConcSNR.filter > dConcSNR.min,
                                                                      TowerPosition_A != TowerPosition_B)
  
  return(df.new)
}
                    

Apply.filter <- function( site.tibble,
                          flux.limit, 
                          ustar.filter, 
                          FG_sd.limit,
                          diff.limit,
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
                                    approach = approach) 
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}

# Filter Report:

filter_report <- function( df, 
                           flux.limit, 
                           ustar.filter, 
                           diff.limit, 
                           FG_sd.limit,
                           dConcSNR.min, approach){
  df <- as.data.frame(df) 
  
  names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 } 
  
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR.filter = abs(dConc)/dConc_sd ) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>% 
    mutate(flag.dConc_pvalue = case_when( dConc_pvalue <= 0.1 ~ 0, dConc_pvalue > 0.1 ~ 1, is.na(dConc_pvalue ) ~ 0),
           flag.FG_mean = case_when(abs(FG_mean) <= flux.limit ~ 0 , abs(FG_mean) > flux.limit ~ 1, is.na(FG_mean) ~ 0),
           flag.ustar_interp = case_when( ustar_interp >=  ustar.filter~ 0, ustar_interp <  ustar.filter ~ 1, is.na(ustar_interp) ~ 0), 
           flag.cross_grad_flag = case_when(cross_grad_flag != 1 ~ 0, cross_grad_flag == 1 ~ 1, is.na(cross_grad_flag) ~ 0),
           flag.Eddy_outlier = case_when(Eddy_outlier !=1 ~ 0, Eddy_outlier ==1 ~ 1, is.na( Eddy_outlier) ~ 0),
           flag.FG_sd = case_when( abs(FG_sd) <= FG_sd.limit ~ 0 ,abs(FG_sd) > FG_sd.limit ~ 1, is.na( FG_sd) ~ 0),
           flag.dConc = case_when( dConcSNR.filter >= dConcSNR.min ~ 0, dConcSNR.filter < dConcSNR.min ~ 1, is.na( FG_sd) ~ 0),
           
           # Interactions with ustar
           interaction.ustar_dConc_pvalue = flag.ustar_interp + flag.dConc_pvalue,
           flag.ustar_dConc_pvalue = case_when( interaction.ustar_dConc_pvalue == 2 ~ 1 ),
           
           interaction.ustar_FG_mean = flag.ustar_interp + flag.FG_mean,
           flag.ustar_FG_mean = case_when( interaction.ustar_FG_mean == 2 ~ 1 ),
           
           interaction.ustar_cross_grad_flag = flag.ustar_interp + flag.cross_grad_flag,
           flag.ustar_cross_grad_flag = case_when( interaction.ustar_cross_grad_flag == 2 ~ 1 ),
           
           interaction.ustar_Eddy_outlier = flag.ustar_interp + flag.Eddy_outlier,
           flag.ustar_Eddy_outlier = case_when(  interaction.ustar_Eddy_outlier == 2 ~ 1 ),
           
           interaction.ustar_FG_sd = flag.ustar_interp + flag.FG_sd,
           flag.ustar_FG_sd = case_when( interaction.ustar_FG_sd == 2 ~ 1 ),
           
           interaction.ustar_dConc= flag.ustar_interp + flag.dConc,
           flag.ustar_dConc = case_when(  interaction.ustar_dConc == 2 ~ 1 ),
           
           # Interactions with Eddy Outlier
           interaction.Eddy_outlier_dConc_pvalue = flag.Eddy_outlier + flag.dConc_pvalue,
           flag.Eddy_outlier_dConc_pvalue = case_when( interaction.Eddy_outlier_dConc_pvalue == 2 ~ 1 ),
           
           interaction.Eddy_outlier_FG_mean = flag.Eddy_outlier + flag.FG_mean,
           flag.Eddy_outlier_FG_mean = case_when( interaction.Eddy_outlier_FG_mean == 2 ~ 1 ),
           
           interaction.Eddy_outlier_cross_grad_flag = flag.Eddy_outlier + flag.cross_grad_flag,
           flag.Eddy_outlier_cross_grad_flag = case_when( interaction.Eddy_outlier_cross_grad_flag == 2 ~ 1 ),
           
           interaction.Eddy_outlier_FG_sd = flag.Eddy_outlier + flag.FG_sd,
           flag.Eddy_outlier_FG_sd = case_when( interaction.Eddy_outlier_FG_sd == 2 ~ 1 ),
           
           interaction.Eddy_outlier_dConc= flag.Eddy_outlier + flag.dConc,
           flag.Eddy_outlier_dConc= case_when( interaction.Eddy_outlier_dConc == 2 ~ 1 ) , 
           
           interaction.ALL= flag.dConc_pvalue + flag.FG_mean + flag.ustar_interp +flag.cross_grad_flag + flag.Eddy_outlier+flag.FG_sd + flag.dConc  ,
           flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( total =  length(timeEnd_A), 
             flag.dConc_pvalue = sum(flag.dConc_pvalue, na.rm=T)/ total *100,
             flag.FG_mean  = sum(flag.FG_mean, na.rm=T)/ total *100,
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.cross_grad_flag  = sum(flag.cross_grad_flag, na.rm=T) /total *100,
             flag.Eddy_outlier  = sum( flag.Eddy_outlier, na.rm=T)/ total *100,
             flag.FG_sd  = sum(flag.FG_sd, na.rm=T)/ total *100,
             flag.dConc  = sum(flag.dConc, na.rm=T) /total *100,
             
             # Interactions with ustar
             flag.ustar_dConc_pvalue = sum( flag.ustar_dConc_pvalue, na.rm=T) /total *100,
             flag.ustar_FG_mean  = sum( flag.ustar_FG_mean, na.rm=T) / total *100,
             flag.ustar_cross_grad_flag  = sum( flag.ustar_cross_grad_flag, na.rm=T)/ total *100,
             flag.ustar_Eddy_outlier  = sum( flag.ustar_Eddy_outlier , na.rm=T)/ total *100,
             flag.ustar_FG_sd  = sum( flag.ustar_FG_sd, na.rm=T)/ total *100,
             flag.ustar_dConc  = sum(flag.ustar_dConc, na.rm=T)/ total *100,
             
             # Interactions with Eddy Outlier
             flag.Eddy_outlier_dConc_pvalue  = sum(flag.Eddy_outlier_dConc_pvalue, na.rm=T) /total *100,
             flag.Eddy_outlier_FG_mean  = sum(flag.Eddy_outlier_FG_mean, na.rm=T)/ total *100,
             flag.Eddy_outlier_cross_grad_flag  = sum(flag.Eddy_outlier_cross_grad_flag, na.rm=T) /total *100,
             flag.Eddy_outlier_FG_sd  = sum(flag.Eddy_outlier_FG_sd, na.rm=T) /total *100,
             flag.Eddy_outlier_dConc  = sum(flag.Eddy_outlier_dConc, na.rm=T)/total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}

Generate.filter.report <- function( site.tibble,
                          flux.limit, 
                          ustar.filter, 
                          FG_sd.limit,
                          diff.limit,
                          dConcSNR.min,
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
                                     approach = approach) %>% mutate(site = i)
    
    REPORT <-  REPORT %>% rbind(filtered.data )
    
  }
  
  
  return(REPORT  )  
}
