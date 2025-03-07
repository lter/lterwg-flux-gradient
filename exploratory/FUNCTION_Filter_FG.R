
# Filter for MBR 

filter_fluxes <- function( df, 
                           flux.limit, 
                           ustar.filter, 
                           diff.limit, 
                           FG_sd.limit,
                           dConc.limit){
  df <- as.data.frame(df) 
  names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConc.filter = dConc/dConc_sd ) %>%  filter(dConc_pvalue <= 0.1, 
                                                                      FG_mean < flux.limit & FG_mean > -flux.limit,
                                                                      ustar_interp >  ustar.filter,
                                                                      #Stability_100 == 'unstable',
                                                                      cross_grad_flag != 1,
                                                                      dLevelsAminusB  %in% H.filter.list,
                                                                      diff.flux <  diff.limit,
                                                                      abs(FG_sd) < FG_sd.limit,
                                                                      diff.flux < dConc.limit,
                                                                      TowerPosition_A != TowerPosition_B)
  
  return(df.new)
}
                    

Apply.filter <- function( site.tibble,
                          flux.limit, 
                          ustar.filter, 
                          FG_sd.limit,
                          diff.limit,
                          dConc.limit){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    print(i)
    filtered.data <- filter_fluxes( df = site.tibble[i],
                                    flux.limit = flux.limit, 
                                    ustar.filter = ustar.filter, 
                                    FG_sd.limit = FG_sd.limit,
                                    diff.limit = diff.limit,
                                    dConc.limit =  dConc.limit) 
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}
                           
