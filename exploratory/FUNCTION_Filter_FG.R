
# Filtering Functions ####

filter.MBR.CO2 <- function( df, flux.limit, 
                            ustar.filter, 
                            H.filter.list,
                            FG_sd.limit){
  df.new <- df %>% filter(dConc_pvalue_CO2 <= 0.1, 
                          FCO2_MBR_H2Otrace_mean < flux.limit & FCO2_MBR_H2Otrace_mean > -flux.limit, 
                          ustar_interp_CO2 >  ustar.filter,
                          dLevelsAminusB_CO2  %in% H.filter.list,
                          FCH4_MBR_CO2trace_sd > FG_sd.limit)
  return(df.new)
}

filter.MBR.CO2.FC <- function( df, flux.limit, 
                            ustar.filter, 
                            H.filter.list,
                            diff.limit,
                            FG_sd.limit){

  df.new <- df %>% mutate(diff.flux = abs(FCO2_MBR_H2Otrace_mean - FC_nee_interp_CO2)  ) %>% 
    filter(dConc_pvalue_CO2 <= 0.1, 
           FCO2_MBR_H2Otrace_mean < flux.limit & FCO2_MBR_H2Otrace_mean > -flux.limit, 
           ustar_interp_CO2 >  ustar.filter,
           dLevelsAminusB_CO2  %in% H.filter.list,
           FCH4_MBR_CO2trace_sd > FG_sd.limit,
           diff.flux <  diff.limit)
  
  
  return(df.new)
}

filter.AEWP <- function( df, flux.limit, flux.sd.limit, ustar.filter, H.filter.list, diff.limit){

  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_nee_interp)  ) %>%  filter(dConc_pvalue <= 0.1, 
                          FG_mean < flux.limit & FG_mean > -flux.limit,
                          abs(FG_sd) <  flux.sd.limit,
                          ustar_interp >  ustar.filter,
                          Stability_100 == 'unstable',
                          cross_grad_flag != 1,
                          dLevelsAminusB  %in% H.filter.list,
                          diff.flux <  diff.limit,
                          FG_sd < 10)
  
  return(df.new)
}
