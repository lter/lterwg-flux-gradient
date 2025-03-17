# Reformat the MBR list to be the same as the AE and WP list:

format_MBR <- function(df){
  
  
  df <- df %>% distinct(match_time, .keep_all = TRUE)
  names <- df %>% names

  dConc.names <- names %>% str_detect('dConc') %>% keep(names, .)
  co2.names.1 <- names %>% str_detect('_CO2$') %>% keep(names, .) %>% setdiff(  dConc.names )
  h2o.names.1 <- names %>% str_detect('_H2O$') %>% keep(names, .) 
  ch4.names.1 <- names %>% str_detect('_CH4$') %>% keep(names, .) 
  mbr.names.1 <- names %>% str_detect('_MBR_') %>% keep(names, .) 

  
  co2.names <- co2.names.1 %>% setdiff(  c(dConc.names,h2o.names.1, ch4.names.1,mbr.names.1 ))
  
  h2o.names <- h2o.names.1 %>% 
    setdiff(  c(dConc.names,co2.names, ch4.names.1,mbr.names.1 ))
  
  ch4.names <-  ch4.names.1%>% 
    setdiff(  c(dConc.names,h2o.names, co2.names,mbr.names.1 ))
  
  mbr.names  <-  mbr.names.1 %>% 
    setdiff(  c(dConc.names,h2o.names, ch4.names,co2.names ))
  
  common.names <- df %>% select( !co2.names & !h2o.names & !ch4.names & !mbr.names & !dConc.names) %>% names
  
  
  # Gradient FLux CH4
  mbr.ch4.names <- mbr.names %>% str_detect('FCH4') %>% keep(mbr.names, .) 
  mbr.ch4.mean.names <- mbr.ch4.names %>% str_detect('mean') %>% keep(mbr.ch4.names, .)
  mbr.ch4.mean.co2.names <- mbr.ch4.mean.names %>% str_detect('CO2') %>% keep(mbr.ch4.mean.names, .)
  mbr.ch4.mean.h2o.names <- mbr.ch4.mean.names %>% str_detect('H2O') %>% keep(mbr.ch4.mean.names, .)
  
  mbr.ch4.sd.names <- mbr.ch4.names %>% str_detect('sd') %>% keep(mbr.ch4.names, .)
  mbr.ch4.sd.co2.names <- mbr.ch4.sd.names %>% str_detect('CO2') %>% keep(mbr.ch4.sd.names, .)
  mbr.ch4.sd.h2o.names <- mbr.ch4.sd.names %>% str_detect('H2O') %>% keep(mbr.ch4.sd.names, .)
  
  # Gradient FLux CO2
  mbr.co2.names <- mbr.names %>% str_detect('FCO2') %>% keep(mbr.names, .) 
  mbr.co2.mean.h2o.names <- mbr.co2.names %>% str_detect('mean') %>% keep(mbr.co2.names, .)
  mbr.co2.sd.h2o.names <- mbr.co2.names %>% str_detect('sd') %>% keep(mbr.co2.names, .)
  
  mbr.h2o.names <- mbr.names %>% str_detect('FH2O') %>% keep(mbr.names, .) 
  mbr.h2o.mean.co2.names <- mbr.h2o.names %>% str_detect('mean') %>% keep(mbr.h2o.names, .)
  mbr.h2o.sd.co2.names <- mbr.h2o.names %>% str_detect('sd') %>% keep(mbr.h2o.names, .)
  
  #dConc
  dConc.CO2.names <-  dConc.names  %>% str_detect('CO2') %>% keep(dConc.names, .) 
  dConc.CO2.mean.names <-   dConc.CO2.names  %>% str_detect('dConc_CO2$') %>% keep(dConc.CO2.names, .) 
  dConc.CO2.pvalue.names <-   dConc.CO2.names  %>% str_detect('pvalue') %>% keep(dConc.CO2.names, .) 
  dConc.CO2.sd.names <-  dConc.CO2.names  %>% str_detect('sd') %>% keep(dConc.CO2.names, .) 
  dConc.CO2.bin.names <-  dConc.CO2.names  %>% str_detect('bin') %>% keep(dConc.CO2.names, .) 
  
  dConc.H2O.names <-  dConc.names  %>% str_detect('H2O') %>% keep(dConc.names, .) 
  dConc.H2O.mean.names <-   dConc.H2O.names  %>% str_detect('dConc_H2O$') %>% keep(dConc.H2O.names, .) 
  dConc.H2O.pvalue.names <-   dConc.H2O.names  %>% str_detect('pvalue') %>% keep(dConc.H2O.names, .) 
  dConc.H2O.sd.names <-  dConc.H2O.names  %>% str_detect('sd') %>% keep(dConc.H2O.names, .) 
  dConc.H2O.bin.names <-  dConc.H2O.names  %>% str_detect('bin') %>% keep(dConc.H2O.names, .) 
  
  dConc.CH4.names <-  dConc.names  %>% str_detect('CH4') %>% keep(dConc.names, .) 
  dConc.CH4.mean.names <-   dConc.CH4.names  %>% str_detect('dConc_CH4$') %>% keep(dConc.CH4.names, .) 
  dConc.CH4.pvalue.names <-   dConc.CH4.names  %>% str_detect('pvalue') %>% keep(dConc.CH4.names, .) 
  dConc.CH4.sd.names <-  dConc.CH4.names  %>% str_detect('sd') %>% keep(dConc.CH4.names, .) 
  dConc.CH4.bin.names <-  dConc.CH4.names  %>% str_detect('bin') %>% keep(dConc.CH4.names, .) 
  
  # DataFrames:
  
  # EC Fluxes:
  flux.df.ec <- function( df, common.names, flux.names, gas){

    common.names <- common.names[ common.names !='match_time']
    F.df <- df %>% select(all_of(common.names), all_of(flux.names))
    flux.names.new = gsub(paste('_', gas, sep=''), '', flux.names)
    names( F.df ) = c( common.names, flux.names.new )
    F.df$gas <- gas
    F.df$cross_grad_flag <- 0 # I added this so that I can have one filter function
    
    return(F.df )
    
  }
  

  
  Fco2.df <- flux.df.ec(df, common.names, co2.names, gas= 'CO2' )
  Fh2o.df <- flux.df.ec(df, common.names, h2o.names, gas= 'H2O' )
  Fch4.df <- flux.df.ec(df, common.names, ch4.names, gas= 'CH4' )


  ec.fluxes <- rbind(Fco2.df, Fh2o.df, Fch4.df  )
  
  # MBR FLuxes:
  fg.df <- function( df, common.names, flux.names, gas, tracer){
    
    F.df <- df %>% select(all_of(common.names), all_of(flux.names))
    flux.names.new = c('FG_mean', 'FG_sd')
    names( F.df ) = c( common.names, flux.names.new )
    F.df$gas <- gas
    F.df$tracer <- tracer
    return(F.df )
    
  }
  
  mbr.ch4.df.tco2 <- fg.df( df = df, common.names, 
                        flux.names = c(mbr.ch4.mean.co2.names, mbr.ch4.sd.co2.names), 
                        gas = "CH4", 
                        tracer = 'CO2')
 
  mbr.ch4.df.th20 <- fg.df( df = df, common.names, 
                        flux.names = c(mbr.ch4.mean.h2o.names, mbr.ch4.sd.h2o.names), 
                        gas = "CH4", 
                        tracer = 'H2O')
 
  mbr.co2.df.th2o <- fg.df( df = df, common.names, 
                            flux.names = c(mbr.co2.mean.h2o.names, mbr.co2.sd.h2o.names), 
                            gas = "CO2", 
                            tracer = 'H2O')

  mbr.h2o.df.tco2 <- fg.df( df = df, common.names, 
                            flux.names = c(mbr.h2o.mean.co2.names, mbr.h2o.sd.co2.names), 
                            gas = "H2O", 
                            tracer = 'CO2')
  

#dConc
  dConc.df <- function( df, common.names, 
                        flux.names, 
                        gas){
    
    F.df <- df %>% select(all_of(common.names), all_of(flux.names))
    flux.names.new = c('dConc', 'dConc_pvalue', 'dConc_sd', 'dConc_bin')
    names( F.df ) = c( common.names, flux.names.new )
    F.df$gas <- gas
    return(F.df )
    
  }

  
  
  dConc.CO2.df <- dConc.df( df, common.names, 
                        flux.names=c(dConc.CO2.mean.names, dConc.CO2.pvalue.names, dConc.CO2.sd.names, dConc.CO2.bin.names), 
                        gas = 'CO2')
  
  dConc.CH4.df <- dConc.df( df, common.names, 
                        flux.names=c(dConc.CH4.mean.names, dConc.CH4.pvalue.names, dConc.CH4.sd.names, dConc.CH4.bin.names), 
                        gas = 'CH4')
  
  dConc.H2O.df <- dConc.df( df, common.names, 
                        flux.names=c(dConc.H2O.mean.names, dConc.H2O.pvalue.names, dConc.H2O.sd.names, dConc.H2O.bin.names), 
                        gas = 'H2O')
  
  # Combine these MBR and dConc:

  mbr.co2.df.th2o.j <- full_join(mbr.co2.df.th2o,   dConc.CO2.df, by = c("match_time", "site", "gas")) 
  mbr.h2o.df.tco2.j <- full_join(mbr.h2o.df.tco2,   dConc.H2O.df, by = c("match_time", "site", "gas"))
  mbr.ch4.df.th20.j <- full_join(mbr.ch4.df.th20,   dConc.CH4.df, by = c("match_time", "site", "gas"))
  mbr.ch4.df.tco2.j <- full_join(mbr.ch4.df.tco2,   dConc.CH4.df, by = c("match_time", "site", "gas"))
  
  g.fluxes <-  rbind( mbr.co2.df.th2o.j, mbr.h2o.df.tco2.j, mbr.ch4.df.th20.j,  mbr.ch4.df.tco2.j)

  mbr.ec <- full_join( g.fluxes ,  ec.fluxes, by = c("match_time", "site", "gas"))
  
return(mbr.ec )
 }

apply_format_MBR <- function(site.list){
  
  sites <- names(site.list)
  site.tibble_Reformat <- list()
  
  for ( i in sites){
    print(i)
    site.list[i]
    
    df <- site.list[i] %>% as.data.frame
    names(df) <- substring( names(df), 6)
    
    df.reformat <- format_MBR(df)
    site.tibble_Reformat[i] <- list(df.reformat)
    
  }
  return(site.tibble_Reformat )
}

