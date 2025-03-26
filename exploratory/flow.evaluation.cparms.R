source(fs::path(DirRepo,'exploratory/FUNCTION_LRC_Parms.R' ))

SITES_MBR_9min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_MBR_9min_FILTER_BH, 
                                          iterations = 10000, 
                                          priors.lrc= priors.lrc, 
                                          priors.trc= priors.trc, 
                                          idx = 'YearMon',
                                          PAR = 'PAR',
                                          nee = 'FC_turb_interp',
                                          TA = 'Tair')

# Check that all the sites are in this thing!

SITES_MBR_9min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_MBR_9min_FILTER_BH, 
                                          iterations = 10000, 
                                          priors.lrc= priors.lrc, 
                                          priors.trc= priors.trc, 
                                          idx = 'YearMon',
                                          PAR = 'PAR',
                                          nee = 'FG_mean' ,
                                          TA = 'Tair')

SITES_AE_9min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_AE_9min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )


SITES_AE_9min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_AE_9min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )

SITES_WP_9min_CPARMS_EC <- PARMS_Sites( sites.tibble = SITES_WP_9min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )


SITES_WP_9min_CPARMS_FG <- PARMS_Sites( sites.tibble = SITES_WP_9min_FILTER_BH, 
                                         iterations = 10000, 
                                         priors.lrc= priors.lrc, 
                                         priors.trc= priors.trc,
                                         idx = 'YearMon',
                                         PAR = 'PAR',
                                         TA='Tair',
                                         nee = 'FG_mean' )



merge.CParms <- function( tibble1, tibble2,  TYP1, TYP2){
  
  tibble <- list()
  for( i in names(tibble1)){
    print(i)
    
    df.1 <- tibble1[i] %>% as.data.frame()
    names( df.1) <- substring( names(  df.1), 6)
    
    df.1n <-  df.1 %>% mutate( TYP = TYP1)
    
    
    df.2 <- tibble2[i] %>% as.data.frame()
    names( df.2) <- substring( names(  df.2), 6)
    
    df.2n <-  df.2 %>% mutate( TYP = TYP2)
    
    new.df <- rbind( df.1n, df.2n)
    
    tibble[i] <- list(  new.df)
  }
  
  return(tibble)
}

MBR.CPARMS <- merge.CParms(tibble1 = SITES_MBR_9min_CPARMS_EC,
                           tibble2 = SITES_MBR_9min_CPARMS_FG,
                           TYP1 = 'EC' , TYP2 = 'FG')

AE.CPARMS <- merge.CParms(tibble1 = SITES_AE_9min_CPARMS_EC,
                          tibble2 = SITES_AE_9min_CPARMS_FG,
                          TYP1 = 'EC' , TYP2 = 'FG')


WP.CPARMS <- merge.CParms(tibble1 = SITES_WP_9min_CPARMS_EC,
                          tibble2 = SITES_WP_9min_CPARMS_FG,
                          TYP1 = 'EC' , TYP2 = 'FG')
