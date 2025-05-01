
source(fs::path(DirRepo,'exploratory/FUNCTION_DIURNAL.R' ))

# Calculate Diurnal Patterns by Year-month:
Diurnal.MBR.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_9min_FILTER_BH, 
                                          FG_flux = 'FG_mean', 
                                          EC_flux = 'FC_turb_interp',
                                          Gas = "CO2")

Diurnal.WP.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_9min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', 
                                         Gas = "CO2")

Diurnal.AE.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_9min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', 
                                         Gas = "CO2")

diurnal.summary.CO2 <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.CO2, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.CO2, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.CO2, TYP='WP' ) )  

# Diurnals for H2O

# add Gas to the function!!!!
Diurnal.MBR.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_9min_FILTER_BH, 
                                          FG_flux = 'FG_mean', 
                                          EC_flux = 'FH2O_interp', Gas = "H2O")

Diurnal.WP.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_9min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FH2O_interp', Gas = "H2O")

Diurnal.AE.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_9min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FH2O_interp', Gas = "H2O")

diurnal.summary.H2O <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.H2O, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.H2O, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.H2O, TYP='WP' ) )  

diurnal.summary.H2O$Type <- factor( diurnal.summary.H2O$Type, levels= c('MBR', 'AE', 'WP'))

# Plots :

setwd(dir.diel)

for ( i in site.list){
  
  df.MBR <-  Diurnal.MBR.CO2[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  df.AE <-  Diurnal.AE.CO2[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  df.WP <-  Diurnal.WP.CO2[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  p1 <- p2 <- p3 <- NULL
  
  try({
   
    p1 <- ggplot( data = df.MBR) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") 
    
    p2 <- ggplot( data = df.AE) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") 
    
    p3 <-ggplot( data = df.WP) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") 
  })
  
  print(ggarrange( p1, p2, p3, nrow=3))
  
  setwd(dir.diel)
  
  png(paste("Diurnal_CO2_", i,".png", sep=""), width=6, 
      height=6, units="in", res=1200)
  
  print(ggarrange( p1, p2, p3, nrow=3))
  dev.off()
  
  print("done with CO2 Diel")       
  
  df.MBR <-  Diurnal.MBR.H2O[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  df.AE <-  Diurnal.AE.H2O[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  df.WP <-  Diurnal.WP.H2O[[i]] %>% as.data.frame %>% mutate( Hour = Hour %>%  as.numeric)
  
  p1 <- p2 <- p3 <- NULL
  try({
    p1 <- ggplot( data = df.MBR) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") 
    
    p2 <- ggplot( data = df.AE) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") 
    
    p3 <-ggplot( data = df.WP) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") 
  })
  
  print(ggarrange( p1, p2, p3, nrow=3))
  
  png(paste("Diurnal_H2O_", i,".png", sep=""), width=6, 
      height=5, units="in", res=1200)
  print(ggarrange( p1, p2, p3, nrow=3))
  dev.off()

  
  print("done with H2O diel")   
  
}

