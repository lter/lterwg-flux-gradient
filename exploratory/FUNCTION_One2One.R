# Function to produce 1 to 1 plot by height:

# One to One PLOT: ####
one2one.plots.co2 <- function ( MBR.DF, AE.DF, WP.DF){ 
  
  MBR.DF <- MBR.DF %>% as.data.frame
  names(MBR.DF) <- substring( names( MBR.DF), 6)
  
  AE.DF <- AE.DF %>% as.data.frame
  names(AE.DF) <- substring( names( AE.DF), 6)
  
  WP.DF <- WP.DF %>% as.data.frame
  names(WP.DF) <- substring( names( WP.DF), 6)
  
  
p.1 <- ggplot(data = MBR.DF ,aes(x = FC_turb_interp_CO2, y = FCO2_MBR_H2Otrace_mean ))  + stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + geom_point(alpha=0.1) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = -30, label.y = 30, size = 3) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = -30, label.y = 20, size =3) +
  geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
  facet_wrap(~dLevelsAminusB_CO2, ncol = length(unique(MBR.DF$dLevelsAminusB_CO2)) ) + ylab("MBR") + xlim(-30, 30)+ ylim(-30, 30) + xlab("EC") +theme_bw()


p.2 <- ggplot(data = WP.DF  ,aes(x = FC_nee_interp, y = FG_mean ))  + stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + geom_point(alpha=0.1) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = -30, label.y = 30, size = 3) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = -30, label.y = 20, size =3) +
  geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
  facet_wrap(~ dLevelsAminusB, ncol = length(unique(WP.DF$dLevelsAminusB ))) + ylab("WP")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()

p.3 <- ggplot(data = AE.DF  ,aes(x = FC_nee_interp, y = FG_mean ))  + stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + geom_point(alpha=0.1) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = -30, label.y = 30, size = 3) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = -30, label.y = 20, size =3) +
  geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
  facet_wrap(~ dLevelsAminusB, ncol = length(unique(AE.DF$dLevelsAminusB ))) + ylab("AE")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()

final.plot <- ggpubr::ggarrange(p.1, p.2, p.3,
                  ncol=1, nrow=3)

return(final.plot )
}

# write a function that pulls out the linear model info(R2, LM, RMSE, and stores it in a DF)

linear.parms <- function ( Y, X, DF, TYPE){
  
  model <- lm(data =   DF, DF[,Y] ~ DF[, X])
  DF <- DF[,c(X, Y)] %>% na.omit
  predicted <- predict(model, DF)
  rmse <- sqrt(mean((DF[,Y] %>% na.omit - predicted)^2))
  
  linear.parms <- data.frame( Intercept =model$coefficients[1] , Slope = model$coefficients[2],
                              R2= summary(model)$r.squared, RMSE= rmse) %>% 
    mutate(Apprach = TYPE)
}

one2one.parms.co2 <- function ( MBR.DF, AE.DF, WP.DF){
  
  MBR.DF <- MBR.DF %>% as.data.frame
  names(MBR.DF) <- substring( names( MBR.DF), 6)

  AE.DF <- AE.DF %>% as.data.frame
  names(AE.DF) <- substring( names( AE.DF), 6)
  
  WP.DF <- WP.DF %>% as.data.frame
  names(WP.DF) <- substring( names( WP.DF), 6)
  
  
  linear.parms.final <- data.frame( Intercept = as.numeric() ,
                                    Slope = as.numeric() ,
                                    R2 = as.numeric() ,
                                    RMSE = as.numeric() , 
                                    Apprach = as.character() , 
                                    dLevelsAminusB = as.character() )
  
  for ( a in unique(MBR.DF$dLevelsAminusB_CO2)){
    print(a)
    
    linear.parms.mbr <- try(linear.parms( Y= 'FCO2_MBR_H2Otrace_mean', 
                                     X='FC_turb_interp_CO2', 
                                     DF = MBR.DF %>% 
                                       filter(dLevelsAminusB_CO2 == a), TYPE= 'MBR'), silent = T)
    linear.parms.AE <- try(linear.parms( Y= 'FG_mean', 
                                   X='FC_nee_interp', 
                                   DF = AE.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE= 'AE'), silent = T)
  
  linear.parms.WP <- try(linear.parms( Y= 'FG_mean', 
                                  X='FC_nee_interp', 
                                  DF = WP.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE= 'WP'), silent = T)
  
  
  linear.parms <- rbind(linear.parms.mbr, linear.parms.AE, linear.parms.WP) %>% mutate( dLevelsAminusB = a) 

  linear.parms.final <- rbind( linear.parms.final , linear.parms )
    
  }
  
  row.names(linear.parms.final) <- NULL
  
  return(linear.parms.final)
}

one2one.parms.site <- function ( MBR.tibble, AE.tibble, WP.tibble){
  
  sites <- names( MBR.tibble)
  
  site.tibble_parms <- data.frame( Intercept = as.numeric() ,
                                   Slope = as.numeric() ,
                                   R2 = as.numeric() ,
                                   RMSE = as.numeric() , 
                                   Apprach = as.character() ,
                                   dLevelsAminusB = as.character(), 
                                   Site= as.character())
  
  for ( i in sites){
    print(i)
    
    df.parms <- one2one.parms.co2( MBR.DF= MBR.tibble[i] , 
                      AE.DF = AE.tibble[i], 
                      WP.DF= WP.tibble[i]) %>%
      mutate( Site = i)
    
    site.tibble_parms  <- rbind( site.tibble_parms ,  df.parms)
  }
  
  site.tibble_parms <- site.tibble_parms %>% filter( Intercept > -100 )
 return(site.tibble_parms ) 
}
