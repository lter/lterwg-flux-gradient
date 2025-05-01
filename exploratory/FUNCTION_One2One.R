# Function to produce 1 to 1 plot by height:

# One to One PLOT: ####

one2one.plots <- function ( MBR.DF, AE.DF, WP.DF, gas){ 
  
  MBR.DF <- MBR.DF %>% as.data.frame
  names(MBR.DF) <- substring( names( MBR.DF), 6)
  
  AE.DF <- AE.DF %>% as.data.frame
  names(AE.DF) <- substring( names( AE.DF), 6) 
  
  WP.DF <- WP.DF %>% as.data.frame
  names(WP.DF) <- substring( names( WP.DF), 6)
  
  if( gas == 'CO2'){
    
    AE.DF <- AE.DF %>% filter(gas == "CO2")
    WP.DF <- WP.DF %>% filter(gas == "CO2")
    MBR.DF <- MBR.DF %>% filter(gas == "CO2")
    
    p.1 <- ggplot(data = MBR.DF , aes(x = FC_turb_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label = paste(after_stat(rr.label), after_stat(..p.label..), sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)) ) + 
      ylab("MBR") + xlim(-30, 30)+ ylim(-30, 30) + xlab("EC") +theme_bw()
    
    
    p.2 <- ggplot(data = WP.DF  , aes(x = FC_turb_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label =paste(after_stat(rr.label), after_stat(..p.label..), sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(WP.DF$dLevelsAminusB ))) + 
      ylab("WP")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()
    
    p.3 <- ggplot(data = AE.DF  ,aes(x = FC_turb_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label = paste(after_stat(rr.label), after_stat(..p.label..), sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(AE.DF$dLevelsAminusB ))) +
      ylab("AE")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()
    
    final.plot <- ggpubr::ggarrange(p.1, p.3, p.2,
                                    ncol=1, nrow=3)
  }
  
  if( gas == 'H2O'){  
    
    AE.DF <- AE.DF %>% filter(gas == "H2O")
    WP.DF <- WP.DF %>% filter(gas == "H2O")
    MBR.DF <- MBR.DF %>% filter(gas == "H2O")
    
    p.1 <- ggplot(data = MBR.DF , aes(x = FH2O_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)) ) + 
      ylab("MBR") + xlim(-30, 30)+ ylim(-30, 30) + xlab("EC") +theme_bw()
    
    
    p.2 <- ggplot(data = WP.DF  , aes(x = FH2O_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(WP.DF$dLevelsAminusB ))) + 
      ylab("WP")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()
    
    p.3 <- ggplot(data = AE.DF  ,aes(x = FH2O_interp, y = FG_mean ))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), # adds R^2 and p-value
               r.accuracy = 0.01,
               p.accuracy = 0.001,
               label.x = -30, label.y = 30, size = 3) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -30, label.y = 20, size =3) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(AE.DF$dLevelsAminusB ))) + 
      ylab("AE")+ xlim(-30, 30)+ ylim(-30, 30) + xlab("EC")+theme_bw()
    
    final.plot <- ggpubr::ggarrange(p.1, p.3, p.2,
                                    ncol=1, nrow=3)
  }
  
  
  return(final.plot )
}

linear.parms <- function ( Y, X, DF, TYPE){
  
  if(sum(!is.na(DF[,X]+DF[,Y])) < 3){
    linear.parms <- data.frame( Intercept =as.numeric(NA) , 
                                Slope = as.numeric(NA),
                                R2= as.numeric(NA), RMSE= as.numeric(NA)) %>% 
      mutate( Approach = TYPE)
    return()
  }
  
  model <- lm(data =   DF, DF[,Y] ~ DF[, X])
  DF <- DF[,c(X, Y)] %>% na.omit
  predicted <- predict(model, DF)
  rmse <- sqrt(mean((DF[,Y] %>% na.omit - predicted)^2))
  
  linear.parms <- data.frame( Intercept =model$coefficients[1] , Slope = model$coefficients[2],
                              R2= summary(model)$r.squared, RMSE= rmse) %>% 
    mutate( Approach = TYPE)
}

one2one.parms <- function ( MBR.DF, AE.DF, WP.DF, gas){
  
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
                                    Approach = as.character() , 
                                    dLevelsAminusB = as.character() )
  
  dLevelsAll <- unique(c(unique( AE.DF$dLevelsAminusB),unique( WP.DF$dLevelsAminusB),unique( MBR.DF$dLevelsAminusB)))
  
  if(gas == 'CO2'){
    
    AE.DF <- AE.DF %>% filter(gas == "CO2")
    WP.DF <- WP.DF %>% filter(gas == "CO2")
    MBR.DF <- MBR.DF %>% filter(gas == "CO2")
    
    for ( a in dLevelsAll){
      print(a)
      
      if(any(a %in% MBR.DF$dLevelsAminusB)){
        linear.parms.mbr <- linear.parms( Y= 'FG_mean', 
                                          X='FC_turb_interp', 
                                          DF = MBR.DF %>% 
                                            filter(dLevelsAminusB == a),
                                          TYPE= 'MBR')
      } else {
        linear.parms.mbr <- NULL
      }
      
      if(any(a %in% AE.DF$dLevelsAminusB)){
        linear.parms.AE <- linear.parms( Y= 'FG_mean', 
                                         X='FC_turb_interp', 
                                         DF = AE.DF %>% 
                                           filter(dLevelsAminusB == a),
                                         TYPE= 'AE')
      } else {
        linear.parms.AE <- NULL
      }
      
      if(any(a %in% WP.DF$dLevelsAminusB)){
        linear.parms.WP <-linear.parms( Y= 'FG_mean', 
                                        X='FC_turb_interp', 
                                        DF = WP.DF %>% 
                                          filter(dLevelsAminusB == a),
                                        TYPE= 'WP')
      } else {
        linear.parms.WP <- NULL
      }  
      
      linear.parms.data <- rbind(linear.parms.mbr, linear.parms.AE, linear.parms.WP) %>% as.data.frame %>% mutate( dLevelsAminusB = a) 
      
      linear.parms.final <- rbind( linear.parms.final , linear.parms.data )
      
    }
    
    row.names(linear.parms.final) <- NULL
  }
  
  if(gas == 'H2O'){ 
    
    AE.DF <- AE.DF %>% filter(gas == "H2O")
    WP.DF <- WP.DF %>% filter(gas == "H2O")
    MBR.DF <- MBR.DF %>% filter(gas == "H2O")
    
    for ( a in dLevelsAll){
      print(a)
      
      if(any(a %in% MBR.DF$dLevelsAminusB)){
        linear.parms.mbr <- linear.parms( Y= 'FG_mean', 
                                          X='FH2O_interp', 
                                          DF = MBR.DF %>% 
                                            filter(dLevelsAminusB == a),
                                          TYPE= 'MBR')
      } else {
        linear.parms.MBR <- NULL
      }
      
      if(any(a %in% AE.DF$dLevelsAminusB)){
        linear.parms.AE <- linear.parms( Y= 'FG_mean', 
                                         X='FH2O_interp', 
                                         DF = AE.DF %>% 
                                           filter(dLevelsAminusB == a),
                                         TYPE= 'AE')
      } else {
        linear.parms.AE <- NULL
      }
      
      if(any(a %in% WP.DF$dLevelsAminusB)){
        linear.parms.WP <- linear.parms( Y= 'FG_mean', 
                                         X='FH2O_interp', 
                                         DF = WP.DF %>% 
                                           filter(dLevelsAminusB == a),
                                         TYPE= 'WP')
      } else {
        linear.parms.WP <- NULL
      }
      
      
      linear.parms.data <- rbind(linear.parms.mbr, linear.parms.AE, linear.parms.WP) %>% as.data.frame %>% mutate( dLevelsAminusB = a) 
      
      linear.parms.final <- rbind( linear.parms.final , linear.parms.data )
      
    }
    
    row.names(linear.parms.final) <- NULL
  }
  
  
  return(linear.parms.final)
}

one2one.parms.site <- function ( MBR.tibble, AE.tibble, WP.tibble, gas){
  
  sites <- names( MBR.tibble)
  
  site.tibble_parms <- data.frame( Intercept = as.numeric() ,
                                   Slope = as.numeric() ,
                                   R2 = as.numeric() ,
                                   RMSE = as.numeric() , 
                                   Approach = as.character() ,
                                   dLevelsAminusB = as.character(), 
                                   Site= as.character())
  
  for ( i in sites){
    print(i)
    
    df.parms <- one2one.parms( MBR.DF= MBR.tibble[i] , 
                               AE.DF = AE.tibble[i], 
                               WP.DF= WP.tibble[i], gas) %>%
      mutate( Site = i)
    
    site.tibble_parms  <- rbind( site.tibble_parms ,  df.parms)
  }
  
  return(site.tibble_parms ) 
}