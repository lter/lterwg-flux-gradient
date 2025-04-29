# Function to produce plots and calculations using Lin's Concordance Correlation Coefficient:

# Source the CCC calculation function
source(fs::path(DirRepo,"./functions/calc.lins.ccc.R"))

library(ggpubr)
# CCC PLOTS: ####

ccc.plots <- function(MBR.DF, AE.DF, WP.DF, gas) {
  
  MBR.DF <- MBR.DF %>% as.data.frame
  #names(MBR.DF) <- substring(names(MBR.DF), 6)
  
  AE.DF <- AE.DF %>% as.data.frame
  #names(AE.DF) <- substring(names(AE.DF), 6) 
  
  WP.DF <- WP.DF %>% as.data.frame
  #names(WP.DF) <- substring(names(WP.DF), 6)
  
  if(gas == 'CO2') {
    
    AE.DF <- AE.DF %>% filter(gas == "CO2")
    WP.DF <- WP.DF %>% filter(gas == "CO2")
    MBR.DF <- MBR.DF %>% filter(gas == "CO2")
    
    p.1 <- ggplot(data = MBR.DF, aes(x = FC_turb_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB))) +
      # Add CCC annotation using annotate instead of stat_cor
      annotate("text", x = -20, y = 20, size = 5, 
               label = MBR.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FC_turb_interp, FG_mean)$rho.c$est %>% round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("MBR") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.2 <- ggplot(data = WP.DF, aes(x = FC_turb_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      # Add CCC annotation
      annotate("text", x = -20, y = 30, size = 5, 
               label = WP.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FC_turb_interp, FG_mean)$rho.c$est %>% 
                             round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(WP.DF$dLevelsAminusB))) + 
      ylab("WP") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.3 <- ggplot(data = AE.DF, aes(x = FC_turb_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      # Add CCC annotation
      annotate("text", x = -20, y = 30, size = 5, 
               label = AE.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FC_turb_interp, FG_mean)$rho.c$est %>% 
                             round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(AE.DF$dLevelsAminusB))) +
      ylab("AE") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    final.plot <- ggpubr::ggarrange(p.1, p.3, p.2,
                                    ncol=1, nrow=3)
  }
  
  if(gas == 'H2O') {  
    
    AE.DF <- AE.DF %>% filter(gas == "H2O")
    WP.DF <- WP.DF %>% filter(gas == "H2O")
    MBR.DF <- MBR.DF %>% filter(gas == "H2O")
    
    p.1 <- ggplot(data = MBR.DF, aes(x = FH2O_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      # Add CCC annotation
      annotate("text", x = -20, y = 30, size = 5, 
               label = MBR.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FH2O_interp, FG_mean)$rho.c$est %>% 
                             round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      facet_wrap(~dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB))) + 
      ylab("MBR") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.2 <- ggplot(data = WP.DF, aes(x = FH2O_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      # Add CCC annotation
      annotate("text", x = -20, y = 30, size = 5, 
               label = WP.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FH2O_interp, FG_mean)$rho.c$est %>% 
                             round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(WP.DF$dLevelsAminusB))) + 
      ylab("WP") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.3 <- ggplot(data = AE.DF, aes(x = FH2O_interp, y = FG_mean))  + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      # Add CCC annotation
      annotate("text", x = -20, y = 30, size = 5, 
               label = AE.DF %>% 
                 group_by(dLevelsAminusB) %>% 
                 summarize(ccc = calculate_lins_ccc(FH2O_interp, FG_mean)$rho.c$est %>% 
                             round(2) %>% 
                             paste("CCC =", .)) %>% 
                 pull(ccc) %>% 
                 paste(collapse = "\n")) +
      stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                            label.x = -20, label.y = 20, size =1) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(AE.DF$dLevelsAminusB))) + 
      ylab("AE") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    final.plot <- ggpubr::ggarrange(p.1, p.3, p.2,
                                    ncol=1, nrow=3)
  }
  
  return(final.plot)
}

ccc.parms <- function(Y, X, DF, TYPE) {
  
  if(sum(!is.na(DF[,X]+DF[,Y])) < 3) {
    ccc.parms <- data.frame(Intercept = as.numeric(NA), 
                            Slope = as.numeric(NA),
                            CCC = as.numeric(NA), 
                            RMSE = as.numeric(NA),
                            R2= as.numeric(NA)) %>% 
      mutate(Approach = TYPE)
    return(ccc.parms)
  }
  
  # Calculate linear model parameters (same as in linear.parms)
  model <- lm(data = DF, DF[,Y] ~ DF[, X])
  DF <- DF[,c(X, Y)] %>% na.omit
  predicted <- predict(model, DF)
  rmse <- sqrt(mean((DF[,Y] %>% na.omit - predicted)^2))
  
  # Calculate CCC
  ccc_result <- calculate_lins_ccc(DF[,X], DF[,Y])
  
  ccc.parms <- data.frame(Intercept = model$coefficients[1], 
                          Slope = model$coefficients[2],
                          CCC = ccc_result$rho.c$est, 
                          RMSE = rmse,
                          R2= summary(model)$r.squared) %>% 
    mutate(Approach = TYPE)
  
  return(ccc.parms)
}

ccc.parms.height <- function(MBR.DF, AE.DF, WP.DF, gas) {
  
  MBR.DF <- MBR.DF %>% as.data.frame
  names(MBR.DF) <- substring(names(MBR.DF), 6)
  
  AE.DF <- AE.DF %>% as.data.frame
  names(AE.DF) <- substring(names(AE.DF), 6)
  
  WP.DF <- WP.DF %>% as.data.frame
  names(WP.DF) <- substring(names(WP.DF), 6)
  
  ccc.parms.final <- data.frame(Intercept = as.numeric(),
                                Slope = as.numeric(),
                                CCC = as.numeric(),
                                RMSE = as.numeric(), 
                                Approach = as.character(), 
                                dLevelsAminusB = as.character())
  
  dLevelsAll <- unique(c(unique(AE.DF$dLevelsAminusB), unique(WP.DF$dLevelsAminusB), unique(MBR.DF$dLevelsAminusB)))
  
  if(gas == 'CO2') {
    
    AE.DF <- AE.DF %>% filter(gas == "CO2")
    WP.DF <- WP.DF %>% filter(gas == "CO2")
    MBR.DF <- MBR.DF %>% filter(gas == "CO2")
    
    for(a in dLevelsAll) {
      print(a)
      
      if(any(a %in% MBR.DF$dLevelsAminusB)) {
        ccc.parms.mbr <- ccc.parms(Y = 'FG_mean', 
                                   X = 'FC_turb_interp', 
                                   DF = MBR.DF %>% 
                                     filter(dLevelsAminusB == a),
                                   TYPE = 'MBR')
      } else {
        ccc.parms.mbr <- NULL
      }
      
      if(any(a %in% AE.DF$dLevelsAminusB)) {
        ccc.parms.AE <- ccc.parms(Y = 'FG_mean', 
                                  X = 'FC_turb_interp', 
                                  DF = AE.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE = 'AE')
      } else {
        ccc.parms.AE <- NULL
      }
      
      if(any(a %in% WP.DF$dLevelsAminusB)) {
        ccc.parms.WP <- ccc.parms(Y = 'FG_mean', 
                                  X = 'FC_turb_interp', 
                                  DF = WP.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE = 'WP')
      } else {
        ccc.parms.WP <- NULL
      }  
      
      ccc.parms.data <- rbind(ccc.parms.mbr, ccc.parms.AE, ccc.parms.WP) %>% 
        as.data.frame %>% 
        mutate(dLevelsAminusB = a) 
      
      ccc.parms.final <- rbind(ccc.parms.final, ccc.parms.data)
    }
    
    row.names(ccc.parms.final) <- NULL
  }
  
  if(gas == 'H2O') { 
    
    AE.DF <- AE.DF %>% filter(gas == "H2O")
    WP.DF <- WP.DF %>% filter(gas == "H2O")
    MBR.DF <- MBR.DF %>% filter(gas == "H2O")
    
    for(a in dLevelsAll) {
      print(a)
      
      if(any(a %in% MBR.DF$dLevelsAminusB)) {
        ccc.parms.mbr <- ccc.parms(Y = 'FG_mean', 
                                   X = 'FH2O_interp', 
                                   DF = MBR.DF %>% 
                                     filter(dLevelsAminusB == a),
                                   TYPE = 'MBR')
      } else {
        ccc.parms.mbr <- NULL
      }
      
      if(any(a %in% AE.DF$dLevelsAminusB)) {
        ccc.parms.AE <- ccc.parms(Y = 'FG_mean', 
                                  X = 'FH2O_interp', 
                                  DF = AE.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE = 'AE')
      } else {
        ccc.parms.AE <- NULL
      }
      
      if(any(a %in% WP.DF$dLevelsAminusB)) {
        ccc.parms.WP <- ccc.parms(Y = 'FG_mean', 
                                  X = 'FH2O_interp', 
                                  DF = WP.DF %>% 
                                    filter(dLevelsAminusB == a),
                                  TYPE = 'WP')
      } else {
        ccc.parms.WP <- NULL
      }
      
      ccc.parms.data <- rbind(ccc.parms.mbr, ccc.parms.AE, ccc.parms.WP) %>% 
        as.data.frame %>% 
        mutate(dLevelsAminusB = a) 
      
      ccc.parms.final <- rbind(ccc.parms.final, ccc.parms.data)
    }
    
    row.names(ccc.parms.final) <- NULL
  }
  
  return(ccc.parms.final)
}

ccc.parms.site <- function(MBR.tibble, AE.tibble, WP.tibble, gas) {
  
  sites <- names(MBR.tibble)
  
  site.tibble_parms <- data.frame(Intercept = as.numeric(),
                                  Slope = as.numeric(),
                                  CCC = as.numeric(),
                                  RMSE = as.numeric(), 
                                  Approach = as.character(),
                                  dLevelsAminusB = as.character(), 
                                  Site = as.character())
  
  for(i in sites) {
    print(i)
    
    df.parms <- ccc.parms.height(MBR.DF = MBR.tibble[i], 
                                 AE.DF = AE.tibble[i], 
                                 WP.DF = WP.tibble[i], gas) %>%
      mutate(Site = i)
    
    site.tibble_parms <- rbind(site.tibble_parms, df.parms)
  }
  
  return(site.tibble_parms) 
}

