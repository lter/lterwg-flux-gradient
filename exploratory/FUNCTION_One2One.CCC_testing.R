# Source the CCC calculation function
source(fs::path(DirRepo,"./functions/calc.lins.ccc.R"))

# Define get_facet_stats as a global function
get_facet_stats <- function(df, facet_level, x_col, y_col) {
  subset_df <- df %>% filter(dLevelsAminusB == facet_level)
  
  # Skip if insufficient data
  if(nrow(subset_df) < 10) {
    return("Insufficient data")
  }
  
  # Calculate CCC
  ccc_result <- tryCatch({
    calculate_lins_ccc(subset_df[[x_col]], subset_df[[y_col]])$rho.c$est
  }, error = function(e) NA)
  
  # Calculate R² and RMSE
  model <- tryCatch({
    lm(as.formula(paste(y_col, "~", x_col)), data = subset_df)
  }, error = function(e) NULL)
  
  if(!is.null(model)) {
    r2_val <- summary(model)$r.squared
    rmse_val <- sqrt(mean((subset_df[[y_col]] - predict(model))^2, na.rm = TRUE))
  } else {
    r2_val <- NA
    rmse_val <- NA
  }
  
  # Return formatted statistics
  return(sprintf("CCC = %.2f\nR² = %.2f\nRMSE = %.2f", 
                 ccc_result, r2_val, rmse_val))
}

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
    
    # Custom labeller function for facets
    mbr_labels <- sapply(unique(MBR.DF$dLevelsAminusB), function(level) {
      get_facet_stats(MBR.DF, level, "FC_turb_interp", "FG_mean")
    })
    
    ae_labels <- sapply(unique(AE.DF$dLevelsAminusB), function(level) {
      get_facet_stats(AE.DF, level, "FC_turb_interp", "FG_mean")
    })
    
    wp_labels <- sapply(unique(WP.DF$dLevelsAminusB), function(level) {
      get_facet_stats(WP.DF, level, "FC_turb_interp", "FG_mean")
    })
    
    # Create the plots with custom labels
    p.1 <- ggplot(data = MBR.DF, aes(x = FC_turb_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("MBR") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    
    
    
    p.2 <- ggplot(data = WP.DF, aes(x = FC_turb_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("WP") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.3 <- ggplot(data = AE.DF, aes(x = FC_turb_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("AE") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    final.plot <- ggpubr::ggarrange(p.1, p.3, p.2,
                                    ncol=1, nrow=3)
  }
  
  if(gas == 'H2O') {  
    
    AE.DF <- AE.DF %>% filter(gas == "H2O")
    WP.DF <- WP.DF %>% filter(gas == "H2O")
    MBR.DF <- MBR.DF %>% filter(gas == "H2O")
    
    # Custom labeller function for facets - H2O
    mbr_labels <- sapply(unique(MBR.DF$dLevelsAminusB), function(level) {
      get_facet_stats(MBR.DF, level, "FH2O_interp", "FG_mean")
    })
    
    ae_labels <- sapply(unique(AE.DF$dLevelsAminusB), function(level) {
      get_facet_stats(AE.DF, level, "FH2O_interp", "FG_mean")
    })
    
    wp_labels <- sapply(unique(WP.DF$dLevelsAminusB), function(level) {
      get_facet_stats(WP.DF, level, "FH2O_interp", "FG_mean")
    })
    
    p.1 <- ggplot(data = MBR.DF, aes(x = FH2O_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("MBR") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.2 <- ggplot(data = WP.DF, aes(x = FH2O_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
      ylab("WP") + xlim(-30, 30) + ylim(-30, 30) + xlab("EC") + theme_bw()
    
    p.3 <- ggplot(data = AE.DF, aes(x = FH2O_interp, y = FG_mean)) + 
      stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
      geom_point(alpha=0.1) +
      facet_wrap(~ dLevelsAminusB, ncol = length(unique(MBR.DF$dLevelsAminusB)),
                 labeller = function(variable, value) {
                   if(variable == "dLevelsAminusB") {
                     return(mbr_labels[as.character(value)])
                   }
                   return(value)
                 }) +
      geom_abline(intercept = 0, slope = 1, col = 'grey50', linetype="dashed") + 
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
                            R2= as.numeric(NA),
                            count = as.numeric(NA)) %>% 
      mutate(Approach = TYPE)
    return(ccc.parms)
  }
  
  # Calculate linear model parameters (same as in linear.parms)
  model <- lm(data = DF, DF[,Y] ~ DF[, X])
  DF <- DF[,c(X, Y)] %>% na.omit
  predicted <- predict(model, DF)
  rmse <- sqrt(mean((DF[,Y] %>% na.omit - predicted)^2))
  count <- DF[,Y] %>% na.omit() %>% length()
  # Calculate CCC
  ccc_result <- calculate_lins_ccc(DF[,X], DF[,Y])
  
  ccc.parms <- data.frame(Intercept = model$coefficients[1], 
                          Slope = model$coefficients[2],
                          CCC = ccc_result$rho.c$est, 
                          RMSE = rmse,
                          R2= summary(model)$r.squared,
                          count= count) %>% 
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
                                R2 = as.numeric(),
                                Approach = as.character(), 
                                dLevelsAminusB = as.character(),
                                count=as.numeric())
  
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
                                  R2 = as.numeric(),
                                  Approach = as.character(),
                                  dLevelsAminusB = as.character(), 
                                  Site = as.character(),
                                  count = as.numeric())
  
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

