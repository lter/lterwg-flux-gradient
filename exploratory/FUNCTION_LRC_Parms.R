library(brms) 
library(cmdstanr)
library(ggplot2)
library(beepr)
library(tidybayes)
library(tidyverse)
library(ggpubr)

# Example of priors: 

priors.lrc <-  prior(normal(-0.01, 0.02), nlpar = "a1", lb=-1, ub= 0) +
prior(normal( -1 ,  2), nlpar = "ax", lb=-30, ub= -0.01) +
prior(normal(1, 2), nlpar = "r", lb=0, ub= 30)


LRC_PARMS <- function( df, iterations, priors.lrc, idx, PAR, nee){
 
  data.frame <- df %>% as.data.frame
  #names(data.frame) <- substring( names(  data.frame), 6)

  
  data.frame$PAR <- data.frame[,PAR] %>% as.numeric
  data.frame$idx <- data.frame[,idx] %>% as.character
  data.frame$nee <-data.frame[,nee] %>% as.numeric
 
  data.frame.f <- data.frame %>% select(idx, PAR, nee, TowerH, gas) %>% filter( PAR > 0 & gas =="CO2")
  
  equation <- nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) - r
  
  # PARM Dataframe:
  parms <- data.frame(idx = as.character(), 
                      a1.mean = as.numeric(), 
                      a1.se = as.numeric(),
                      a1.Bulk_ESS= as.numeric() ,
                      a1.Tail_ESS= as.numeric() ,
                      a1.Rhat= as.numeric(),
                      
                      ax.mean = as.numeric(),
                      ax.se = as.numeric(),
                      ax.Bulk_ESS= as.numeric() ,
                      ax.Tail_ESS= as.numeric() ,
                      ax.Rhat= as.numeric(),
                      
                      r.mean = as.numeric(),
                      r.se= as.numeric(),
                      r.Bulk_ESS= as.numeric() ,
                      r.Tail_ESS= as.numeric() ,
                      r.Rhat= as.numeric(),
                      samples.lrc= as.numeric(),
                      TowerH = as.character())
  
  for ( a in unique(data.frame.f$TowerH)) {
   
     print(a)
    
    df <- data.frame.f %>% filter(TowerH == a) %>% na.omit()
  
    for ( i in unique(df$idx) ){
      print(i) 
      
      # Subset the file:
      df.n <- df %>% filter(idx == i) %>% na.omit()
      
      #priors.lrc <- get_prior(bf(equation, a1+ax+r ~ 1, nl=TRUE),
                             # data = df.n, family = poisson())
      
      try(model.brms <- brm( bf( equation, a1+ax+r ~ 1, nl=TRUE),
                             prior = priors.lrc , data = df.n, iter = iterations, cores =3, chains = 1, backend = "cmdstanr"), silent= T)
      
      try(model.brms.df <- summary(model.brms)$fixed , silent = T)
      
      try(model.brms.df.a1 <- model.brms.df %>% filter( row.names(model.brms.df) == 'a1_Intercept'), silent = T)
      try( model.brms.df.ax <- model.brms.df %>% filter( row.names(model.brms.df) == 'ax_Intercept'), silent = T)
      try(model.brms.df.r <- model.brms.df %>% filter( row.names(model.brms.df) == 'r_Intercept'), silent = T)
      
      samples <-  df %>% filter(idx == i)%>% select(nee)  %>% na.omit %>% nrow
      baseline <- as.Date(paste(i, '-01', sep="")) %>% lubridate::days_in_month() *48 %>% as.numeric
      
      
     try( results <- data.frame( idx = i, 
                                 a1.mean = model.brms.df.a1$Estimate ,
                                 a1.se = model.brms.df.a1$Est.Error ,
                                 a1.Bulk_ESS = model.brms.df.a1$Bulk_ESS , 
                                 a1.Tail_ESS = model.brms.df.a1$Tail_ESS ,
                                 a1.Rhat = model.brms.df.a1$Rhat ,
                                 
                                 ax.mean = model.brms.df.ax$Estimate ,
                                 ax.se = model.brms.df.ax$Est.Error ,
                                 ax.Bulk_ESS = model.brms.df.ax$Bulk_ESS , 
                                 ax.Tail_ESS = model.brms.df.ax$Tail_ESS ,
                                 ax.Rhat = model.brms.df.ax$Rhat ,
                                 
                                 r.mean = model.brms.df.r$Estimate ,
                                 r.se = model.brms.df.r$Est.Error ,
                                 r.Bulk_ESS = model.brms.df.r$Bulk_ESS , 
                                 r.Tail_ESS = model.brms.df.r$Tail_ESS ,
                                 r.Rhat = model.brms.df.r$Rhat,
                                 samples.lrc= samples/baseline *100 ,
                                 TowerH = a), silent=T)
      
      rm(model.brms.df.a1, model.brms.df.ax, model.brms.df.r )
      
      try(parms <- parms %>% rbind(results), silent = T)

    }
    }
  
  return(parms ) 
}


priors.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)


TRC_PARMS <- function( df, iterations, priors.trc, idx, nee, TA, PAR){
  
  data.frame <- df %>% as.data.frame
  #names(data.frame) <- substring( names(  data.frame), 6)
  
  
  data.frame$TA <- data.frame[, TA] %>% as.numeric
  data.frame$idx <- data.frame[,idx] %>% as.character
  data.frame$nee <-data.frame[,nee] %>% as.numeric
  data.frame$PAR <-data.frame[,PAR] %>% as.numeric
  
  data.frame.f <- data.frame %>% select(idx, PAR, nee, TA, TowerH, gas) %>% filter( PAR < 50, gas == "CO2")
  
  equation = nee ~ a * exp(b*TA)
  
  # PARM Dataframe:
  try(parms <- data.frame(idx=as.character(), 
                          a.mean = as.numeric(), 
                          a.se = as.numeric(),
                          a.Bulk_ESS= as.numeric() ,
                          a.Tail_ESS= as.numeric() ,
                          a.Rhat= as.numeric(),
                          
                          b.mean = as.numeric(),
                          b.se = as.numeric(),
                          b.Bulk_ESS= as.numeric() ,
                          b.Tail_ESS= as.numeric() ,
                          b.Rhat= as.numeric(),
                          samples.trc= as.numeric(),
                          TowerH = as.character()), silent = T)
  for ( a in unique(data.frame.f$TowerH)) {
    print(a) 
    
    df <- data.frame.f %>% filter(TowerH == a) %>% na.omit()
    
    for ( i in unique(df$idx)){
      print(i)
      
      # Subset the file:
      try( df.trc <- df %>% filter(idx == i), silent= T)
      # get priors:
      
     #priors.trc <- get_prior(bf(equation, a+b ~ 1, nl=TRUE), 
                              #data = df.trc, family = poisson())
                          
      try(model.brms <-  brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                              prior = priors.trc , data = df.trc, 
                              backend = "cmdstanr", iter = iterations, cores =4, seed=101), silent= T)
      
      
      try(model.brms.df <- summary(model.brms)$fixed , silent= T)
      
      try(model.brms.df.a <- model.brms.df %>% filter( row.names(model.brms.df) == 'a_Intercept'), silent= T)
      try(model.brms.df.b <- model.brms.df %>% filter( row.names(model.brms.df) == 'b_Intercept'), silent= T)
      
      try(samples <- df.trc %>% filter(idx == i)%>% select(nee)  %>% na.omit %>% nrow , silent= T)
      try(baseline <- as.Date(paste(i, '-01', sep="")) %>% lubridate::days_in_month() *48 %>% as.numeric, silent= T)
      
      
      try(results <- data.frame( idx = i, 
                                 a.mean = model.brms.df.a$Estimate ,
                                 a.se = model.brms.df.a$Est.Error ,
                                 a.Bulk_ESS = model.brms.df.a$Bulk_ESS , 
                                 a.Tail_ESS = model.brms.df.a$Tail_ESS ,
                                 a.Rhat = model.brms.df.a$Rhat ,
                                 
                                 b.mean = model.brms.df.b$Estimate ,
                                 b.se = model.brms.df.b$Est.Error ,
                                 b.Bulk_ESS = model.brms.df.b$Bulk_ESS , 
                                 b.Tail_ESS = model.brms.df.b$Tail_ESS ,
                                 b.Rhat = model.brms.df.b$Rhat,
                                 samples.trc= samples/baseline *100,
                                 TowerH = a), silent= T)
      
      try( parms <- parms %>% rbind(results), silent= T)
      
      rm(results,  model.brms.df)
    }
    
 }
  return(parms ) }
  
  
PARMS_Sites <- function(sites.tibble,
                        iterations, 
                        priors.lrc, 
                        priors.trc, 
                        idx,
                        PAR, 
                        nee,
                        TA) {
  
  site.tibble.parms <- list()
  
  sites <- sites.tibble %>% names
  
  for( site in  sites){
    
    message(paste("Working with", site))
    
   
    try( parms.lrc <- LRC_PARMS( df = sites.tibble[[site]], iterations = iterations, priors= priors.lrc, idx = idx , PAR = PAR, nee = nee ), silent =T)
    
    message("Done with LRC")
    
    try(parms.trc <- TRC_PARMS( df = sites.tibble[[site]], 
                        iterations = iterations, 
                        priors= priors.trc, 
                        idx = idx , 
                        TA = TA,
                        PAR = PAR,
                        nee = nee ), silent =T)
    
    parameters <- parms.lrc %>% full_join( parms.trc, by= c('idx', 'TowerH'))
    
    message("Done with TRC")
    
    site.tibble.parms[[site]] <-  list(parameters)
    message(paste("Done with",site))
    
  }
  return( site.tibble.parms)
}




