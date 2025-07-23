bhatt.coeff.df <- function(df , df.filter){
  
  VPD.1 = df %>% select(VPD) %>% na.omit
  VPD.2 = df.filter %>% select(VPD) %>% na.omit 
  
  Tair_K.1 = df %>% select(Tair_K) %>% na.omit
  Tair_K.2 = df.filter %>% select(Tair_K) %>% na.omit 
  
  PAR.1 = df %>% select(PAR) %>% na.omit
  PAR.2 = df.filter %>% select(PAR) %>% na.omit 
 
    summary <- data.frame( 
      bhatt.coe.PAR = bhatt.coeff(x = PAR.1[,1], y = PAR.2[,1]),
      bhatt.coe.Tair_K = bhatt.coeff(x =  Tair_K.1[,1], y = Tair_K.2[,1]),
      bhatt.coe.VPD = bhatt.coeff(x = VPD.1[,1], y = VPD.2[,1]),
      hellinger.PAR = statip::hellinger(x = PAR.1[,1], y = PAR.2[,1], -Inf, Inf),
      hellinger.Tair_K = statip::hellinger(x = Tair_K.1[,1], y = Tair_K.2[,1], -Inf, Inf),
      hellinger.VPD = statip::hellinger(x =  VPD.1[,1], y = VPD.2[,1], -Inf, Inf))
   

  
  summary.final <-  summary %>% mutate(  bhatt.PAR = -log(bhatt.coe.PAR),
                                                         bhatt.Tair_K = -log(bhatt.coe.Tair_K),
                                                         bhatt.VPD = -log(bhatt.coe.VPD ) ) %>% na.omit
  
  return(  summary.final)
}