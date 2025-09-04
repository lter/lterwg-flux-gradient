bhatt.coeff.df <- function(df , df.filter){
  
  VPD.1 = df %>% select(VPD) %>% na.omit
  VPD.2 = df.filter %>% select(VPD) %>% na.omit 
  
  Tair_K.1 = df %>% select(Tair_K) %>% na.omit
  Tair_K.2 = df.filter %>% select(Tair_K) %>% na.omit 
  
  PAR.1 = df %>% select(PAR) %>% na.omit
  PAR.2 = df.filter %>% select(PAR) %>% na.omit 
 
    summary <- data.frame( 
      Bhatt.coe.PAR = bhatt.coeff(x = PAR.1[,1], y = PAR.2[,1]),
      Bhatt.coe.Tair_K = bhatt.coeff(x =  Tair_K.1[,1], y = Tair_K.2[,1]),
      Bhatt.coe.VPD = bhatt.coeff(x = VPD.1[,1], y = VPD.2[,1]),
      Hellinger.PAR = statip::hellinger(x = PAR.1[,1], y = PAR.2[,1], -Inf, Inf),
      Hellinger.Tair_K = statip::hellinger(x = Tair_K.1[,1], y = Tair_K.2[,1], -Inf, Inf),
      Hellinger.VPD = statip::hellinger(x =  VPD.1[,1], y = VPD.2[,1], -Inf, Inf),
      KS.PAR = stats::ks.test(x = PAR.1[,1], y = PAR.2[,1], alternative = c("two.sided"))$statistic,
      KS.Tair_K = stats::ks.test(x = Tair_K.1[,1], y = Tair_K.2[,1], alternative = c("two.sided"))$statistic,
      KS.VPD = stats::ks.test(x =  VPD.1[,1], y = VPD.2[,1],alternative = c("two.sided"))$statistic,
      KS.P.PAR = stats::ks.test(x = PAR.1[,1], y = PAR.2[,1], alternative = c("two.sided"))$p.value,
      KS.P.Tair_K = stats::ks.test(x = Tair_K.1[,1], y = Tair_K.2[,1], alternative = c("two.sided"))$p.value,
      KS.P.VPD = stats::ks.test(x =  VPD.1[,1], y = VPD.2[,1],alternative = c("two.sided"))$p.value)

  summary.final <-  summary %>% mutate(  bhatt.PAR = -log(Bhatt.coe.PAR),
                                                         Bhatt.Tair_K = -log(Bhatt.coe.Tair_K),
                                                         Bhatt.VPD = -log(Bhatt.coe.VPD ) ) %>% na.omit
  
  return(  summary.final)
}