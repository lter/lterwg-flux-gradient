#' flag.all.gas.stability
#'
#' @param flux.df dataframe of calculated fluxes and L is the name of the column with obukov length 
#' @param z not used for zeta any more (kept so existing calls still work); was the aerodynamic canopy height
#' @param d zero plane displacement height column (0.66 h)
#' @param L Obukov length column
#' @param zA,zB columns with the heights of the two inlets of the pair
#' zeta is the stability parameter of each sampling-height pair, (z - d)/L with z = sqrt(zA zB) and
#' z - d >= 0.1 m, as used for phi_h in calc.stability.correction(). It was previously (h - d)/L, which
#' is the same for every pair at a given time.
#' @return df with new column for stability conditions
#' 
#'
#' @author Sparkle Malone
#' 
#' 
flag.all.gas.stability <- function(flux.df, L, z = NULL, d, zA = "TowerHeight_A", zB = "TowerHeight_B"){
  
  L.val <- as.numeric(flux.df[[L]])
  d.val <- as.numeric(flux.df[[d]])
  z.pair <- sqrt(as.numeric(flux.df[[zA]]) * as.numeric(flux.df[[zB]]))
  
  zeta <- pmax(z.pair - d.val, 0.1) / L.val
  
  #use obukov length to filter for stable, neutral, unstable atmospheric conditions
  #use 100m and 500m as threshold for comparison
 flux.df$zeta = zeta
 flux.df$L = L.val
  flux.df <- flux.df %>% mutate( Stability_100 = case_when( abs(L) > 100 ~ "neutral",
                                                            L < 100 & L > 0 ~ "stable", 
                                                            L > -100 & L < 0 ~  "unstable"),
                                 Stability_500 = case_when( abs(L) > 500 ~ "neutral",
                                                            L < 500 & L > 0 ~ "stable", 
                                                            L > -500 & L < 0 ~  "unstable"),
                                 Stability_Exteme = case_when( abs(zeta) > 1 ~ "extreme",
                                          abs(zeta) <= 1 ~ "stable"))                            
  
 
  #calculate amount of data in each condition
  percent.neutral100 <- round(length(flux.df[which(flux.df$Stability_100=="neutral"), 1])/length(flux.df[,1]), 3)*100
  percent.stable100 <- round(length(flux.df[which(flux.df$Stability_100=="stable"),1])/length(flux.df[,1]), 3)*100
  percent.unstable100 <- round(length(flux.df[which(flux.df$Stability_100=="unstable"), 1])/length(flux.df[,1]), 3)*100
  
  percent.Stability_Exteme <- round(length(flux.df[which(flux.df$Stability_Exteme=="stable"), 1])/length(flux.df[,1]), 3)*100
  
  print(paste0("Amount of data under neutral conditions is ~", percent.neutral100, "% using 100m threshold"))
  print(paste0("Amount of data under stable conditions is ~", percent.stable100, "% using 100m threshold"))
  print(paste0("Amount of data under unstable conditions is ~", percent.unstable100, "% using 100m threshold"))
  
  percent.neutral500 <- round(length(flux.df[which(flux.df$Stability_500=="neutral"),1])/length(flux.df[,1]), 3)*100
  percent.stable500 <- round(length(flux.df[which(flux.df$Stability_500=="stable"),1])/length(flux.df[,1]), 3)*100
  percent.unstable500 <- round(length(flux.df[which(flux.df$Stability_500=="unstable"),1])/length(flux.df[,1]), 3)*100
  print(paste0("Amount of data under neutral conditions is ~", percent.neutral100, "% using 500m threshold"))
  print(paste0("Amount of data under stable conditions is ~", percent.stable100, "% using 500m threshold"))
  print(paste0("Amount of data under unstable conditions is ~", percent.unstable100, "% using 500m threshold"))
  
  print(paste0("Amount of data under stable conditions is ~", percent.Stability_Exteme , "% using extreme zeta cutoffs from cantero et al., 2022"))
  return(flux.df)
  
}