#' flag.all.gas.stability
#'
#' @param flux.df dataframe of calculated fluxes and L is the name of the column with obukov length 
#' @param z canopy height as computed from ustar and wind speed at tower top, per Eqn. 9.7.1b in Stull 
#' @param d zero plane displacement height, calculated as 2/3 of z_veg_aero
#' @param L Obukov length
#' @return df with new column for stability conditions
#' 
#'
#' @author Sparkle Malone
#' 
#' 
flag.all.gas.stability <- function(flux.df, L, z, d){
  
  L = flux.df %>% select(all_of(L))
  z = flux.df %>% select(all_of(z))
  d = flux.df %>% select(all_of(d))
  
  flux.df$zeta = (z-d)/L
  
  #use obukov length to filter for stable, neutral, unstable atmospheric conditions
  #use 100m and 500m as threshold for comparison
  flux.df <- flux.df %>% mutate( Stability_100 = case_when( abs(L) > 100 ~ "neutral",
                                                            L < 100 & L > 0 ~ "stable", 
                                                            L > -100 & L < 0 ~  "unstable"),
                                 Stability_500 = case_when( abs(L) > 500 ~ "neutral",
                                                            L < 500 & L > 0 ~ "stable", 
                                                            L > -500 & L < 0 ~  "unstable"),
                                 Stability_Exteme = case_when( abs(zeta) > 1 ~ "extreme",
                                                               abs(zeta) <= 1 ~ "stable")) 
  
  flux.df$Stability_Exteme
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