#' StabilityFilter
#'
#' @param site df of calculated flux from specific site taken from Validation df
#'
#' @return df with new column for stability conditions
#' 
#'
#' @author Alexis Helgeson
#' 
stability.filter <- function(site){
  #use obukov length to filter for stable, neutral, unstable atmospheric conditions
  #use 100m and 500m as threshold for comparison
  site[which(abs(site$L) > 100),"Stability_100"] <- "neutral"
  site[which(site$L < 100 & site$L > 0), "Stability_100"] <- "stable"
  site[which(site$L > -100 & site$L < 0), "Stability_100"] <- "unstable"
  
  site[which(abs(site$L) > 500),"Stability_500"] <- "neutral"
  site[which(site$L < 500 & site$L > 0), "Stability_500"] <- "stable"
  site[which(site$L > -500 & site$L < 0), "Stability_500"] <- "unstable"
  #calculate amount of data in each condition
  percent.neutral100 <- round(length(site[which(site$Stability_100=="neutral"),"FG"])/length(site[,"FG"]), 3)*100
  percent.stable100 <- round(length(site[which(site$Stability_100=="stable"),"FG"])/length(site[,"FG"]), 3)*100
  percent.unstable100 <- round(length(site[which(site$Stability_100=="unstable"),"FG"])/length(site[,"FG"]), 3)*100
  print(paste0("Amount of data under neutral conditions is ~", percent.neutral100, "% using 100m threshold"))
  print(paste0("Amount of data under stable conditions is ~", percent.stable100, "% using 100m threshold"))
  print(paste0("Amount of data under unstable conditions is ~", percent.unstable100, "% using 100m threshold"))
  
  percent.neutral500 <- round(length(site[which(site$Stability_500=="neutral"),"FG"])/length(site[,"FG"]), 3)*100
  percent.stable500 <- round(length(site[which(site$Stability_500=="stable"),"FG"])/length(site[,"FG"]), 3)*100
  percent.unstable500 <- round(length(site[which(site$Stability_500=="unstable"),"FG"])/length(site[,"FG"]), 3)*100
  print(paste0("Amount of data under neutral conditions is ~", percent.neutral100, "% using 500m threshold"))
  print(paste0("Amount of data under stable conditions is ~", percent.stable100, "% using 500m threshold"))
  print(paste0("Amount of data under unstable conditions is ~", percent.unstable100, "% using 500m threshold"))
  
  return(site)
  
}