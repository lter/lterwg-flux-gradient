#' IQR.Outlier.Filter
#'
#' @param site df of calculated flux from specific site taken from Validation df
#'
#' @return df with outliers removed
#' 
#' @author Alexis Helgeson
#'
IQR.outlier.filter <- function(site){
  #IQR Filtering for CO2
  #filter df for desired gas
  site.CO2 <- site %>% filter(gas == "CO2")
  #calculate IQR range based on specified upper and lower bounds
  #remember the FG column is the calculated flux
  Q3 <- quantile(site.CO2$FG, c(0.75), na.rm = T)[[1]]
  Q1 <- quantile(site.CO2$FG, c(0.25), na.rm = T)[[1]]
  IQR <- (as.numeric(Q3) - as.numeric(Q1))*3 #filter for outliers use 1.5; for extreme outliers use 3
  upper.threshold <- as.numeric(Q3) + as.numeric(IQR)
  lower.threshold <- as.numeric(Q1) - as.numeric(IQR)
  #add flag for values that fall outside of IQR range; use NEON convention 1 = bad data, 0 = good data
  site.CO2[which(site.CO2$FG < lower.threshold),"IQR.flag"] <- "1"
  site.CO2[which(site.CO2$FG > upper.threshold),"IQR.flag"] <- "1"
  site.CO2[which(site.CO2$FG >= lower.threshold & site.CO2$FG <= upper.threshold),"IQR.flag"] <- "0"
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.CO2[which(site.CO2$IQR.flag=="0"),"FG"])/length(site.CO2[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for CO2"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  #site.CO2$FG_IQR <- site.CO2$FG
  #site.CO2[which(site.CO2$IQR.flag=="1"), "FG_IQR"] <- NA
  
  #IQR Filtering for H20
  #filter df for desired gas
  site.H20 <- site %>% filter(gas == "H2O")
  #calculate IQR range based on specified upper and lower bounds
  #remember the FG column is the calculated flux
  Q3 <- quantile(site.H20$FG, c(0.75), na.rm = T)[[1]]
  Q1 <- quantile(site.H20$FG, c(0.25), na.rm = T)[[1]]
  IQR <- (as.numeric(Q3) - as.numeric(Q1))*3 #filter for outliers use 1.5; for extreme outliers use 3
  upper.threshold <- as.numeric(Q3) + as.numeric(IQR)
  lower.threshold <- as.numeric(Q1) - as.numeric(IQR)
  #add flag for values that fall outside of IQR range; use NEON convention 1 = bad data, 0 = good data
  site.H20[which(site.H20$FG < lower.threshold),"IQR.flag"] <- "1"
  site.H20[which(site.H20$FG > upper.threshold),"IQR.flag"] <- "1"
  site.H20[which(site.H20$FG >= lower.threshold & site.H20$FG <= upper.threshold),"IQR.flag"] <- "0"
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.H20[which(site.H20$IQR.flag=="0"),"FG"])/length(site.H20[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for H20"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  # site.H20$FG_IQR <- site.H20$FG
  # site.H20[which(site.H20$IQR.flag=="1"), "FG_IQR"] <- NA
  
  #IQR Filtering for CH4
  #filter df for desired gas
  site.CH4 <- site %>% filter(gas == "CH4")
  #calculate IQR range based on specified upper and lower bounds
  #remember the FG column is the calculated flux
  Q3 <- quantile(site.CH4$FG, c(0.75), na.rm=T)[[1]]
  Q1 <- quantile(site.CH4$FG, c(0.25), na.rm=T)[[1]]
  IQR <- (as.numeric(Q3) - as.numeric(Q1))*3 #filter for outliers use 1.5; for extreme outliers use 3
  upper.threshold <- as.numeric(Q3) + as.numeric(IQR)
  lower.threshold <- as.numeric(Q1) - as.numeric(IQR)
  #add flag for values that fall outside of IQR range; use NEON convention 1 = bad data, 0 = good data
  site.CH4[which(site.CH4$FG < lower.threshold),"IQR.flag"] <- "1"
  site.CH4[which(site.CH4$FG > upper.threshold),"IQR.flag"] <- "1"
  site.CH4[which(site.CH4$FG >= lower.threshold & site.CH4$FG <= upper.threshold),"IQR.flag"] <- "0"
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.CH4[which(site.CH4$IQR.flag=="0"),"FG"])/length(site.CH4[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for CH4"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  # site.CH4$FG_IQR <- site.CH4$FG
  # site.CH4[which(site.CH4$IQR.flag=="1"), "FG_IQR"] <- NA
  
  site.outlier <- rbind(site.CO2, site.H20, site.CH4)
  
  return(site.outlier)
}