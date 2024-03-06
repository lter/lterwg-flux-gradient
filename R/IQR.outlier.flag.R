#' IQR.outlier.filter
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
  #add IQR.flag to data frame
  site.CO2 <- calculate.IQR.add.flag(gas.df = site.CO2)
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.CO2[which(site.CO2$IQR.flag=="0"),"FG"])/length(site.CO2[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for CO2"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  #site.CO2$FG_IQR <- site.CO2$FG
  #site.CO2[which(site.CO2$IQR.flag=="1"), "FG_IQR"] <- NA
  
  #IQR Filtering for H20
  #filter df for desired gas
  site.H20 <- site %>% filter(gas == "H2O")
  #add IQR.flag to data frame
  site.H20 <- calculate.IQR.add.flag(gas.df = site.H20)
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.H20[which(site.H20$IQR.flag=="0"),"FG"])/length(site.H20[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for H20"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  # site.H20$FG_IQR <- site.H20$FG
  # site.H20[which(site.H20$IQR.flag=="1"), "FG_IQR"] <- NA
  
  #IQR Filtering for CH4
  #filter df for desired gas
  site.CH4 <- site %>% filter(gas == "CH4")
  #add IQR.flag to data frame
  site.CH4 <- calculate.IQR.add.flag(gas.df = site.CH4)
  #calculate how much data would be remain after IQR
  percent.good.data <- round(length(site.CH4[which(site.CH4$IQR.flag=="0"),"FG"])/length(site.CH4[,"FG"]),3)*100 #good data/total data
  print(paste0("After filtering for extreme outliers there is ~", percent.good.data, "% good data remaining for CH4"))
  #duplicate FG column and set values IQR.flag == 1 to NA
  # site.CH4$FG_IQR <- site.CH4$FG
  # site.CH4[which(site.CH4$IQR.flag=="1"), "FG_IQR"] <- NA
  
  site.outlier <- bind_rows(site.CO2, site.H20, site.CH4)
  
  return(site.outlier)
}