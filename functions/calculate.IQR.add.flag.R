#' calculate.IQR.add.flag
#'
#' @param gas.df data frame of specific gas CO2, CH4, H2O with FG column
#'
#' @return data frame with IQR.flag column
#' 
#'
#' @author Alexis Helgeson
calculate.IQR.add.flag <- function(gas.df){
  #calculate IQR range based on specified upper and lower bounds
  #remember the FG column is the calculated flux
  Q3 <- quantile(gas.df$FG, c(0.75))[[1]]
  Q1 <- quantile(gas.df$FG, c(0.25))[[1]]
  IQR <- (as.numeric(Q3) - as.numeric(Q1))*3 #filter for outliers use 1.5; for extreme outliers use 3
  upper.threshold <- as.numeric(Q3) + as.numeric(IQR)
  lower.threshold <- as.numeric(Q1) - as.numeric(IQR)
  #add flag for values that fall outside of IQR range; use NEON convention 1 = bad data, 0 = good data
  gas.df[which(gas.df$FG < lower.threshold),"IQR.flag"] <- "1"
  gas.df[which(gas.df$FG > upper.threshold),"IQR.flag"] <- "1"
  gas.df[which(gas.df$FG >= lower.threshold & gas.df$FG <= upper.threshold),"IQR.flag"] <- "0"
  
  return(gas.df)
}