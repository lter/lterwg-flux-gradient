#' Comp_Function
#'
#' @param obs_data Observational Data (vector)
#' @param derv_data Derived Data (vector)
#' @param IQR Inter-Quartile Range, numeric 1.5 for outlier removal, 3 for extreme outliers
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Sam Jurado


Flux_Comparison <- function(obs_data,derv_data,IQR){
  
  diff <- as.numeric(obs_data) - as.numeric(derv_data)
  
  diff_df <- data.frame(diff)
  #number of points that fall outside of x number of standard deviations
  
  
  #makes this an IQR test instead
  q1 <- quantile(diff_df$diff,prob=c(.25), type = 1 , na.rm =TRUE)
  q3 <- quantile(diff_df$diff, prob=c(.75), type=1, na.rm=TRUE)
  
  outliers = (q3-q1)*IQR
  floor = q1 - outliers
  ceiling = q3 + outliers
  
  diff_df$diff <- replace(diff_df$diff, diff_df$diff < floor , NA)
  diff_df$diff <- replace(diff_df$diff, diff_df$diff > ceiling , NA)
  
  percent_diff <- (diff_df$diff)/as.numeric(df$obs_data) *100
  
  
  diff_df$percent_diff <- percent_diff
  
  
  q1 <- quantile(diff_df$percent_diff,prob=c(.25), type = 1 , na.rm =TRUE)
  q3 <- quantile(diff_df$percent_diff, prob=c(.75), type=1, na.rm=TRUE)
  
  outliers = (q3-q1)*IQR
  floor = q1 - outliers
  ceiling = q3 + outliers
  
  diff_df$percent_diff <- replace(diff_df$percent_diff, diff_df$percent_diff < floor , NA)
  diff_df$percent_diff <- replace(diff_df$percent_diff, diff_df$percent_diff > ceiling , NA)
  
  return(data.frame(diff_df))
}
