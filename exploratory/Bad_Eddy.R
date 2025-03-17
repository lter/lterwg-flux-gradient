
Bad_Eddy <- function(df, method) {
  # Access the data based on the site and method
  site_data <- df
  
  # Check if the specified method is valid
  if (!(method %in% c("EddyDiff", "EddyDiff_WP"))) {
    stop("Invalid method. Please choose either 'EddyDiff' or 'EddyDiff_WP'.")
  }
  
  # Select the column based on the method
  method_column <- site_data[[method]]
  kgas <- site_data$Kgas
  
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(method_column, 0.25, na.rm=TRUE)
  Q3 <- quantile(method_column, 0.75, na.rm=TRUE)
  IQR_value <- IQR(method_column, na.rm=TRUE)
  
  # Calculate the lower and upper bounds for outliers
  lower_bound <- 0  # Since you're checking for values >= 0 (for EddyDiff)
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Create the Eddy_outlier column based on the outlier condition
  site_data$Eddy_outlier <- ifelse(kgas < lower_bound | kgas > upper_bound, 1, 0)
  
  # Calculate the percentage of data flagged as outliers
  percent_flagged <- mean(site_data$Eddy_outlier, na.rm=TRUE) * 100
  
  # Print the message on how much data was lost
  cat("Percentage of data flagged as outliers:", percent_flagged, "%\n")
  
  # Return the updated dataframe with the new 'Eddy_outlier' column
  return(site_data)
}

#updated_data <- Bad_Eddy(AE_9min.df, "EddyDiff")


