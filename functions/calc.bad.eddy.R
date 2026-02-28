

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


######Super Bad Eddy######
"Super_Bad_eddy takes a dataframe (df), an eddy diffusivity (method) to analyze (calculated from either AE or WP method) and specified bound (x). We then compare the K
calculated by the AE or WP method against the eddy diffusivity back calculated from the eddy covariance method (kgas). If kgas is different from the 
eddy diffusivity calcuated from AE and WP, we flag it as an outlier with the assumption that the difference was caused by conditions unsuitable for the 
gradient method. The argument x determines what the boundaries of what is interpreted as unsuitable are. Argument x could possibly be defined as +- a 
certain percentage of a each EC kgas observation, or in the case of the Bad_Eddy function, if kgas is within 1.5IQR of the range calculated by AE and WP 
for each site."



Super_Bad_Eddy <- function(df, method, x) {
  # Access the data
  site_data <- df
  
  # Check if the specified method is valid
  if (!(method %in% c("EddyDiff", "EddyDiff_WP"))) {
    stop("Invalid method. Please choose either 'EddyDiff' or 'EddyDiff_WP'.")
  }
  
  # Select the column based on the method
  method_column <- site_data[[method]]
  kgas <- site_data$Kgas
  
  # Calculate the lower and upper bounds for outliers based on x
  lower_bound <- method_column - x
  upper_bound <- method_column + x
  
  # Create the s_eddy_outliers column based on the outlier condition
  site_data$s_eddy_outliers <- ifelse(kgas < lower_bound | kgas > upper_bound, 1, 0)
  
  # Calculate the percentage of data flagged as outliers
  percent_flagged <- mean(site_data$s_eddy_outliers, na.rm=TRUE) * 100
  
  # Print the message on how much data was flagged
  cat("Percentage of data flagged as s_eddy_outliers:", percent_flagged, "%\n")
  
  # Return the updated dataframe with the new 's_eddy_outliers' column
  return(site_data)
}



