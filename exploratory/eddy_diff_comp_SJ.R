####Get Residuals
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)


percent_difference <- function(A, B) {
  abs_diff <- abs(A - B)                # Absolute difference
  avg <- (A + B) / 2                    # Average of the two values
  percent_diff <- (abs_diff / avg) * 100 # Percent difference
  return(percent_diff)
}

###Data###
site_data <- SITES_AE_9min$JORN

#####Now I will filter so that it is only non cross grad and within 0 and 4 for eddy diff

site_data <- site_data %>% filter(cross_grad_flag == 0)
site_data <- site_data %>% filter(Kgas < 4)
site_data <- site_data %>% filter(Kgas > 0)

hist(site_data$Kgas,
     xlim = c(0,4), breaks =100)



# Extract only H2O and CO2 data, and merge them by 'timeMid'
H2O_data <- site_data %>%
  filter(gas == "H2O") %>%
  select(timeMid, EddyDiff_H2O = Kgas, dLevelsAminusB, dHeight)

CO2_data <- site_data %>%
  filter(gas == "CO2") %>%
  select(timeMid, EddyDiff_CO2 = Kgas, dLevelsAminusB, dHeight)

# Merge the H2O and CO2 data on 'timeMid'
merged_data <- H2O_data %>%
  left_join(CO2_data, by = c("timeMid", "dLevelsAminusB", "dHeight"))

# Remove rows where either H2O or CO2 is NA
merged_data_clean <- merged_data %>%
  drop_na(EddyDiff_H2O, EddyDiff_CO2)


merged_data_clean$res <- merged_data_clean$EddyDiff_H2O -  merged_data_clean$EddyDiff_CO2 

merged_data_clean$perc_diff <- percent_difference(merged_data_clean$EddyDiff_H2O,
                                                  merged_data_clean$EddyDiff_CO2)


hist(merged_data_clean$res, breaks = 100)

hist(merged_data_clean$perc_diff, breaks = 100)

mean_res <- mean(merged_data_clean$res)
mean_abs_diff <- mean(abs(merged_data_clean$res))
mean_perc_diff <- mean(merged_data_clean$perc_diff)

print("Mean Residual:")
print(mean_res)
print("Mean Absolute Difference:")
print(mean_abs_diff)
print("Mean Percent Difference:")
print(mean_perc_diff)



#####Seasonal Variations of Residual####

# Extract day of the year (ignoring the year)
merged_data_clean$day_of_year <- format(merged_data_clean$timeMid, "%j")  # "%j" gives the day of the year

# Group by day of the year and calculate the average of `res` for each day
yearly_pattern <- merged_data_clean %>%
  group_by(day_of_year) %>%
  summarise(avg_res = mean(res, na.rm = TRUE))

# Plot the average yearly pattern
ggplot(yearly_pattern, aes(x = as.numeric(day_of_year), y = avg_res)) +
  geom_line() +
  labs(title = "Average Yearly Pattern of 'res'", x = "Day of the Year", y = "Average 'res'") +
  theme_minimal()





#####Diurnal Variations of Residual#####


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming `timeMid` is of class DateTime and `res` is the value to plot
# Convert timeMid to POSIXct if it is not already
merged_data_clean$timeMid <- as.POSIXct(merged_data_clean$timeMid)

# Extract hour of the day
merged_data_clean$hour <- format(merged_data_clean$timeMid, "%H")

# Group by hour and calculate the average of `res` for each hour
diurnal_pattern <- merged_data_clean %>%
  group_by(hour) %>%
  summarise(avg_res = mean(res, na.rm = TRUE))

# Plot the diurnal pattern
ggplot(diurnal_pattern, aes(x = as.numeric(hour), y = avg_res)) +
  geom_line() +
  labs(title = "Diurnal Pattern of 'res'", x = "Hour of the Day", y = "Average 'res'") +
  theme_minimal()



#####RMSE at each height, at each site###
# Load necessary libraries
library(dplyr)
library(tidyr)

# Function to calculate RMSE between H2O and CO2 Kgas
calculate_rmse <- function(df) {
  sqrt(mean((df$Kgas_H2O - df$Kgas_CO2)^2, na.rm = TRUE))
}

# Assuming SITES_AE_9min is your list containing the data frames GUAN, HARV, KONZ, JORN
# Loop through each site data frame in the list and calculate RMSE
results <- lapply(names(SITES_AE_9min), function(site_name) {
  
  # Get the data frame for this site
  site_df <- SITES_AE_9min[[site_name]]
  
  # Apply the filters
  site_df <- site_df %>% 
    filter(cross_grad_flag == 0) %>%  # Filter for cross_grad_flag == 0
    filter(Kgas < 4) %>%             # Filter for Kgas less than 4
    filter(Kgas > 0)                 # Filter for Kgas greater than 0
  
  # Ensure 'gas' is a character column, in case it's factors
  site_df$gas <- as.character(site_df$gas)
  
  # Separate H2O and CO2 data
  H2O_data <- site_df %>% filter(gas == "H2O") %>% select(timeMid, dLevelsAminusB, Kgas, dHeight)
  CO2_data <- site_df %>% filter(gas == "CO2") %>% select(timeMid, dLevelsAminusB, Kgas)
  
  # Merge H2O and CO2 data by timeMid and dLevelsAminusB
  merged_data <- left_join(H2O_data, CO2_data, by = c("timeMid", "dLevelsAminusB"), suffix = c("_H2O", "_CO2"))
  
  # Initialize an empty list to store results
  rmse_results <- list()
  
  # Get unique dLevelsAminusB values
  levels <- unique(merged_data$dLevelsAminusB)
  
  # Loop through each dLevelsAminusB level
  for (level in levels) {
    # Filter data for this level
    level_data <- merged_data %>% filter(dLevelsAminusB == level)
    
    # Calculate RMSE for this level
    rmse_value <- calculate_rmse(level_data)
    
    # Get the associated dHeight for this level (assuming all dHeight values are the same for a given dLevelsAminusB)
    dHeight_value <- level_data$dHeight[1]  # This should be consistent for each level
    
    # Calculate Relative RMSE to Mean
    mean_value <- mean(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE)
    relative_rmse_mean <- (rmse_value / mean_value) * 100
    
    # Calculate Relative RMSE to Range
    range_value <- max(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE) - min(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE)
    relative_rmse_range <- (rmse_value / range_value) * 100
    
    # Calculate Relative RMSE to Median
    median_value <- median(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE)
    relative_rmse_median <- (rmse_value / median_value) * 100
    
    # Calculate the number of observations (n) for this level
    n_obs <- nrow(level_data)
    
    # Calculate the standard deviation for combined Kgas (H2O and CO2) for this level
    combined_kgas <- c(level_data$Kgas_H2O, level_data$Kgas_CO2)
    std_dev <- sd(combined_kgas, na.rm = TRUE)
    
    # Store the result in the list
    rmse_results[[level]] <- data.frame(
      dLevelsAminusB = level,
      RMSE = rmse_value,
      relative_rmse_mean = relative_rmse_mean,
      relative_rmse_range = relative_rmse_range,
      relative_rmse_median = relative_rmse_median,  # Include relative RMSE to median
      dHeight = dHeight_value,  # Include dHeight
      n = n_obs,                # Include the number of observations (n)
      std_dev = std_dev,        # Include the standard deviation (std_dev)
      site = site_name
    )
  }
  
  # Combine all results for this site into a data frame
  site_rmse_data <- bind_rows(rmse_results)
  
  return(site_rmse_data)
})

# Combine all sites' results into one data frame
final_result_height <- bind_rows(results)

# View the final result
print(final_result_height)




############Relative Med and Mean RMSE by stability##################
# Load necessary libraries
library(dplyr)

# Function to calculate RMSE between H2O and CO2 Kgas
calculate_rmse <- function(df) {
  sqrt(mean((df$Kgas_H2O - df$Kgas_CO2)^2, na.rm = TRUE))
}

# Create a function to calculate RMSE for each stability level at each site, with relative RMSE to mean and median
calculate_rmse_by_stability <- function(site_data) {
  # Apply the filters to the data
  site_data <- site_data %>% 
    filter(cross_grad_flag == 0) %>%  # Filter for cross_grad_flag == 0
    filter(Kgas < 4) %>%             # Filter for Kgas less than 4
    filter(Kgas > 0)                 # Filter for Kgas greater than 0
  
  # Ensure 'gas' is a character column (to avoid factor issues)
  site_data$gas <- as.character(site_data$gas)
  
  # Separate H2O and CO2 data
  H2O_data <- site_data %>% filter(gas == "H2O") %>% select(timeMid, Stability_500, Kgas, site)
  CO2_data <- site_data %>% filter(gas == "CO2") %>% select(timeMid, Stability_500, Kgas)
  
  # Merge H2O and CO2 data by timeMid and Stability_500
  merged_data <- left_join(H2O_data, CO2_data, by = c("timeMid", "Stability_500"), suffix = c("_H2O", "_CO2"))
  
  # Initialize an empty list to store results
  stability_rmse_results <- list()
  
  # Get unique stability levels
  stability_levels <- unique(merged_data$Stability_500)
  
  # Loop through each stability level
  for (level in stability_levels) {
    # Filter data for this stability level
    level_data <- merged_data %>% filter(Stability_500 == level)
    
    # Calculate RMSE for this level
    rmse_value <- calculate_rmse(level_data)
    
    # Calculate Relative RMSE to Mean
    mean_value <- mean(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE)
    relative_rmse_mean <- (rmse_value / mean_value) * 100
    
    # Calculate Relative RMSE to Median
    median_value <- median(c(level_data$Kgas_H2O, level_data$Kgas_CO2), na.rm = TRUE)
    relative_rmse_median <- (rmse_value / median_value) * 100
    
    # Calculate the number of observations (n) for this stability level
    n_obs <- nrow(level_data)
    
    # Calculate the standard deviation for combined Kgas (H2O and CO2) for this level
    combined_kgas <- c(level_data$Kgas_H2O, level_data$Kgas_CO2)
    std_dev <- sd(combined_kgas, na.rm = TRUE)
    
    # Store the result in the list
    stability_rmse_results[[level]] <- data.frame(
      Stability_500 = level,
      RMSE = rmse_value,
      relative_rmse_mean = relative_rmse_mean,
      relative_rmse_median = relative_rmse_median,
      n = n_obs,
      std_dev = std_dev,
      site = site_data$site[1]  # Assuming the site is consistent across the dataset
    )
  }
  
  # Combine all results for this site into a data frame
  site_rmse_data <- bind_rows(stability_rmse_results)
  
  return(site_rmse_data)
}

# Apply the function to each site in SITES_AE_9min
results <- lapply(names(SITES_AE_9min), function(site_name) {
  # Get the data frame for this site
  site_df <- SITES_AE_9min[[site_name]]
  
  # Calculate RMSE by stability for this site
  site_rmse_data <- calculate_rmse_by_stability(site_df)
  
  return(site_rmse_data)
})

# Combine all sites' results into one data frame
final_result_stability <- bind_rows(results)

# View the final result
print(final_result_stability)


#####PLOT for height and level#####
# Load required libraries
library(ggplot2)

# Assuming your dataframe is called 'df'
ggplot(final_result_height, aes(x = dHeight, y = relative_rmse_median, label = dLevelsAminusB, color = site)) +
  geom_text(fontface = "bold") +                             # Make text bold
  scale_color_manual(values = c("HARV" = "#018571", "GUAN" = "#80cdc1", "KONZ" = "#dfc27d", "JORN"="#a6611a")) +  # Customize colors for the site variable
  labs(
    x = "dHeight [m]", 
    y = "Relative RMSE Median [%]", 
    title = "Plot of Relative RMSE Median vs dHeight by Site"
  ) +                                         # Title and axis labels
  theme_minimal() +                           # Minimal theme for a clean look
  theme(legend.position = "right")            # Position the legend on the right



