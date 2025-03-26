
#'Perhaps make a plot with the y axis as different time intervals (30 min, hour,
#' 2 hourly, 4hourly,8 hourly,daily, 2 days, 4 days, Weekly, biweekly, monthly, 
#' 4 monthly), the x axis as different tower heights with a vertical canopy line,
#' and the color as the pvalue / RMSE / R2. make it a color scheme where
#' p < .05 becomes a different color
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)

###
averaging_periods <- c("30 min", "1 hour", "2 hours", "4 hours", "8 hours", "1 day", 
                       "2 days", "4 days", "1 week", "2 week", "1 month", "2 months","4 months")
site_date=site_data

# Function to compute p-value, R², and RMSE for H2O vs CO2 at each height difference and averaging interval
analyze_eddy_diff <- function(site_data, averaging_periods) {
  
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
  
  # Initialize results storage
  results <- data.frame()
  
  level = '2_1'
  
  # Loop through each height difference
  for (level in unique(merged_data_clean$dLevelsAminusB)) {
    
    subset_data <- merged_data_clean %>% filter(dLevelsAminusB == level)
    
    # Average data over different time intervals
    for (interval in averaging_periods) {
      
      interval = '2 days'
      # Create a new column 'interval_time' to round the timeMid to the nearest averaging period
      subset_data_interval <- subset_data %>%
        mutate(rounded_time = floor(as.numeric(timeMid) / (60 * as.numeric(strsplit(interval, " ")[[1]][1]))) * (60 * as.numeric(strsplit(interval, " ")[[1]][1]))) %>%
        group_by(rounded_time, dLevelsAminusB, dHeight) %>%
        summarise(EddyDiff_H2O_avg = mean(EddyDiff_H2O, na.rm = TRUE),
                  EddyDiff_CO2_avg = mean(EddyDiff_CO2, na.rm = TRUE)) %>%
        ungroup()
      
      # Linear regression on averaged data
      model <- lm(EddyDiff_H2O_avg ~ EddyDiff_CO2_avg, data = subset_data_interval)
      model_summary <- summary(model)
      p_value <- glance(model)$p.value
      r2_value <- model_summary$r.squared
      predicted <- predict(model, subset_data_interval)
      rmse_value <- sqrt(mean((subset_data_interval$EddyDiff_H2O_avg - predicted)^2))
      
      # Store results
      results <- rbind(results, data.frame(
        Averaging = interval,
        dLevelsAminusB = level,
        dHeight = unique(subset_data_interval$dHeight),
        P_value = p_value,
        R2 = r2_value,
        RMSE = rmse_value
      ))
    }
  }
  
  return(results)
}

# Function to process all sites and time intervals
process_all_sites <- function(dataset_list, averaging_periods) {
  
  all_results <- data.frame()
  
  for (site in names(dataset_list)) {
    site_data <- dataset_list[[site]]
    site_results <- analyze_eddy_diff(site_data, averaging_periods)
    site_results$Site <- site
    all_results <- rbind(all_results, site_results)
  }
  
  return(all_results)
}

# Function to create heatmap
plot_heatmap <- function(results_df) {
  
  # Ensure that Averaging is set up as a factor with levels ordered from highest frequency to lowest
  averaging_periods <- c("30 min", "1 hour", "2 hour", "4 hour", "8 hour",
                         "1 day", "2 days", "4 days", "1 week", "2 weeks",
                         "1 month", "2 months", "4 months")
  
  # Set the  as a factor with levels ordered from highest frequency to lowest
  results_df <- results_df %>% 
    mutate(Averaging = factor(Averaging, levels = rev(averaging_periods)))
  
  # Order dLevelsAminusB by increasing dHeight
  results_df <- results_df %>%
    arrange(dHeight) %>%
    mutate(dLevelsAminusB = factor(dLevelsAminusB, levels = unique(dLevelsAminusB)))
  
  # Create heatmap
  ggplot(results_df, aes(x = dLevelsAminusB, y = Averaging, fill = RMSE)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("darkgreen", "red"), limits = c(.3, .9)) +
    labs(title = "RMSE Heatmap: Eddy Diffusivity (H2O vs CO2)",
         x = "Height Levels (Ordered by dHeight)",
         y = "Averaging Period",
         fill = "P-value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Example usage:
# Assuming SITES_MBR_9min, SITES_AE_9min, etc., are available
# dataset_list <- list(HARV = SITES_MBR_9min$HARV, GUAN = SITES_MBR_9min$GUAN)
# results <- process_all_sites(dataset_list)
# plot_heatmap(results)




site_data <- SITES_AE_9min$HARV



#####Now I will filter so that it is only non cross grad and within 0 and 4 for eddy diff

site_data <- site_data %>% filter(cross_grad_flag == 0)
site_data <- site_data %>% filter(Kgas < 4)
site_data <- site_data %>% filter(Kgas > 0)

hist(site_data$Kgas,
     xlim = c(0,4), breaks =100)



plot()











# Define the averaging periods (higher frequency to lower frequency)
averaging_periods <- c("30 min", "1 hour", "2 hour", "4 hour", "8 hour", "1 day", 
                       "2 days", "4 days", "1 week", "2 week", "1 month", "2 months","4 months")



# Wrap the site data into a list (can be extended to multiple sites)
dataset_list <- list(Site1 = site_data)

# Process the dataset list
all_results <- process_all_sites(dataset_list, averaging_periods)

# View the first few rows of the results
head(all_results)



# Remove rows with NA in dHeight
all_results_clean <- all_results %>%
  filter(!is.na(dHeight))


# Plot the heatmap
plot_heatmap(all_results_clean)



df_h2o <- site_data %>% filter(gas == "H2O")
df_co2 <- site_data %>% filter(gas == "CO2")

merged_df <- merge(df_h2o[, c("Kgas", "timeMid","dLevelsAminusB")], df_co2[, c("Kgas", "timeMid","dLevelsAminusB")], by = "timeMid", all = TRUE)

merged_df <- merged_df %>% filter(dLevelsAminusB.y == "2_1")


plot(merged_df$Kgas.x,merged_df$Kgas.y, xlim = c(0,.2),ylim = c(0,.2))



#'perhaps plot Mean absolute error MAE instead (RSME), or just slope, since we
#'just want to see how close it is to a 1:1 line under certain conditions 

