library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(mgcv)  # For gam with sinusoidal terms

# Assuming data_file is already loaded
 data_list <- min9.FG.WP.list
 gas <- "CO2"  # or "H2O" based on what you want to plot
 data_file <- data_list[[gas]]

# Extract all unique measurement heights and their corresponding positions
extract_heights_data <- function(data) {
  # Extract heights from both A and B measurements
  heights_A <- data %>% 
    select(TowerPosition_A, TowerHeight_A) %>%
    rename(
      tower_position = TowerPosition_A,
      height = TowerHeight_A
    ) %>%
    distinct(tower_position, height)
  
  heights_B <- data %>% 
    select(TowerPosition_B, TowerHeight_B) %>%
    rename(
      tower_position = TowerPosition_B,
      height = TowerHeight_B
    ) %>%
    distinct(tower_position, height)
  
  # Combine and get unique combinations
  unique_heights <- bind_rows(heights_A, heights_B) %>%
    distinct(tower_position, height) %>%
    mutate(height = as.numeric(height)) %>%
    arrange(height)
  
  return(unique_heights)
}

# Gather all concentration data for each height with timestamp
gather_concentration_data <- function(data) {
  # Get data for A positions
  data_A <- data %>%
    select(timeEnd_A, TowerPosition_A, TowerHeight_A, mean_A) %>%
    rename(
      timestamp = timeEnd_A,
      tower_position = TowerPosition_A,
      height = TowerHeight_A,
      concentration = mean_A
    )
  
  # Get data for B positions
  data_B <- data %>%
    select(timeEnd_B, TowerPosition_B, TowerHeight_B, mean_B) %>%
    rename(
      timestamp = timeEnd_B,
      tower_position = TowerPosition_B,
      height = TowerHeight_B,
      concentration = mean_B
    )
  
  # Combine and convert height to numeric
  all_data <- bind_rows(data_A, data_B) %>%
    mutate(height = as.numeric(height))
  
  return(all_data)
}

# Extract time components and prepare for sinusoidal fitting
prepare_diel_data <- function(data) {
  data %>%
    # Ensure timestamp is POSIXct
    mutate(
      hour_of_day = hour(timestamp) + minute(timestamp)/60,
      # Convert hour to radians for sinusoidal model (1 day = 2π)
      hour_rad = hour_of_day * 2 * pi / 24,
      month = month(timestamp),
      month_name = factor(month.abb[month], levels = month.abb)
    )
}

# Get the unique heights
unique_heights <- extract_heights_data(data_file)
print(unique_heights)

# Get all concentration data
all_concentration_data <- gather_concentration_data(data_file)

# Prepare data for sinusoidal fitting
prepared_data <- prepare_diel_data(all_concentration_data)

# Function to fit sinusoidal model for each month and height
fit_sinusoidal_models <- function(data) {
  # Group data by month and height
  results <- data %>%
    group_by(month_name, height) %>%
    # Use a sinusoidal model with 24-hour periodicity
    # We'll use both sine and cosine terms to capture phase shifts
    do({
      # Fit sinusoidal model
      model <- lm(concentration ~ sin(hour_rad) + cos(hour_rad), data = .)
      
      # Create prediction data
      pred_hours <- seq(0, 24, by = 0.1)
      pred_data <- data.frame(
        hour_of_day = pred_hours,
        hour_rad = pred_hours * 2 * pi / 24
      )
      
      # Make predictions
      pred_data$fit <- predict(model, newdata = pred_data)
      
      # Calculate SE
      pred_mat <- cbind(1, sin(pred_data$hour_rad), cos(pred_data$hour_rad))
      pvar <- pred_mat %*% vcov(model) %*% t(pred_mat)
      pred_data$se <- sqrt(diag(pvar))
      
      # Add confidence intervals
      pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
      pred_data$upper <- pred_data$fit + 1.96 * pred_data$se
      
      pred_data
    }) %>%
    ungroup()
  
  return(results)
}

# Fit sinusoidal models
sinusoidal_fits <- fit_sinusoidal_models(prepared_data)

# Plot sinusoidal fits
ggplot(sinusoidal_fits, aes(x = hour_of_day, y = fit, color = factor(height), group = height)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(height)), alpha = 0.2, color = NA) +
  facet_wrap(~ month_name, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), 
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 24)) +
  scale_color_viridis_d(name = "Height (m)", option = "viridis") +
  scale_fill_viridis_d(name = "Height (m)", option = "viridis", guide = "none") +
  labs(
    title = paste("Sinusoidal Diel Concentration Pattern by Height"),
    subtitle = paste("Faceted by Month, with 95% Confidence Bands"),
    x = "Hour of Day",
    y = "Concentration"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

# Alternative: Use a more complex harmonic model with 2 harmonics
# This allows capturing both 24-hour and 12-hour cycles
fit_harmonic_models <- function(data) {
  # Group data by month and height
  results <- data %>%
    group_by(month_name, height) %>%
    do({
      # Fit harmonic model with 2 harmonics
      model <- lm(concentration ~ sin(hour_rad) + cos(hour_rad) + 
                    sin(2*hour_rad) + cos(2*hour_rad), data = .)
      
      # Create prediction data
      pred_hours <- seq(0, 24, by = 0.1)
      pred_data <- data.frame(
        hour_of_day = pred_hours,
        hour_rad = pred_hours * 2 * pi / 24
      )
      
      # Make predictions
      pred_data$fit <- predict(model, newdata = pred_data)
      
      # Calculate SE
      pred_mat <- cbind(1, sin(pred_data$hour_rad), cos(pred_data$hour_rad), 
                        sin(2*pred_data$hour_rad), cos(2*pred_data$hour_rad))
      pvar <- pred_mat %*% vcov(model) %*% t(pred_mat)
      pred_data$se <- sqrt(diag(pvar))
      
      # Add confidence intervals
      pred_data$lower <- pred_data$fit - 1.96 * pred_data$se
      pred_data$upper <- pred_data$fit + 1.96 * pred_data$se
      
      pred_data
    }) %>%
    ungroup()
  
  return(results)
}

# Fit harmonic models
harmonic_fits <- fit_harmonic_models(prepared_data)

# Plot harmonic fits
ggplot(harmonic_fits, aes(x = hour_of_day, y = fit, color = factor(height), group = height)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(height)), alpha = 0.2, color = NA) +
  facet_wrap(~ month_name, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), 
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 24)) +
  scale_color_viridis_d(name = "Height (m)", option = "viridis") +
  scale_fill_viridis_d(name = "Height (m)", option = "viridis", guide = "none") +
  labs(
    title = paste("Harmonic Diel Concentration Pattern by Height"),
    subtitle = paste("Using 24-hour and 12-hour harmonics, with 95% Confidence Bands"),
    x = "Hour of Day",
    y = "Concentration"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )