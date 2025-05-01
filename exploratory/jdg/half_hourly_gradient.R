library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)  # For viridis color scales

# Assuming data_file is already loaded
 data_list <- min9.FG.WP.list
 gas <- "H2O"  # or "H2O" based on what you want to plot
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

# Extract time components and calculate stats - taking means across years
process_diel_data <- function(data) {
  data %>%
    # Ensure timestamp is POSIXct
    mutate(
      hour_of_day = hour(timestamp) + minute(timestamp)/60,
      month = month(timestamp),
      month_name = factor(month.abb[month], levels = month.abb)
    ) %>%
    # Group by month and hour, ignoring year to take means across years
    group_by(month_name, hour_of_day, height) %>%
    summarize(
      mean_concentration = mean(concentration, na.rm = TRUE),
      sd_concentration = sd(concentration, na.rm = TRUE),
      n = n(),
      se_concentration = sd_concentration / sqrt(n),
      .groups = "drop"
    )
}

# Get the unique heights
unique_heights <- extract_heights_data(data_file)
print(unique_heights)

# Get all concentration data
all_concentration_data <- gather_concentration_data(data_file)

# Process for diel patterns, taking means across years
diel_data <- process_diel_data(all_concentration_data)

# Create the plot with viridis color scale
ggplot(diel_data, aes(x = hour_of_day, y = mean_concentration, color = factor(height), group = height)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_concentration - se_concentration, 
                  ymax = mean_concentration + se_concentration,
                  fill = factor(height)), alpha = 0.2, color = NA) +
  facet_wrap(~ month_name, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 24, by = 6), 
                     labels = c("0:00", "6:00", "12:00", "18:00", "24:00"),
                     limits = c(0, 24)) +
  scale_color_viridis_d(name = "Height (m)", option = "viridis") +  # Using discrete viridis scale for colors
  scale_fill_viridis_d(name = "Height (m)", option = "viridis") +   # Same for fill
  labs(
    title = paste("Mean Diel Concentration Pattern by Height"),
    subtitle = paste("Faceted by Month, with Standard Error Bands"),
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