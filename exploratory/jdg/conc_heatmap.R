library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)

# Assuming data_file is already loaded
 data_list <- min9.FG.WP.list
 gas <- "H2O"  # or "H2O" based on what you want to plot
 data_file <- data_list[[gas]]
 data_file<-data_file%>%filter(dConc_pvalue<=0.01)

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

# Convert to EST timezone and prepare for half-hourly binning
prepare_half_hourly_data <- function(data) {
  data %>%
    # Convert timestamps to EST
    mutate(
      timestamp_est = with_tz(timestamp, tzone = "America/New_York"),
      # Extract components for grouping
      month = month(timestamp_est),
      month_name = factor(month.abb[month], levels = month.abb),
      # Round to nearest half hour for binning
      half_hour = floor(hour(timestamp_est) * 2 + minute(timestamp_est) / 30) / 2,
      # Create a nice label for half hours
      half_hour_label = sprintf("%02d:%02d", floor(half_hour), 
                                ifelse(half_hour %% 1 == 0, 0, 30))
    )
}

# Get the unique heights
unique_heights <- extract_heights_data(data_file)
print(unique_heights)

# Get all concentration data
all_concentration_data <- gather_concentration_data(data_file)

# Prepare data with EST timezone and half-hourly bins
half_hourly_data <- prepare_half_hourly_data(all_concentration_data)

# Calculate half-hourly means for each month and height
half_hourly_means <- half_hourly_data %>%
  group_by(month_name, half_hour, half_hour_label, height) %>%
  summarize(
    mean_concentration = mean(concentration, na.rm = TRUE),
    sd_concentration = sd(concentration, na.rm = TRUE),
    n = n(),
    se_concentration = sd_concentration / sqrt(n),
    .groups = "drop"
  )

# Filter for May-September months only
summer_months <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
summer_data <- half_hourly_means %>%
  filter(month_name %in% summer_months) %>%
  # Reorder month_name to ensure chronological order
  mutate(month_name = factor(month_name, levels = summer_months))

# Plot heatmap for May-September only
ggplot(summer_data, aes(x = half_hour, y = (height), fill = (mean_concentration))) +
  geom_tile() +
  facet_wrap(~ month_name, ncol = 5) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), 
                     labels = c("00:00", "03:00", "06:00", "09:00", "12:00", 
                                "15:00", "18:00", "21:00", "00:00"),
                     expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Mean\nConcentration", option = "viridis") +
  labs(
    title = paste("Half-Hourly Mean Concentration by Height (EST)"),
    subtitle = paste("Growing Season (May-October)"),
    x = "Hour of Day (EST)",
    y = "Height (m)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
