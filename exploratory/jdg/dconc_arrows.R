library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# Assuming data_list and gas variables are defined
# data_list <- min9.FG.WP.list
gas <- "CO2"
# data_file <- data_list[[gas]]

# Ensure 'timeEnd_A' is in POSIXct format if needed
if(!inherits(data_file$timeEnd_A, "POSIXct")) {
  data_file <- data_file %>%
    mutate(timeEnd_A = ymd_hms(as.character(timeEnd_A)))
}

# Add a 'season' column based on the month 
data_file <- data_file %>%
  mutate(season = case_when(
    month(timeEnd_A) %in% c(12, 1, 2) ~ "DJF",
    month(timeEnd_A) %in% c(3, 4, 5) ~ "MAM",
    month(timeEnd_A) %in% c(6, 7, 8) ~ "JJA",
    month(timeEnd_A) %in% c(9, 10, 11) ~ "SON"
  )) %>%
  mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")))

# Add day/night classification
# For simplicity, using hour of day (6am-6pm is day, otherwise night)
data_file <- data_file %>%
  mutate(tod = case_when(
    hour(timeEnd_A) >= 6 & hour(timeEnd_A) < 18 ~ "Day",
    TRUE ~ "Night"
  )) %>%
  mutate(tod = factor(tod, levels = c("Day", "Night")))

# Remove global outliers using IQR on dConc instead of FG_mean
Q1 <- quantile(data_file$dConc, 0.25, na.rm = TRUE)
Q3 <- quantile(data_file$dConc, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
data_file <- data_file %>%
  filter(dConc >= (Q1 - 3 * IQR) & dConc <= (Q3 + 3 * IQR))

# Ensure TowerHeight variables are numeric
data_file <- data_file %>%
  mutate(
    TowerHeight_A = as.numeric(TowerHeight_A),
    TowerHeight_B = as.numeric(TowerHeight_B)
  )

# Group by tower positions, heights, season, and time of day, then calculate the mean concentration difference
conc_data <- data_file %>%
  group_by(TowerPosition_A, TowerPosition_B, TowerHeight_A, TowerHeight_B, season, tod) %>%
  summarise(dConc = mean(dConc, na.rm = TRUE)) %>%  # Using dConc directly, not log
  ungroup()

# Rename columns for plotting
conc_data <- conc_data %>%
  rename(to_level = TowerPosition_A, from_level = TowerPosition_B)

# Get all unique levels and heights
all_levels <- sort(unique(c(conc_data$from_level, conc_data$to_level)))
all_heights <- sort(unique(c(conc_data$TowerHeight_A, conc_data$TowerHeight_B)))

# Convert levels and heights to factors and add numerical versions
conc_data <- conc_data %>%
  mutate(
    from_level = factor(from_level, levels = all_levels),
    to_level = factor(to_level, levels = all_levels),
    from_height = as.factor(TowerHeight_B),
    to_height = as.factor(TowerHeight_A),
    from_level_num = as.numeric(as.character(from_level)),
    to_level_num = as.numeric(as.character(to_level)),
    from_height_num = TowerHeight_B,  # Already numeric
    to_height_num = TowerHeight_A     # Already numeric
  )

# CRITICAL ADDITION: Explicitly create high-to-low concentration direction
conc_data <- conc_data %>%
  mutate(
    # If dConc > 0, then Conc_A > Conc_B
    # If dConc < 0, then Conc_A < Conc_B
    higher_conc_height = ifelse(dConc > 0, to_height_num, from_height_num),
    lower_conc_height = ifelse(dConc > 0, from_height_num, to_height_num),
    arrow_direction = ifelse(higher_conc_height > lower_conc_height, "down", "up")
  )

# Remove zero-length segments where from_level equals to_level
conc_data <- conc_data %>% 
  filter(from_level_num != to_level_num)

# Adjust x positions for the main plot
conc_data <- conc_data %>%
  group_by(from_level_num) %>%
  mutate(
    x = from_level_num + (row_number() - (n() + 1) / 2) * 0.2  # Adjust spacing as needed
  ) %>%
  ungroup()

# Filter for adjacent levels only (for the stacked arrows)
adjacent_conc_data <- conc_data %>%
  filter(abs(to_level_num - from_level_num) == 1)

# Main plot
ggplot() +
  # Plot arrows from high concentration to low concentration
  geom_segment(data = conc_data, aes(
    x = x, y = higher_conc_height,
    xend = x, yend = lower_conc_height,
    size = abs(dConc),
    color = dConc
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Add right-most arrows (adjacent levels) with black borders
  geom_segment(data = adjacent_conc_data, aes(
    x = max(conc_data$x) + 1, y = higher_conc_height,
    xend = max(conc_data$x) + 1, yend = lower_conc_height,
    size = 3  # Border size for thicker border
  ),
  color = "black",  # Border color
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Overlay the interior of the arrows with fill colors
  geom_segment(data = adjacent_conc_data, aes(
    x = max(conc_data$x) + 1, y = higher_conc_height,
    xend = max(conc_data$x) + 1, yend = lower_conc_height,
    size = 1,  # Smaller size for interior arrow
    color = dConc  # Use the concentration difference color for the interior
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Size scaling for concentration difference magnitude
  scale_size_continuous(range = c(0.5, 5)) +
  
  # Use scale_color_gradient2 for mapping colors based on the gas type
  {
    if (gas == "CO2") {
      scale_color_gradient2(low = "forestgreen", mid = "white", high = "chocolate4", midpoint = 0)
    } else if (gas == "CH4") {
      scale_color_gradient2(low = "#4682B4", mid = "white", high = "#B22222", midpoint = 0)
    } else if (gas == "H2O") {
      scale_color_gradient2(low = "goldenrod", mid = "white", high = "#4682B4", midpoint = 0)
    }
  } +
  
  # Set x and y-axis labels
  scale_x_continuous(
    breaks = c(all_levels, max(conc_data$x) + 1),
    labels = c(paste("Level", all_levels), "Adjacent\nLevels")
  ) +
  
  scale_y_continuous(
    breaks = all_heights,
    labels = paste(all_heights, "m")
  ) +
  
  labs(
    x = "Tower Position",
    y = "Height (m)",
    title = paste(gas, "Concentration Differences Between Tower Levels"),
    subtitle = "Arrows point from high to low concentration",
    size = "dConc Magnitude",
    color = "dConc"
  ) +
  
  guides(size = "none") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  
  # Use facet_grid to split by both season and time of day
  facet_grid(tod ~ season)