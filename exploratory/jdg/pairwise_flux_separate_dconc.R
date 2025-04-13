library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

#####CHANGE THIS TO USE CONC DIFFS NOT FLUXES!!!!!

data_list<-min9.FG.WP.list
gas <- "CO2"
data_file <- data_list[[gas]]

ggplot(data_file, aes(x=log(dConc)))+
  geom_histogram()+
  facet_wrap(~dLevelsAminusB)

# Ensure 'timeEnd_A' is in POSIXct format
data_file <- data_file %>%
  mutate(timeEnd_A = ymd_hms(timeEnd_A))

# Add a 'season' column based on the month and order it as winter (DJF), spring (MAM), summer (JJA), and fall (SON)
data_file <- data_file %>%
  mutate(season = case_when(
    month(timeEnd_A) %in% c(12, 1, 2) ~ "DJF",
    month(timeEnd_A) %in% c(3, 4, 5) ~ "MAM",
    month(timeEnd_A) %in% c(6, 7, 8) ~ "JJA",
    month(timeEnd_A) %in% c(9, 10, 11) ~ "SON"
  )) %>%
  mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")))  # Set the factor levels in the desired order

# Remove global outliers using IQR
Q1 <- quantile(data_file$FG_mean, 0.25, na.rm = TRUE)
Q3 <- quantile(data_file$FG_mean, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
data_file <- data_file %>%
  filter(FG_mean >= (Q1 - 3 * IQR) & FG_mean <= (Q3 + 3 * IQR))

# Group by 'TowerPosition_A', 'TowerPosition_B', and 'season', and calculate the mean flux
flux_data <- data_file %>%
  group_by(TowerPosition_A, TowerPosition_B, TowerHeight_A, TowerHeight_B, season) %>%
  summarise(dConc = mean(log(dConc), na.rm = TRUE)) %>%
  ungroup()

# Rename columns to match the variables used in the plotting code
flux_data <- flux_data %>%
  rename(to_level = TowerPosition_A, from_level = TowerPosition_B)

# Convert 'from_level' and 'to_level' to factors with combined levels
all_levels <- sort(unique(c(flux_data$from_level, flux_data$to_level)))
all_heights <- as.numeric(sort(unique(c(flux_data$TowerHeight_A, flux_data$TowerHeight_B))))

flux_data <- flux_data %>%
  mutate(
    from_level = factor(from_level, levels = all_levels),
    to_level = factor(to_level, levels = all_levels),
    from_height = factor(TowerHeight_B, levels = all_heights),
    to_height = factor(TowerHeight_A, levels = all_heights),
    from_level_num = as.numeric(as.character(from_level)),
    to_level_num = as.numeric(as.character(to_level)),
    from_height_num = as.numeric(as.character(TowerHeight_B)),
    to_height_num = as.numeric(as.character(TowerHeight_A))
  )

# Remove zero-length segments where from_level equals to_level
flux_data <- flux_data %>% filter(from_level_num != to_level_num)

# Adjust x positions for the main plot
flux_data <- flux_data %>%
  group_by(from_level_num) %>%
  mutate(
    x = from_level_num + (row_number() - (n() + 1) / 2) * 0.2  # Adjust spacing as needed
  ) %>%
  ungroup()

# Split the data into positive and negative fluxes for main plot
positive_flux <- flux_data %>% filter(dConc > 0)
negative_flux <- flux_data %>% filter(dConc <= 0)

# Filter for adjacent levels only (for the stacked arrows)
adjacent_flux_data <- flux_data %>%
  filter(abs(to_level_num - from_level_num) == 1)

positive_adjacent_flux <- adjacent_flux_data %>% filter(dConc > 0)
negative_adjacent_flux <- adjacent_flux_data %>% filter(dConc <= 0)

# Main plot
ggplot() +
  # Plot positive fluxes with arrows pointing to 'last'
  geom_segment(data = positive_flux, aes(
    x = x, y = from_height_num,
    xend = x, yend = to_height_num,
    size = abs(dConc),
    color = dConc
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +  # Restore original geometry
  
  # Plot negative fluxes with arrows pointing to 'first'
  geom_segment(data = negative_flux, aes(
    x = x, y = from_height_num,
    xend = x, yend = to_height_num,
    size = abs(dConc),
    color = dConc
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "first", angle = 30),
  lineend = "butt", linejoin = "mitre") +  # Restore original geometry
  
  # Add right-most arrows (adjacent levels) with black borders
  geom_segment(data = positive_adjacent_flux, aes(
    x = max(flux_data$x) + 1, y = from_height_num,
    xend = max(flux_data$x) + 1, yend = to_height_num,
    size = 3  # Border size for thicker border
  ),
  color = "black",  # Border color
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Overlay the interior of the arrows with fill colors
  geom_segment(data = positive_adjacent_flux, aes(
    x = max(flux_data$x) + 1, y = from_height_num,
    xend = max(flux_data$x) + 1, yend = to_height_num,
    size = 1,  # Smaller size for interior arrow
    color = dConc  # Use the flux color for the interior
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Repeat for negative adjacent fluxes
  geom_segment(data = negative_adjacent_flux, aes(
    x = max(flux_data$x) + 1, y = from_height_num,
    xend = max(flux_data$x) + 1, yend = to_height_num,
    size = 3  # Border size
  ),
  color = "black",  # Border color
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "first", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  geom_segment(data = negative_adjacent_flux, aes(
    x = max(flux_data$x) + 1, y = from_height_num,
    xend = max(flux_data$x) + 1, yend = to_height_num,
    size = 1,  # Smaller size for interior arrow
    color = dConc  # Use flux color for interior
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "first", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Size scaling for flux magnitude
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
    breaks = c(all_levels-1, max(flux_data$x)+1),  # Remove the last Tower Position label
    labels = c(paste("Level", all_levels), "Adjacent \nLevels")  # Custom label for the last position
  ) +
  
  scale_y_continuous(
    breaks = all_heights,
    labels = paste(all_heights, "m")
  ) +
  
  labs(
    x = "Tower Position",
    y = "Height (m)",
    title = paste(gas, "dConc Between Tower Levels"),
    size = "dConc",
    color = "dConc"
  ) +
  
  guides(size = "none") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels by 45 degrees
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a border to each panel
  ) +
  
  # Use facet_wrap by 'season' to split the plot by season
  facet_wrap(~ season)
