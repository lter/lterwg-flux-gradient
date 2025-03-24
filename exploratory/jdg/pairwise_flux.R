library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

#gases=c("CO2", "CH4", "H2O")
#for(i in 1:3){gas<-gases[i]

gas<-"CO2"
data_file<-guanWP[[gas]]

###

# Ensure 'timeEnd_A' is in POSIXct format
data_file <- data_file %>%
  mutate(timeEnd_A = ymd_hms(timeEnd_A))  # Convert to POSIXct if it's not already

# Filter the dataframe for dates between May and September
data_file <- data_file %>%
  filter(month(timeEnd_A) >= 5 & month(timeEnd_A) <= 9)

# View the filtered dataframe
data_file

# Step 2: Remove global outliers using IQR
Q1 <- quantile(data_file$FG_mean, 0.25, na.rm = TRUE)
Q3 <- quantile(data_file$FG_mean, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

data_file <- data_file %>% filter(FG_mean >= (Q1 - 3 * IQR) & FG_mean <= (Q3 + 3 * IQR))


# Group by 'TowerPosition_A' and 'TowerPosition_B' and calculate the mean flux
flux_data <- data_file %>%
  group_by(TowerPosition_A, TowerPosition_B, TowerHeight_A, TowerHeight_B) %>%
  summarise(flux = mean(FG_mean, na.rm = TRUE)) %>%
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
    to_height = factor(TowerHeight_A, levels = all_heights)
  )

# Convert factor levels to numeric values correctly
flux_data <- flux_data %>%
  mutate(
    from_level_num = as.numeric(as.character(from_level)),
    to_level_num = as.numeric(as.character(to_level)),
    from_height_num = as.numeric(as.character(TowerHeight_B)),
    to_height_num = as.numeric(as.character(TowerHeight_A))
  )

# Remove zero-length segments where from_level equals to_level
flux_data <- flux_data %>% filter(from_level_num != to_level_num)

# Adjust x positions within each 'from_level' group to prevent overlap
flux_data <- flux_data %>%
  group_by(from_level_num) %>%
  mutate(
    x = from_level_num + (row_number() - (n() + 1) / 2) * 0.2  # Adjust spacing as needed
  ) %>%
  ungroup()

# Create 'arrow_ends' based on the sign of 'flux'
flux_data <- flux_data %>%
  mutate(arrow_ends = ifelse(flux > 0, "last", "first"))

# Split the data into positive and negative fluxes
positive_flux <- flux_data %>% filter(flux > 0)
negative_flux <- flux_data %>% filter(flux <= 0)

# Assuming your data is in a variable called 'df' and the color-mapped variable is 'value'
min_value <- pmin(min(negative_flux$flux, na.rm = TRUE), 0)
max_value <- pmax(max(positive_flux$flux, na.rm = TRUE), 0)


ggplot() +
  # Plot positive fluxes with arrows pointing to 'last' (towards 'to_level')
  geom_segment(data = positive_flux, aes(
    x = x, y = from_height_num,
    xend = x, yend = to_height_num,
    size = abs(flux),
    color = flux  # Use color for mapping flux
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Plot negative fluxes with arrows pointing to 'first' (back to 'from_level')
  geom_segment(data = negative_flux, aes(
    x = x, y = from_height_num,
    xend = x, yend = to_height_num,
    size = abs(flux),
    color = flux  # Use color for mapping flux
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "first", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
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
  
  scale_x_continuous(
    breaks = all_levels,
    labels = paste("Level", all_levels)
  ) +
  
  # Set y-axis labels to display the levels as discrete labels
  scale_y_continuous(
    breaks = all_heights,
    labels = paste(all_heights, "m")
  ) +
  
  labs(
    x = "",
    y = "",
    title = paste(gas, "Flux Between Tower Levels"),
    size = "Flux",
    color = "Flux"
  ) +
  
  guides(size = "none") +  # Remove size legend
  theme_minimal()+
  theme_minimal() +
  theme(panel.grid.major.y = element_line(),  # Keep major grid lines
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())  # Remove minor grid lines





# Filter to only keep adjacent levels
flux_data <- flux_data %>%
  filter(abs(to_level_num - from_level_num) == 1)

# Split the data into positive and negative fluxes
positive_flux <- flux_data %>% filter(flux > 0)
negative_flux <- flux_data %>% filter(flux <= 0)

# Create the plot with the same aesthetics as the original
ggplot() +
  # Plot positive fluxes with arrows pointing to 'last' (towards 'to_level')
  geom_segment(data = positive_flux, aes(
    x = as.factor(1), y = from_height_num,  # Fixed x position for stacking
    xend = 1, yend = to_height_num,
    size = abs(flux),
    color = flux  # Use color for mapping flux
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "last", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Plot negative fluxes with arrows pointing to 'first' (back to 'from_level')
  geom_segment(data = negative_flux, aes(
    x = as.factor(1), y = from_height_num,  # Fixed x position for stacking
    xend = 1, yend = to_height_num,
    size = abs(flux),
    color = flux  # Use color for mapping flux
  ),
  arrow = arrow(type = "closed", length = unit(0.3, "cm"), ends = "first", angle = 30),
  lineend = "butt", linejoin = "mitre") +
  
  # Size scaling for the flux magnitude
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
  
  # Set y-axis labels to display the heights as discrete labels
  scale_y_continuous(
    breaks = sort(unique(c(flux_data$from_height_num, flux_data$to_height_num))),
    labels = paste(sort(unique(c(flux_data$from_height_num, flux_data$to_height_num))), "m")
  ) +
  
  labs(
    x = NULL,
    y = "Height (m)",
    title = paste(gas, "Fluxes Between Successive Heights"),
    size = "Flux",
    color = "Flux"
  ) +
  
  theme_minimal() +
  theme(panel.grid.major.y = element_line(),  
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
