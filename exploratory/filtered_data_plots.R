library(ggplot2)
library(dplyr)
# Function to bin data and compute mean & standard error
compute_binned_stats <- function(data, x_col, y_col, bins = 20) {
  # Create numeric bins
  bin_edges <- seq(min(data[[x_col]], na.rm = TRUE), max(data[[x_col]], na.rm = TRUE), length.out = bins + 1)
  
  data %>%
    mutate(
      x_bin = cut(!!sym(x_col), breaks = bin_edges, labels = FALSE),  # Numeric bins
      x_center = (bin_edges[x_bin] + bin_edges[x_bin + 1]) / 2  # Compute bin midpoint
    ) %>%
    group_by(x_bin, x_center) %>%
    summarise(
      y_mean = mean(!!sym(y_col), na.rm = TRUE),
      y_se = sd(!!sym(y_col), na.rm = TRUE) / sqrt(n()),  # Standard Error
      .groups = "drop"
    ) %>%
    filter(!is.na(x_center))  # Remove any remaining NAs
}

# Function to plot binned data with error bars
plot_binned_data <- function(original_data, filtered_data, x_col, y_col, bins = 20) {
  # Compute stats for filtered and unfiltered data
  original_stats <- compute_binned_stats(original_data, x_col, y_col, bins)
  filtered_stats <- compute_binned_stats(filtered_data, x_col, y_col, bins)
  
  ggplot() +
    # Error bars for filtered-out data (grey)
    geom_errorbar(data = original_stats, aes(x = x_center, ymin = y_mean - y_se, ymax = y_mean + y_se), 
                  width = 1, color = "grey", alpha = 0.5) +
    geom_point(data = original_stats, aes(x = x_center, y = y_mean), color = "grey", alpha = 0.5) +
    
    # Error bars for kept data (blue)
    geom_errorbar(data = filtered_stats, aes(x = x_center, ymin = y_mean - y_se, ymax = y_mean + y_se), 
                  width = 1, color = "blue", alpha = 0.8) +
    geom_point(data = filtered_stats, aes(x = x_center, y = y_mean), color = "blue", alpha = 0.8) +
    
    # 1:1 reference line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    
    labs(title = "Binned Mean Points with Error Bars",
         x = x_col, y = y_col) +
    theme_minimal()
}

# Generate sample data
set.seed(123)
original_data <- data.frame(
  x = rnorm(5000, mean = 50, sd = 10),
  y = rnorm(5000, mean = 50, sd = 10) + rnorm(5000, mean = 0, sd = 5)
)

# Apply filtering: Keep points where y is within 10 units of x
filtered_data <- original_data %>% filter(abs(y - x) < 10)

# Plot with binned means and error bars
plot_binned_data(original_data, filtered_data, "x", "y", bins = 20)








library(ggplot2)
library(dplyr)

# Function to plot scatter plot with transparency
plot_filtered_scatter <- function(original_data, filtered_data, x_col, y_col) {
  ggplot() +
    # Scatter plot for filtered-out data (grey, semi-transparent)
    geom_point(data = original_data, aes_string(x = x_col, y = y_col), 
               color = "grey", alpha = 0.1, size = 1) +
    
    # Scatter plot for kept data (blue, slightly more opaque)
    geom_point(data = filtered_data, aes_string(x = x_col, y = y_col), 
               color = "blue", alpha = 0.3, size = 1) +
    
    # 1:1 reference line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    
    labs(title = "Scatter Plot of Filtered vs. Kept Data",
         x = x_col, y = y_col) +
    theme_minimal()
}

# Generate larger sample data
set.seed(123)
original_data <- data.frame(
  x = rnorm(5000, mean = 50, sd = 10),
  y = rnorm(5000, mean = 50, sd = 10)
)
original_data$y <- original_data$y + rnorm(5000, mean = 0, sd = 5)

# Apply filtering: Keep points where y is within 10 units of x
filtered_data <- original_data %>% filter(abs(y - x) < 10)

# Plot using the function
plot_filtered_scatter(original_data, filtered_data, "x", "y")

###############DIEL###############

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to calculate diel average
calculate_diel_avg <- function(data, time_column, value_column, filter_condition = NULL) {
  data <- data %>%
    mutate(Hour = as.numeric(format(!!sym(time_column), "%H")))  # Extract hour
  
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))  # Apply filtering
  }
  
  diel_avg <- data %>%
    group_by(Hour) %>%
    summarise(Mean = mean(!!sym(value_column), na.rm = TRUE),
              SD = sd(!!sym(value_column), na.rm = TRUE),
              N = n()) %>%
    ungroup()
  
  return(diel_avg)
}

# Function to plot diel average
plot_diel_avg <- function(diel_avg, title) {
  ggplot(diel_avg, aes(x = Hour, y = Mean)) +
    geom_line(color = "blue", size = 1) +
    geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.2, fill = "blue") +
    labs(title = title, x = "Hour of the Day", y = "Mean Value") +
    theme_minimal()
}

# Example dataset
set.seed(123)
example_data <- data.frame(
  Timestamp = seq.POSIXt(from = as.POSIXct("2023-01-01 00:00"), 
                         to = as.POSIXct("2023-01-07 23:00"), by = "hour"),
  Value = rnorm(168, mean = 50, sd = 10),  # Simulated values
  FilterFlag = sample(c(TRUE, FALSE), 168, replace = TRUE)  # Random filtering flag
)

# Compute and plot diel average for unfiltered data
diel_avg_unfiltered <- calculate_diel_avg(example_data, "Timestamp", "Value")
plot_diel_avg(diel_avg_unfiltered, "Diel Average - Unfiltered")

# Compute and plot diel average for filtered data
diel_avg_filtered <- calculate_diel_avg(example_data, "Timestamp", "Value", "FilterFlag == TRUE")
plot_diel_avg(diel_avg_filtered, "Diel Average - Filtered")

