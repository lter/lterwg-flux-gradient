

# Function to compute RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm=TRUE))
}


# Function to create plots for the site, super_bad_eddy threshold, and gas
generate_plots <- function(site_name, super_bad_threshold, gas_type) {
  
  # GET DATA
  filt_df <- SITES_AE_9min_FILTER_BH[[site_name]]
  df <- SITES_AE_9min[[site_name]]
  
  # Filter by the best height
  best_heights <- unique(filt_df$dLevelsAminusB)
  df <- df %>% filter(dLevelsAminusB %in% best_heights[1:2])
  
  # Filter by gas
  df_gas <- df %>% dplyr::filter(gas == gas_type)
  filt_df_gas <- filt_df %>% dplyr::filter(gas == gas_type)
  
  # Apply Super Bad Eddy filter
  df_gas <- Super_Bad_Eddy(df_gas, "EddyDiff", super_bad_threshold)
  Bad_df_gas <- df_gas %>% dplyr::filter(s_eddy_outliers == 0)
  
  # Print number of data points
  raw_count <- nrow(df_gas)
  filtered_count <- nrow(filt_df_gas)
  superbad_count <- nrow(Bad_df_gas)
  
  percent_flagged <- round(100 * (1 - superbad_count / raw_count), 2)
  print(paste("Percentage of data flagged as s_eddy_outliers:", percent_flagged, "%"))
  print(paste("Number of datapoints for", gas_type, ":"))
  print(paste("Raw data:", raw_count))
  print(paste("Filtered data:", filtered_count))
  print(paste("Superbad filtered data:", superbad_count))
  
  # Set correct x variable
  if (gas_type == "CO2") {
    x_var <- "FC_turb_interp"
  } else if (gas_type == "H2O") {
    x_var <- "FH2O_interp"
  } else {
    stop("Unknown gas type")
  }
  
  y_var <- "FG_mean"  # Always use FG_mean
  
  # Plotting
  p1 <- ggplot(df_gas, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Raw Data -", gas_type)) +
    xlim(-2, 12) + ylim(-10, 40) +
    geom_abline(slope = 1, intercept = 0, color = "red")
  
  p2 <- ggplot(filt_df_gas, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Filtered Data -", gas_type)) +
    xlim(-2, 12) + ylim(-10, 40) +
    geom_abline(slope = 1, intercept = 0, color = "red")
  
  p3 <- ggplot(Bad_df_gas, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Superbad Filtered Data -", gas_type)) +
    xlim(-2, 12) + ylim(-10, 40) +
    geom_abline(slope = 1, intercept = 0, color = "red")
  
  # Stack vertically
  grid.arrange(p1, p2, p3, ncol = 1)
}



# Example usage:
generate_plots(site_name = "KONZ", super_bad_threshold = 0.1, gas_type = "H2O")
generate_plots(site_name = "KONZ", super_bad_threshold = 0.05, gas_type = "CO2")
generate_plots(site_name = "GUAN", super_bad_threshold = 0.05, gas_type = "CO2")
generate_plots(site_name = "GUAN", super_bad_threshold = 0.1, gas_type = "H2O")
generate_plots(site_name = "JORN", super_bad_threshold = 0.1, gas_type = "H2O")
generate_plots(site_name = "JORN", super_bad_threshold = 0.1, gas_type = "CO2")
generate_plots(site_name = "HARV", super_bad_threshold = 0.1, gas_type = "CO2")
generate_plots(site_name = "HARV", super_bad_threshold = 0.2, gas_type = "H2O")




######################What is being filtered##############################



library(ggplot2)
library(dplyr)
library(gridExtra)

# Stability filtering summary function
stability_barplots <- function(site_name, super_bad_threshold, gas_type) {
  
  # GET DATA
  filt_df <- SITES_AE_9min_FILTER_BH[[site_name]]
  df <- SITES_AE_9min[[site_name]]
  
  # Filter by best heights (same logic as before)
  best_heights <- unique(filt_df$dLevelsAminusB)
  df <- df %>% filter(dLevelsAminusB %in% best_heights[1:2])
  
  # Filter by gas
  df_gas <- df %>% filter(gas == gas_type)
  filt_df_gas <- filt_df %>% filter(gas == gas_type)
  
  # Apply Super Bad Eddy filter
  df_gas <- Super_Bad_Eddy(df_gas, "EddyDiff", super_bad_threshold)
  bad_df_gas <- df_gas %>% filter(s_eddy_outliers == 0)
  
  # Helper to count by Stability_100
  count_stability <- function(data) {
    data %>%
      filter(!is.na(Stability_500)) %>%
      count(Stability_500) %>%
      tidyr::complete(Stability_500 = c("neutral", "stable", "unstable"), fill = list(n = 0)) %>%
      arrange(factor(Stability_500, levels = c("neutral", "stable", "unstable")))
  }
  
  raw_counts <- count_stability(df_gas)
  filt_counts <- count_stability(filt_df_gas)
  bad_counts <- count_stability(bad_df_gas)
  
  # Calculate number removed
  removed_filtered <- raw_counts$n - filt_counts$n
  removed_bad <- raw_counts$n - bad_counts$n
  
  # Plot data
  filtered_plot_data <- data.frame(Stability_500 = raw_counts$Stability_500,
                                   Removed = removed_filtered)
  superbad_plot_data <- data.frame(Stability_500 = raw_counts$Stability_500,
                                   Removed = removed_bad)
  
  # Make plots
  p1 <- ggplot(filtered_plot_data, aes(x = Stability_500, y = Removed, fill = Stability_500)) +
    geom_bar(stat = "identity") +
    ggtitle("Filtered Dataset: Stability Classes Removed") +
    ylab("Rows Removed") +
    xlab("Stability Class") +
    scale_fill_manual(values = c("neutral" = "gray", "stable" = "skyblue", "unstable" = "orange")) +
    theme_minimal()
  
  p2 <- ggplot(superbad_plot_data, aes(x = Stability_500, y = Removed, fill = Stability_500)) +
    geom_bar(stat = "identity") +
    ggtitle("Super Bad Eddy: Stability Classes Removed") +
    ylab("Rows Removed") +
    xlab("Stability Class") +
    scale_fill_manual(values = c("neutral" = "gray", "stable" = "skyblue", "unstable" = "orange")) +
    theme_minimal()
  
  # Percent remaining
  pct_remaining <- function(filtered, raw) round(100 * filtered / raw, 1)
  
  pct_filt <- pct_remaining(filt_counts$n, raw_counts$n)
  pct_bad <- pct_remaining(bad_counts$n, raw_counts$n)
  
  # Print percent messages
  for (i in 1:3) {
    cat(paste0("\n", raw_counts$Stability_500[i], ":\n"))
    cat(paste("  Filtered dataset retains", pct_filt[i], "%\n"))
    cat(paste("  Super bad eddy retains", pct_bad[i], "%\n"))
  }
  
  # Show plots
  grid.arrange(p1, p2, ncol = 1)
}

stability_barplots(site_name = "KONZ", super_bad_threshold = 0.1, gas_type = "H2O")
stability_barplots(site_name = "KONZ", super_bad_threshold = 0.05, gas_type = "CO2")
stability_barplots(site_name = "GUAN", super_bad_threshold = 0.05, gas_type = "CO2")
stability_barplots(site_name = "GUAN", super_bad_threshold = 0.1, gas_type = "H2O")
stability_barplots(site_name = "JORN", super_bad_threshold = 0.1, gas_type = "H2O")
stability_barplots(site_name = "JORN", super_bad_threshold = 0.1, gas_type = "CO2")
stability_barplots(site_name = "HARV", super_bad_threshold = 0.1, gas_type = "CO2")
stability_barplots(site_name = "HARV", super_bad_threshold = 0.2, gas_type = "H2O")



#'It seems stable is preferred to be kept becuase under these conditions fluxes 
#'would match since the gradient is not changing very much



################################################################################
####################CAN ONE GAS FILTER ANOTHER?#################################

library(ggplot2)
library(gridExtra)
library(dplyr)

# RMSE function
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}
puritan_filter_workflow <- function(site_name, super_bad_threshold, filtering_gas, target_gas) {
  # Load data
  filt_df <- SITES_AE_9min_FILTER_BH[[site_name]]
  df <- SITES_AE_9min[[site_name]]
  
  # Filter by best height(s)
  best_heights <- unique(filt_df$dLevelsAminusB)
  df <- df %>% filter(dLevelsAminusB %in% best_heights[1:2])
  
  # Filter by gases
  df_filtering_gas <- df %>% filter(gas == filtering_gas)
  df_target_gas <- df %>% filter(gas == target_gas)
  filt_df_target_gas <- filt_df %>% filter(gas == target_gas)
  
  # Apply Super Bad Eddy to filtering_gas
  df_filtering_gas <- Super_Bad_Eddy(df_filtering_gas, "EddyDiff", super_bad_threshold)
  df_filtering_good <- df_filtering_gas %>% filter(s_eddy_outliers == 0)
  
  # Filter target_gas using timestamps from "good" filtering_gas
  df_target_gas_filtered <- df_target_gas %>% filter(timeMid %in% df_filtering_good$timeMid)
  
  # Determine variables dynamically
  if (target_gas == "CO2") {
    x_var <- "FC_turb_interp"
  } else if (target_gas == "H2O") {
    x_var <- "FH2O_interp"
  } else {
    stop("Unsupported target gas")
  }
  y_var <- "FG_mean"
  
  # Data counts
  raw_count <- nrow(df_target_gas)
  filtered_count <- nrow(filt_df_target_gas)
  superbad_filtered_count <- nrow(df_target_gas_filtered)
  
  # Output info
  cat("\n===== Data Point Counts =====\n")
  cat("Site:", site_name, "\n")
  cat("Target Gas:", target_gas, "| Filtering Gas:", filtering_gas, "\n")
  cat("Raw (Unfiltered):", raw_count, "\n")
  cat("Filtered (BH):", filtered_count, "\n")
  cat("Filtered via Super Bad Eddy (from", filtering_gas, "):", superbad_filtered_count, "\n")
  
  # RMSEs
  cat("\n===== RMSE Comparison =====\n")
  cat("Raw RMSE:", rmse(df_target_gas[[x_var]], df_target_gas[[y_var]]), "\n")
  cat("Filtered RMSE:", rmse(filt_df_target_gas[[x_var]], filt_df_target_gas[[y_var]]), "\n")
  cat("Super Bad Eddy Filtered RMSE:", rmse(df_target_gas_filtered[[x_var]], df_target_gas_filtered[[y_var]]), "\n")
  
  # Plotting
  p1 <- ggplot(df_target_gas, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Raw -", target_gas)) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    xlim(-40, 40) + ylim(-60, 40)
  
  p2 <- ggplot(filt_df_target_gas, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Filtered -", target_gas)) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    xlim(-40, 40) + ylim(-60, 40)
  
  p3 <- ggplot(df_target_gas_filtered, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    ggtitle(paste("Filtered via", filtering_gas, "- SBE")) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    xlim(-40, 40) + ylim(-60, 40)
  
  grid.arrange(p1, p2, p3, ncol = 1)
}


# Compare how CO2 performs after filtering based on H2O
puritan_filter_workflow(
  site_name = "KONZ",
  super_bad_threshold = 0.05,
  filtering_gas = "H2O",
  target_gas = "CO2"
)

# Or the reverse
puritan_filter_workflow(
  site_name = "HARV",
  super_bad_threshold = 0.5,
  filtering_gas = "CO2",
  target_gas = "H2O"
)


# Or the reverse
puritan_filter_workflow(
  site_name = "GUAN",
  super_bad_threshold = 0.1,
  filtering_gas = "CO2",
  target_gas = "H2O"
)


