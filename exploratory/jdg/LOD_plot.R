#' Create a Density Ridge Plot for Gas Flux Data
#'
#' This function generates a density ridge plot to visualize gas flux data, 
#' filtering outliers and calculating standard deviation within specified bins.
#'
#' @author Jon Gewirtzman
#'
#' @param list Outputs from the flux processing workflow, e.g. 'min9.FG.WP.list'.
#' Each data frame should include the columns `FC_turb_interp` and `FG_mean`.
#' @param gas A string specifying the name of the gas to plot. It must match the name of the element in the `list`.
#' @param EC_0_bin A numeric value indicating the bin width around 0 for calculating ± standard deviation.
#' @param EC_flux_bins A numeric value defining the width of all additional bins for flux data.
#' @param sd_scalar A numeric value to multiply the standard deviation (sd) for visualizing bounds.
#' @param facet_heights A logical value. If `TRUE`, the plot will use `facet_wrap` for a specified column (e.g., `dLevelsAminusB`).
#'
#' @return A `ggplot` object representing a density ridge plot of gradient-flux values with vertical lines 
#' indicating ± scalar of standard deviation within a bin around zero EC flux.
#'
#' @details 
#' The function performs several steps:
#' 1. Cleans the data by removing NA and infinite values.
#' 2. Removes global outliers using the interquartile range (IQR).
#' 3. Bins the data into a user-specified range and filters bins with fewer than 100 data points.
#' 4. Calculates mean and standard deviation for data in the 0 ± `EC_0_bin` bin.
#' 5. Plots a density ridge plot with a gradient fill and adds vertical lines for ± scalar of the standard deviation.
#' 6. Optionally adds faceting for each pair of concentration profile heights `facet_heights = TRUE`.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import ggridges
#' 
#' @examples
#' ridgeplot(
#'   list = min9.FG.WP.list,
#'   gas = 'CO2',
#'   EC_0_bin = 2,
#'   EC_flux_bins = 3,
#'   sd_scalar = 1,
#'   facet_heights = TRUE
#' )
#' 
#' @export
###

ridgeplot <- function(data_file,
                      gas,
                      EC_0_bin,
                      EC_flux_bins,
                      sd_scalar,
                      facet_heights,
                      trace = NULL) {
  
  # Ensure librarian is installed
  if (!requireNamespace("librarian", quietly = TRUE)) {
    install.packages("librarian")
  }
  # Load required packages
  librarian::shelf(dplyr, ggplot2, tidyr)
  
  # Check if 'data_file' is actually a dataframe
  if (is.data.frame(data_file)) {
    # If a dataframe is provided, construct column names dynamically based on 'gas' and 'trace'
    FC_turb_interp_col <- paste0("FC_turb_interp_", gas)
    if (!is.null(trace)) {
      FG_mean_col <- paste0("F", gas, "_MBR_", trace, "trace_mean")
    } else {
      FG_mean_col <- paste0("FG_mean")
    }
    dLevelsAminusB_col <- paste0("dLevelsAminusB_", gas)
    
    clean_data <- data_file %>%
      filter(
        !is.na(!!sym(FC_turb_interp_col)) &
          !is.na(!!sym(FG_mean_col)) & is.finite(!!sym(FC_turb_interp_col)) & is.finite(!!sym(FG_mean_col))
      )
  } else {
    # If a data_file is provided, use the same approach as before
    clean_data <- data_file[[gas]] %>%
      filter(
        !is.na(FC_turb_interp) &
          !is.na(FG_mean) & is.finite(FC_turb_interp) & is.finite(FG_mean)
      )
  }
  
  # Step 2: Remove global outliers using IQR
  Q1 <- quantile(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], 0.25, na.rm = TRUE)
  Q3 <- quantile(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  clean_data <- clean_data %>%
    filter(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]] >= (Q1 - 3 * IQR) & 
             clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]] <= (Q3 + 3 * IQR))
  
  # Step 3: Define the bin range, ensuring the bin sequence includes all data and zero
  min_value <- floor(min(clean_data[[if (is.data.frame(data_file)) FC_turb_interp_col else "FC_turb_interp"]]) / EC_flux_bins) * EC_flux_bins - EC_0_bin
  max_value <- ceiling(max(clean_data[[if (is.data.frame(data_file)) FC_turb_interp_col else "FC_turb_interp"]]) / EC_flux_bins) * EC_flux_bins + EC_0_bin
  
  breaks_seq <- seq(-EC_0_bin, EC_0_bin, by = EC_flux_bins)
  breaks_seq <- c(rev(seq(-EC_0_bin, min_value, by = -EC_flux_bins)),
                  breaks_seq,
                  seq(EC_0_bin, max_value, by = EC_flux_bins))
  
  # Ensure breaks are unique and sorted
  breaks_seq <- sort(unique(breaks_seq))
  
  # Step 4: Bin the data using cut, ensuring no values fall into NA bins
  clean_data <- clean_data %>%
    mutate(x_bin = cut(
      clean_data[[if (is.data.frame(data_file)) FC_turb_interp_col else "FC_turb_interp"]],
      breaks = breaks_seq,
      include.lowest = TRUE,
      right = TRUE
    ))
  
  # Step 5: Filter out bins with fewer than 100 data points
  clean_data <- clean_data %>%
    group_by(x_bin) %>%
    filter(n() >= 100) %>%
    ungroup()
  
  # Step 6: Calculate mean and ±sd_scalar * SD for the bin around 0 ± EC_0_bin
  bin_stats <- clean_data %>%
    filter(clean_data[[if (is.data.frame(data_file)) FC_turb_interp_col else "FC_turb_interp"]] >= -EC_0_bin & clean_data[[if (is.data.frame(data_file)) FC_turb_interp_col else "FC_turb_interp"]] <= EC_0_bin) %>%
    summarise(
      mean_value = mean(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], na.rm = TRUE),
      sd_value = sd(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], na.rm = TRUE)
    ) %>%
    mutate(
      lower_limit = mean_value - sd_scalar * sd_value,
      upper_limit = mean_value + sd_scalar * sd_value
    )
  
  # Step 7: Create density ridge plot and add vertical lines for ±sd
  plot <- ggplot(clean_data, aes(y = x_bin, x = clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], fill = stat(x))) +
    geom_density_ridges_gradient(scale = 2,
                                 rel_min_height = 0.01,
                                 alpha = 0.7) +
    scale_fill_gradient2(
      low = "darkgreen",
      mid = "white",
      high = "chocolate4",
      midpoint = median(clean_data[[if (is.data.frame(data_file)) FG_mean_col else "FG_mean"]], na.rm = TRUE)
    ) +
    geom_vline(
      xintercept = c(bin_stats$lower_limit, bin_stats$upper_limit),
      color = "red",
      linetype = "dashed",
      size = 0.5
    ) +
    theme_minimal() +
    labs(x = "FG", y = "Binned FC")
  
  # Step 8: Conditionally add facet_wrap based on the facet_heights parameter
  if (facet_heights) {
    facet_col <- if (is.data.frame(data_file)) dLevelsAminusB_col else "dLevelsAminusB"
    plot <- plot + facet_wrap((paste("~", facet_col)))
  }
  
  return(plot)
}