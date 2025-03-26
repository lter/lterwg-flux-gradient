# Load required libraries
library(googledrive)
library(fs)




# Authenticate with Google Drive
email <- 'sam.jurado@yale.edu'
googledrive::drive_auth(email = email)

# Define Google Drive file IDs for required data files
file_ids <- list(
  "SITES_AE_9min.Rdata" = "12nE75Zh-tQ8oXOXARj2_3J2HjA6Lf1wl",
  "SITES_WP_9min.Rdata"= "17Y-QYKSof3nMOAz6L8QsCxtLSoqObvn-",
  "FilteredData_MS1Sites.Rdata" = "1fJ_7lG5WO6lyuYVkhaAhyv1uZCPn4EXw",
  "FilteredData_MS1Sites_BH.Rdata" = "1ziIgm1VpSIrwtiuXmhPHMgse1pebi28L"
)

# Define local directory for downloads
localdir <- "/Users/jurado/flux_group_git/lterwg-flux-gradient/data"

# Create local directory if it doesn't exist
if (!dir.exists(localdir)) {
  dir.create(localdir, recursive = TRUE)
}

# Function to download files from Google Drive
download_googledrive_file <- function(file_name, file_id, localdir) {
  message(paste0("Downloading ", file_name, " from Google Drive..."))
  
  # Define local path
  local_path <- file.path(localdir, file_name)
  
  # Download the file
  googledrive::drive_download(
    as_id(file_id),
    path = local_path,
    overwrite = TRUE
  )
  
  message(paste0(file_name, " downloaded successfully to ", local_path))
}

# Loop through the files and download them
for (file_name in names(file_ids)) {
  download_googledrive_file(file_name, file_ids[[file_name]], localdir)
}

# Load the downloaded RData files
for (file_name in names(file_ids)) {
  local_path <- file.path(localdir, file_name)
  if (file.exists(local_path)) {
    message(paste0("Loading ", file_name, "..."))
    load(local_path)
  } else {
    warning(paste0("File ", file_name, " not found after download."))
  }
}

message("All files processed successfully.")

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm=TRUE))
}

KONZ_df <- SITES_AE_9min$KONZ

KONZ_df <- KONZ_df %>% filter(gas =="CO2")

filt_KONZ_df <- SITES_AE_9min_FILTER_BH$KONZ
best_heights <- unique(filt_KONZ_df$dLevelsAminusB)

##Needs to be an if statement 
KONZ_df <- KONZ_df %>% filter(dLevelsAminusB == best_heights[1] | dLevelsAminusB == best_heights[2])
unique(KONZ_df$dLevelsAminusB)


KONZ_df <- Bad_Eddy(KONZ_df, "EddyDiff")

Bad_KONZ_df <- KONZ_df %>% filter(Eddy_outlier == 0)

KONZ_df <- Super_Bad_Eddy(KONZ_df,"EddyDiff",.2)

Super_Bad_KONZ_df <- KONZ_df %>% filter(s_eddy_outliers == 0)

  
Bad_filt_KONZ_df <- Bad_Eddy(filt_KONZ_df, "EddyDiff")

#Plot 1:1 using my function for unfiltered, filtered, and bad_eddy filtered
# take RMSE for before and after for each

plot(KONZ_df$FC_turb_interp,KONZ_df$FG_mean)
print(rmse(KONZ_df$FC_turb_interp,KONZ_df$FG_mean))

#plot(Bad_KONZ_df$FC_turb_interp,Bad_KONZ_df$FG_mean)
#print(rmse(Bad_KONZ_df$FC_turb_interp,Bad_KONZ_df$FG_mean))

plot(filt_KONZ_df$FC_turb_interp,filt_KONZ_df$FG_mean)
print(rmse(filt_KONZ_df$FC_turb_interp,filt_KONZ_df$FG_mean))

#plot(Bad_filt_KONZ_df$FC_turb_interp,Bad_filt_KONZ_df$FG_mean)
#print(rmse(Bad_filt_KONZ_df$FC_turb_interp,Bad_filt_KONZ_df$FG_mean))


#How to compare what percents of data were taken by each and if they overlap


plot_filtered_scatter(KONZ_df,filt_KONZ_df,"FC_turb_interp","FG_mean")
plot_binned_data(KONZ_df,filt_KONZ_df,"FC_turb_interp","FG_mean", bins = 20)


#How to compare what percents of data were taken by each and if they overlap

#plot_filtered_scatter(KONZ_df,Bad_KONZ_df,"FC_turb_interp","FG_mean")
#plot_binned_data(KONZ_df,Bad_KONZ_df,"FC_turb_interp","FG_mean", bins = 20)

plot_filtered_scatter(KONZ_df,Super_Bad_KONZ_df,"FC_turb_interp","FG_mean")
plot_binned_data(KONZ_df,Super_Bad_KONZ_df,"FC_turb_interp","FG_mean", bins = 20)



######DIEL####


# Compute and plot diel average for unfiltered data
diel_avg_TRUE <- calculate_diel_avg(KONZ_df, "timeBgn_A", "FC_turb_interp")
plot_diel_avg(diel_avg_TRUE, "Diel Average - EC")


# Compute and plot diel average for unfiltered data
diel_avg_unfiltered <- calculate_diel_avg(KONZ_df, "timeBgn_A", "FG_mean")
plot_diel_avg(diel_avg_unfiltered, "Diel Average - Unfiltered")

# Compute and plot diel average for filtered data
#diel_avg_filtered <- calculate_diel_avg(Bad_KONZ_df, "timeBgn_A", "FG_mean")
#plot_diel_avg(diel_avg_filtered, "Diel Average - Filtered Bad_Eddy")

# Compute and plot diel average for filtered data
diel_avg_filtered <- calculate_diel_avg(filt_KONZ_df, "timeBgn_A", "FG_mean")
plot_diel_avg(diel_avg_filtered, "Diel Average - Filtered_dConcNorm")
print(rmse(diel_avg_TRUE$Mean,diel_avg_filtered$Mean ))


# Compute and plot diel average for filtered data
diel_avg_filtered <- calculate_diel_avg(Super_Bad_KONZ_df, "timeBgn_A", "FG_mean")
plot_diel_avg(diel_avg_filtered, "Diel Average - Filtered_Super_Bad")
print(rmse(diel_avg_TRUE$Mean,diel_avg_filtered$Mean ))


#' The conditions in which FG failed are due to concentration gradients
#' that cannot produce the flux measured by the EC tower since they would need 
#' an eddy diffusivity unrealistic of the state of the atmosphere when measured
#' to do so. This filter would require EC with FG to work, but it can show what
#' 1. It is the concentration measurments
#' 2. WE can improve concentration measurments independent of EC data
#' 3. Generally improved RMSE comes at the cost of more convective conditions that 
#' reduce the gradient 
#' Havent checked for water since LE is in W right now
#' 
#' TO DO:
#' RMSE drop vs absolute difference between K for each


# Function to iterate through values of x and compute RMSE
evaluate_x_rmse <- function(df, method, x_values) {
  results <- data.frame(x = numeric(), RMSE = numeric())
  
  for (x in x_values) {
    # Apply Super_Bad_Eddy function
    modified_df <- Super_Bad_Eddy(df, method, x)
    
    # Filter out outliers
    filtered_df <- modified_df %>% filter(s_eddy_outliers == 0)
    
    # Ensure there is data left after filtering
    if (nrow(filtered_df) > 0) {
      # Compute RMSE between FC_turb_interp and FG_mean
      rmse_value <- rmse(filtered_df$FC_turb_interp, filtered_df$FG_mean)
      
      # Store results
      results <- rbind(results, data.frame(x = x, RMSE = rmse_value))
    } else {
      # If all data is filtered out, assign NA
      results <- rbind(results, data.frame(x = x, RMSE = NA))
    }
  }
  
  return(results)
}

# Define range of x values
x_values <- seq(0, 8, by = 0.05)  # Adjust range as needed

# Run evaluation
rmse_results <- evaluate_x_rmse(KONZ_df, "EddyDiff", x_values)

# Print results
print(rmse_results)

# Plot RMSE vs x
plot(rmse_results$x, rmse_results$RMSE, type = "b", pch = 16, col = "blue",
     xlab = "Absolute Difference between KEC and Kcalc", ylab = "RMSE", main = "RMSE vs. x")
