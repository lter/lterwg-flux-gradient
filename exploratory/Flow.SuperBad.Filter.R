# Load required libraries
library(googledrive)
library(fs)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm=TRUE))
}

# Authenticate with Google Drive
email <- 'sam.jurado@yale.edu'
googledrive::drive_auth(email = email)

# Define Google Drive file IDs for required data files
file_ids <- list(
   "SITES_AE_9min.Rdata" = "12nE75Zh-tQ8oXOXARj2_3J2HjA6Lf1wl",
#  "SITES_WP_9min.Rdata"= "17Y-QYKSof3nMOAz6L8QsCxtLSoqObvn-",
  "FilteredData_MS1Sites_BH.Rdata" = "1Uc1KquqdvRGi6J1vxaKFEO0FkP5IC7Tv"
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
    googledrive::as_id(file_id),  # Explicitly call as_id from googledrive
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


#Where are we loosing the most data, under what conditions?

#GET DATA
filt_df<- SITES_AE_9min_FILTER_BH$KONZ
df <- SITES_AE_9min$KONZ
df <- Super_Bad_Eddy(df,"EddyDiff",.03)
Bad_df <- df %>% dplyr::filter( s_eddy_outliers == 0)

#Compare the same heights and the same gas
best_heights <- unique(filt_df$dLevelsAminusB)

if (length(best_heights) >= 2) {
  df <- df %>% dplyr::filter(dLevelsAminusB %in% best_heights[1:2])
} else {
  df <- df %>% dplyr::filter(dLevelsAminusB %in% best_heights)
}

Gas = "CO2"
df <- df %>% dplyr::filter(gas == Gas)
filt_df <- filt_df %>% dplyr::filter(gas == Gas)

############
library(dplyr)
library(ggplot2)

# Count occurrences of each stability category
unfiltered_counts <- df%>%
  count(Stability_500, name = "Total")

filtered_counts <- filt_df %>%
  count(Stability_500, name = "Remaining")

# Merge the counts by stability category
stability_loss <- left_join(unfiltered_counts, filtered_counts, by = "Stability_500") %>%
  mutate(Lost = Total - coalesce(Remaining, 0))  # Handle missing categories in filtered df



ggplot(stability_loss, aes(x = Stability_500, y = Lost, fill = Stability_500)) +
  geom_bar(stat = "identity") +
  labs(title = "Data Loss by Stability Category",
       x = "Stability Category",
       y = "Number of Observations Lost") +
  theme_minimal() +
  scale_fill_manual(values = c("Stable" = "blue", "Unstable" = "red", "Neutral" = "gray")) +
  theme(legend.position = "none")


########Diurnal average####

library(dplyr)
library(ggplot2)
library(lubridate)


# Extract hour of the day
filt_df <- filt_df %>% mutate(hour = hour(timeMid))
df <- df %>% mutate(hour = hour(timeMid))

# Compute mean, SEM, and count for filtered data
diel_avg_filtered <- filt_df  %>%
  group_by(hour) %>%
  summarise(
    FG_mean_avg = mean(FG_mean, na.rm = TRUE),
    FG_mean_SE = sd(FG_mean, na.rm = TRUE) / sqrt(n()),  
    FC_turb_avg = mean(FC_turb_interp, na.rm = TRUE),
    FC_turb_SE = sd(FC_turb_interp, na.rm = TRUE) / sqrt(n()),  
    count_filtered = n()
  )

# Compute count for unfiltered data
diel_avg_unfiltered <- df  %>%
  group_by(hour) %>%
  summarise(count_unfiltered = n())

# Merge data and compute observation loss
diel_avg <- left_join(diel_avg_filtered, diel_avg_unfiltered, by = "hour") %>%
  mutate(observation_loss = count_unfiltered - coalesce(count_filtered, 0))

# Create the plot
ggplot(diel_avg, aes(x = hour)) +
  geom_point(aes(y = FG_mean_avg, size = observation_loss, color = "FG_mean")) +
  geom_errorbar(aes(ymin = FG_mean_avg - FG_mean_SE, ymax = FG_mean_avg + FG_mean_SE, color = "FG_mean"), width = 0.3) +
  geom_point(aes(y = FC_turb_avg, size = observation_loss, color = "FC_turb_interp"), shape = 17) +  # Different shape for FC_turb_interp
  geom_errorbar(aes(ymin = FC_turb_avg - FC_turb_SE, ymax = FC_turb_avg + FC_turb_SE, color = "FC_turb_interp"), width = 0.3) +
  scale_size_continuous(range = c(2, 8)) +  # Adjust point size scale
  scale_color_manual(values = c("FG_mean" = "blue", "FC_turb_interp" = "red")) +
  labs(title = "Diel Average of FG_mean and FC_turb_interp",
       x = "Hour of Day",
       y = "Flux",
       color = "Variable",
       size = "Observations Lost") +
  theme_minimal()



plot(filt_df$FG_mean,filt_df$FC_turb_interp, xlim = c(-30,30), ylim = c(-20,20))
abline(a = 0, b = 1)
plot(Bad_df$FG_mean,Bad_df$FC_turb_interp, xlim = c(-30,30), ylim = c(-20,20))
abline(a = 0, b = 1)


################WILL SUPERBAD WORK USING TRACER GASES?##########################


#GET DATA
filt_df<- SITES_AE_9min_FILTER_BH$KONZ
df <- SITES_AE_9min$KONZ
df <- Super_Bad_Eddy(df,"EddyDiff",.03)


#Compare the same heights and the same gas
best_heights <- unique(filt_df$dLevelsAminusB)

df <- df %>% filter(dLevelsAminusB == best_heights[1] | dLevelsAminusB == best_heights[2])
Bad_df <- df %>% dplyr::filter( s_eddy_outliers == 0)

Gas = "CO2"
df_CO2 <- df %>% dplyr::filter(gas == Gas)
filt_df_CO2 <- filt_df %>% dplyr::filter(gas == Gas)

Gas="H2O"
df_H2O <- df %>% dplyr::filter(gas == Gas)
filt_df_H2O <- filt_df %>% dplyr::filter(gas == Gas)


###For the AE and WP method, K is the same for both gases, but Kec is different
#can filtering Kec of water also filter the Kec of CO2?

#Filtering CO2 to predict H2O
Bad_df_CO2 <- Bad_df %>% dplyr::filter(gas == "CO2")

# Filter df_H2O to keep only timestamps that exist in Bad_df_CO2
df_H2O_filtered <- df_H2O %>% filter(timeMid %in% Bad_df_CO2$timeMid)

plot(df_H2O_filtered$FH2O_interp,df_H2O_filtered$FG_mean)
abline(a = 0, b = 1)

plot(filt_df_H2O$FH2O_interp,filt_df_H2O$FG_mean)
abline(a = 0, b = 1)





###For the AE and WP method, K is the same for both gases, but Kec is different
#can filtering Kec of water also filter the Kec of CO2?

#Filtering H2O to predict CO2
Bad_df_H2O <- Bad_df %>% dplyr::filter(gas == "H2O")

# Filter df_H2O to keep only timestamps that exist in Bad_df_CO2
df_CO2_filtered <- df_CO2 %>% filter(timeMid %in% Bad_df_H2O$timeMid)

plot(df_CO2_filtered$FC_turb_interp,df_CO2_filtered$FG_mean, xlim = c(-40,40), ylim = c(-40,30))
abline(a = 0, b = 1)

plot(filt_df_CO2$FC_turb_interp,filt_df_CO2$FG_mean, xlim = c(-30,30), ylim = c(-20,20))
abline(a = 0, b = 1)

################################################################################
############################PURITAN APPROACH####################################
################################################################################


#GET DATA
filt_df<- SITES_AE_9min_FILTER_BH$KONZ
df <- SITES_AE_9min$KONZ

#Compare the same heights and the same gas
best_heights <- unique(filt_df$dLevelsAminusB)
df <- df %>% filter(dLevelsAminusB == best_heights[1] | dLevelsAminusB == best_heights[2])


#FILTER BY GAS
Gas = "CO2"
df_CO2 <- df %>% dplyr::filter(gas == Gas)
filt_df_CO2 <- filt_df %>% dplyr::filter(gas == Gas)

Gas="H2O"
df_H2O <- df %>% dplyr::filter(gas == Gas)
filt_df_H2O <- filt_df %>% dplyr::filter(gas == Gas)

####FILTER BY SUPER BAD EDDY, first we check getting H2O from filtered CO2

df_CO2 <- Super_Bad_Eddy(df_CO2,"EddyDiff",.1)
Bad_df_CO2 <- df_CO2 %>% dplyr::filter( s_eddy_outliers == 0)

# Filter df_H2O to keep only timestamps that exist in Bad_df_CO2
df_H2O_filtered <- df_H2O %>% filter(timeMid %in% Bad_df_CO2$timeMid)


#Check plot unfiltered, filtered by dConcNorm, filtered by Bad Eddy
plot(df_H2O$FH2O_interp,df_H2O$FG_mean, xlim = c(-2,12), ylim = c(-10,40))
abline(a = 0, b = 1)
print(rmse(df_H2O$FH2O_interp,df_H2O$FG_mean))

plot(filt_df_H2O$FH2O_interp,filt_df_H2O$FG_mean, xlim = c(-2,12), ylim = c(-10,40))
abline(a = 0, b = 1)
print(rmse(filt_df_H2O$FH2O_interp,filt_df_H2O$FG_mean))

plot(df_H2O_filtered$FH2O_interp,df_H2O_filtered$FG_mean, xlim = c(-2,12), ylim = c(-10,40))
abline(a = 0, b = 1)
rmse(df_H2O_filtered$FH2O_interp,df_H2O_filtered$FG_mean)


########################Getting CO2 from filtered H2O###########################


#GET DATA
filt_df<- SITES_AE_9min_FILTER_BH$HARV
df <- SITES_AE_9min$HARV

#Compare the same heights and the same gas
best_heights <- unique(filt_df$dLevelsAminusB)
df <- df %>% filter(dLevelsAminusB == best_heights[2])


#FILTER BY GAS
Gas = "CO2"
df_CO2 <- df %>% dplyr::filter(gas == Gas)
filt_df_CO2 <- filt_df %>% dplyr::filter(gas == Gas)

Gas="H2O"
df_H2O <- df %>% dplyr::filter(gas == Gas)
filt_df_H2O <- filt_df %>% dplyr::filter(gas == Gas)

####FILTER BY SUPER BAD EDDY, first we check getting H2O from filtered CO2

df_H2O <- Super_Bad_Eddy(df_H2O,"EddyDiff",.5)
Bad_df_H2O <- df_H2O %>% dplyr::filter( s_eddy_outliers == 0)

# Filter df_H2O to keep only timestamps that exist in Bad_df_CO2
df_CO2_filtered <- df_CO2 %>% filter(timeMid %in% Bad_df_H2O$timeMid)


#Check plot unfiltered, filtered by dConcNorm, filtered by Bad Eddy
plot(df_CO2$FC_turb_interp,df_CO2$FG_mean, xlim = c(-40,40), ylim = c(-60,40))
abline(a = 0, b = 1)
print(rmse(df_CO2$FC_turb_interp,df_CO2$FG_mean))

plot(filt_df_CO2$FC_turb_interp,filt_df_CO2$FG_mean, xlim = c(-40,40), ylim = c(-60,40))
abline(a = 0, b = 1)
print(rmse(filt_df_CO2$FC_turb_interp,filt_df_CO2$FG_mean))

plot(df_CO2_filtered$FC_turb_interp,df_CO2_filtered$FG_mean, xlim = c(-40,40), ylim = c(-60,40))
abline(a = 0, b = 1)
rmse(df_CO2_filtered$FC_turb_interp,df_CO2_filtered$FG_mean)


#############################DOES IT WORK FOR MBR###############################

##How do I know when to stop filtering? keep at least some %?
#For forested sites, we may be overestimating because we are in the roughness sublayer


# Function to iterate through values of x and compute RMSE & data retention
evaluate_x_rmse <- function(df, method, x_values) {
  results <- data.frame(x = numeric(), RMSE = numeric(), Data_Left = numeric())
  
  total_data <- nrow(df)  # Total number of data points before filtering
  
  for (x in x_values) {
    # Apply Super_Bad_Eddy function
    modified_df <- Super_Bad_Eddy(df, method, x)
    
    # Filter out outliers
    filtered_df <- modified_df %>% filter(s_eddy_outliers == 0)
    
    # Compute percent data retained
    percent_data_left <- (nrow(filtered_df) / total_data) * 100
    
    # Ensure there is data left after filtering
    if (nrow(filtered_df) > 0) {
      # Compute RMSE between FC_turb_interp and FG_mean
      rmse_value <- rmse(filtered_df$FC_turb_interp, filtered_df$FG_mean)
      
      # Store results
      results <- rbind(results, data.frame(x = x, RMSE = rmse_value, Data_Left = percent_data_left))
    } else {
      # If all data is filtered out, assign NA
      results <- rbind(results, data.frame(x = x, RMSE = NA, Data_Left = 0))
    }
  }
  
  return(results)
}

# Define range of x values
x_values <- seq(0, 2, by = 0.001)  # Adjust range as needed

# Run evaluation
rmse_results <- evaluate_x_rmse(df_CO2, "EddyDiff", x_values)

# Print results
print(rmse_results)

# Plot RMSE and % Data Left vs x
par(mar = c(5, 5, 4, 5))  # Adjust margins for second y-axis
plot(rmse_results$x, rmse_results$RMSE, type = "b", pch = 16, col = "blue",
     xlab = "Absolute Difference between KEC and Kcalc", ylab = "RMSE",
     main = "RMSE and % Data Left vs. x", ylim = range(rmse_results$RMSE, na.rm = TRUE))

# Add second y-axis for % data left
par(new = TRUE)
plot(rmse_results$x, rmse_results$Data_Left, type = "b", pch = 16, col = "red", 
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100))
axis(side = 4)  # Add right-side axis
mtext("% Data Left", side = 4, line = 3, col = "red")  # Label second y-axis

# Add legend
legend("topright", legend = c("RMSE", "% Data Left"), col = c("blue", "red"), pch = 16, lty = 1)
