# Load required libraries
library(googledrive)
library(fs)


# Authenticate with Google Drive
email <- 'sam.jurado@yale.edu'
googledrive::drive_auth(email = email)

# Define Google Drive file IDs for required data files
file_ids <- list(
 # "SITES_AE_9min.Rdata" = "12nE75Zh-tQ8oXOXARj2_3J2HjA6Lf1wl",
#  "SITES_WP_9min.Rdata"= "17Y-QYKSof3nMOAz6L8QsCxtLSoqObvn-",
 # "FilteredData_MS1Sites.Rdata" = "1fJ_7lG5WO6lyuYVkhaAhyv1uZCPn4EXw",
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

df <- df %>% filter(dLevelsAminusB == best_heights[1] | dLevelsAminusB == best_heights[2])

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

#tk

















