## ---------------------------------------------------- ##
          # Summarize ICOS High Frequency Data
## ---------------------------------------------------- ##
# Script Authors: Nick J Lyon, Kyle Delwiche

# Purpose:
## Aggregate 'high frequency' data to a user-determined level of temporal granularity
## Data provided by ICOS (Integrated Carbon Observing System)

## ----------------------------------- ##
          # Housekeeping -----
## ----------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Force authentication with Google Drive
googledrive::drive_auth()

# Ensure data folders exist
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "icos"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------- ##
# CH4 (1 Second) Summary ----
## ----------------------------------- ##

# Read in from local folder
icos_v0 <- read.csv(file = file.path("data", "icos", "concentration_2020-001.csv"))

# DELETE ME LATER ----
## Limit the number of rows to make it easier to develop this workflow
icos_df <- icos_v0[1:1000, ]

# Check structure
dplyr::glimpse(icos_df)

# Handle general formatting issues
icos_v2 <- icos_df[-1, ] %>% 
  # Make sure units are explicit
  dplyr::rename(TIMESTAMP_ymd_hms = TIMESTAMP,
                CO2_umolpermol = CO2,
                H2O_mmolpermol = H2O,
                CH4_nmolpermol = CH4,
                # LEVEL = LEVEL,
                T_CELL_degC = T_CELL,
                PRESS_CELL_kPa = PRESS_CELL,
                FLOW_VOLRATE_Lpermin = FLOW_VOLRATE,
                FLOW_VOLRATE_IU_mV = FLOW_VOLRATE_IU) %>% 
  # Fix class of timestamp column
  dplyr::mutate(TIMESTAMP_ymd_hms = as.POSIXct(x = TIMESTAMP_ymd_hms, 
                                               format = "%Y-%m-%d %H:%M:%S")) %>%
  # Fix class of numeric columns
  dplyr::mutate(dplyr::across(.cols = -TIMESTAMP_ymd_hms,
                              .fns = as.numeric))

# Re-check structure
dplyr::glimpse(icos_v2)

# Define the percentage of each minute of data to retain
perc_keep <- 50

# Drop desired percentage of each minute of sensor information
icos_v3 <- icos_v2 %>% 
  # Calculate difference in time (within tower levels)
  dplyr::group_by(LEVEL) %>% 
  dplyr::mutate(gap = as.numeric(difftime(time1 = TIMESTAMP_ymd_hms,
                                          time2 = min(TIMESTAMP_ymd_hms, na.rm = T),
                                          units = "secs")),
                minute_group = ifelse(test = (gap == 0),
                                      yes = 1, 
                                      no = ceiling(gap / 60))) %>%
  dplyr::ungroup() %>% 
  # Identify % of "minute group"
  dplyr::group_by(LEVEL, minute_group) %>% 
  dplyr::mutate(relative_gap = seq_along(gap),
                perc_val = (relative_gap / 60) * 100,
                perc_flag = ifelse(test = ((relative_gap / 60) * 100) >= perc_keep,
                                   yes = "keep", no = "drop")) %>% 
  dplyr::ungroup() %>% 
  # Filter out percentages beneath the desired percent threshold
  dplyr::filter(perc_flag != "drop")

# Check structure
dplyr::glimpse(icos_v3)


# Generate 30-minute groups






## tibble::view(icos_v2)

psych::multi.hist(x = icos_v2$relative_gap)



# End ----
