## Taking the 1-second interval concentration data from SE-Deg and aggregating it to 30 minute
## Kyle Delwiche, March 26, 2025
library(data.table)
library(lubridate)
library(dplyr)

file_id2020 <- "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2020.csv"
file_id2021 <- "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2021.csv"
files <- c("concentration_2020", "concentration_2021", "concentration_2022", "concentration_2023")
files <- "concentration_2020"

# Read first 5 rows to identify columns
# for (file_id in files){

file_path <- paste0("C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/", files, ".csv")
print(file_path)
dt <- fread(file_path)


dt <- dt[-1]  # first row is units, need to change this

# Specify the column names you want to convert to numeric
columns_to_convert <- c("CO2", "H2O", "CH4","T_CELL","PRESS_CELL","FLOW_VOLRATE","FLOW_VOLRATE_IU")  # Replace with your actual column names

# Convert specified columns to numeric
dt[, (columns_to_convert) := lapply(.SD, as.numeric), .SDcols = columns_to_convert]


# header_sample[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]

# Proper solution with verification steps
data <- dt %>%
  # 1. Ensure TIMESTAMP is POSIXct datetime
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%

  # 2. Use standard quotes and verify unit argument
  mutate(timeBgn_A = floor_date(TIMESTAMP, unit = "30 minutes"))

# Summarize high-frequency data to 30-min
data_agg <- data %>%
  group_by(timeBgn_A) %>%
  summarise(across(c(CO2, H2O, CH4, LEVEL, T_CELL, PRESS_CELL, FLOW_VOLRATE, FLOW_VOLRATE_IU),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")
# rm(dt)
# gc()
# }
write.csv(data_agg, "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2020_30min.csv")