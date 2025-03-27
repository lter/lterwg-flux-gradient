## Taking the 1-second interval concentration data from SE-Deg and aggregating it to 30 minute
## Kyle Delwiche, March 26, 2025
library(data.table)
library(lubridate)
library(dplyr)

# file_id2020 <- "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2020.csv"
# file_id2021 <- "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2021.csv"
# files <- c("concentration_2020", "concentration_2021", "concentration_2022", "concentration_2023")
# files <- "concentration_2020"
# 
# # Read first 5 rows to identify columns
# # for (file_id in files){
# 
# file_path <- paste0("C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/", files, ".csv")
# print(file_path)
# dt <- fread(file_path)
# 
# 
# dt <- dt[-1]  # first row is units, need to change this
# 
# # Specify the column names you want to convert to numeric
# columns_to_convert <- c("CO2", "H2O", "CH4","T_CELL","PRESS_CELL","FLOW_VOLRATE","FLOW_VOLRATE_IU")  # Replace with your actual column names
# 
# # Convert specified columns to numeric
# dt[, (columns_to_convert) := lapply(.SD, as.numeric), .SDcols = columns_to_convert]


# header_sample[, TIMESTAMP := as.POSIXct(TIMESTAMP, tz = "UTC")]



Sys.setenv(tz="UTC")

### Read in csv file
file_path <- "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2020.csv"
data1.raw <- fread(file_path)

data1.raw <- data1.raw[-1]  # first row is units, need to drop this

data2 <- data1.raw %>%
  mutate(
    DateTime = as.POSIXct(TIMESTAMP, tz = "UTC"),
    Date = as.Date(DateTime))

# Convert all possible numeric columns
data1 <- data2 %>% 
  mutate(across(where(is.character), ~ as.numeric(.)))

# Identify transition points for LEVEL changes
end_time <- data1$DateTime[which(diff(data1$LEVEL) != 0)]
time.avg.interval = 30
#head(end_time)

# Compute statistics efficiently
mean_sum_all <- lapply(end_time, function(t) {
  subset_data <- data1 %>%
    filter(DateTime > (t - time.avg.interval) & DateTime < t)
  
  if (nrow(subset_data) > 0) {
    subset_data %>%
      summarise(
        Date = as.POSIXct(mean(as.numeric(DateTime), na.rm=TRUE),origin="1970-01-01",tz="UTC"), 
        Start_Time = format(as.POSIXct(min(as.numeric(DateTime), na.rm=TRUE),
                                       origin="1970-01-01",tz="UTC"), "%H:%M:%S"),
        End_Time = format(as.POSIXct(max(as.numeric(DateTime), na.rm=TRUE),
                                     origin="1970-01-01",tz="UTC"), "%H:%M:%S"),
        LEVEL = mean(LEVEL, na.rm=TRUE),
        CO2.mean = mean(CO2, na.rm=TRUE),
        CH4.mean = mean(CH4, na.rm=TRUE),
        H2O.mean = mean(H2O, na.rm=TRUE),
        CO2.sd = sd(CO2, na.rm=TRUE),
        CH4.sd = sd(CH4, na.rm=TRUE),
        H2O.sd = sd(H2O, na.rm=TRUE),
        CO2.n = sum(!is.na(CO2)),
        CH4.n = sum(!is.na(CH4)),
        H2O.n = sum(!is.na(H2O))
      )
  }
}) %>%
  bind_rows()  # Combine results into a data frame

# View result
print(mean_sum_all)





# # Proper solution with verification steps
# data <- dt %>%
#   # 1. Ensure TIMESTAMP is POSIXct datetime
#   mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
# 
#   # 2. Use standard quotes and verify unit argument
#   mutate(timeBgn_A = floor_date(TIMESTAMP, unit = "30 minutes"))
# 
# # Summarize high-frequency data to 30-min
# data_agg <- data %>%
#   group_by(timeBgn_A) %>%
#   summarise(across(c(CO2, H2O, CH4, LEVEL, T_CELL, PRESS_CELL, FLOW_VOLRATE, FLOW_VOLRATE_IU),
#                    ~ mean(.x, na.rm = TRUE)), .groups = "drop")
# # rm(dt)
# gc()
# }
write.csv(data_agg, "C:/Users/kyled/Dropbox/Research/NCEAS_flux gradient/Data/SE-Deg/concentration_2020_30min.csv")