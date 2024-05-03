## ------------------------------------------------- ##
          # Make Non-NEON Attribute Tables
## ------------------------------------------------- ##
# Script author(s): Nick Lyon, Kyle Delwiche

# Purpose:
## Make attribute tables for non-NEON sites that are consistent with those of NEON sites

## ---------------------------- ##
      # Housekeeping ----
## ---------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls())

# Authorize Google Drive
googledrive::drive_auth()
## You'll need to select a pre-authorized email via the Console

# Make a vector of all column names found in an attribute table
attr_colnames <- c("DistZaxsCnpy", "DistZaxsDisp", "DistZaxsGrndOfst", 
                   "DistZaxsLvlMeasTow", "DistZaxsTow", "ElevRefeTow", 
                   "LatTow", "LonTow", "LvlMeasTow", "Pf.AngEnuXaxs", 
                   "Pf.AngEnuYaxs", "Pf.Ofst", "TimeDiffUtcLt", "TimeTube", 
                   "ZoneTime", "ZoneUtm", "TowerPosition", "Site")

## ---------------------------- ##
# Non-NEON Site 1 ----
## ---------------------------- ##

# Read in data
data <- read.csv(file = file.path())

# Glimpse data
dplyr::glimpse(data)

# Wrangle data to proper format for attribute table
attr.df <- data %>% 
  # Rename desired columns
  dplyr::rename() %>% 
  # Keep only those columns
  dplyr::select() %>% 
  # Keep only unique rows (i.e., refine to only attribute information)
  dplyr::distinct()

# Check structure
dplyr::glimpse(attr.df)

# Save as a .RData object

# Zip it

# Upload to Google Drive




# End ----
