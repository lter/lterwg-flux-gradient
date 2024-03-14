## ---------------------------------------------------- ##
            # Summarize High Frequency Data
## ---------------------------------------------------- ##
# Script Authors: Nick J Lyon, Kyle Delwiche

# Purpose:
## Aggregate 'high frequency' data to a user-determined level of temporal granularity

## ----------------------------------- ##
          # Housekeeping -----
## ----------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Force authentication with Google Drive
googledrive::drive_auth()

# Ensure data folder exists
dir.create(path = file.path("data"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------- ##
# ICOS 1 Second CH4 Summary ----
## ----------------------------------- ##

# Read in from local 



# End ----
