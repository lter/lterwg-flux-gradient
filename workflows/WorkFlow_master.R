## -------------------------------------------------- ##
#                  Housekeeping -----
## -------------------------------------------------- ##
# Purpose:
# This master script will run the main workflow scripts to 
# download, unzip, extract, format, and calculate gradient fluxes in order.

# FIRST-TIME INSTRUCTIONS:
# Replace every [CTRL+F INSERT ... HERE] instance with your own existing absolute file paths!
# This workflow involves many different locations, so make sure your paths are absolute (not relative)!
# You can control+F to find and replace every [CTRL+F INSERT ... HERE] instance with your own existing paths or info.
# Since this is a long workflow, it is more convenient to set the paths at each step (even if there are repeated paths) 
# so you can easily jump in where you left off in the workflow.

# Throughout the workflow, inner folders will be automatically created under your listed paths.

## -------------------------------------------------- ##
#       Step 1: flow.neon.data.download.R -----
## -------------------------------------------------- ##

# This step downloads data from NEON. 
# Working group members can skip this and download aligned concentrations from the Google Drive. 

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to folder where NEON data will be downloaded and extracted
# Note: an inner "data" folder will be created under download_extract_dir 
download_extract_dir <- "[CTRL+F INSERT DOWNLOAD DIR HERE]"

# Grab list of sites
# Also found at /Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

my_startdate <- "2021-08"
my_enddate <- "2024-06"
# Set your NEON token by creating an account at https://www.neonscience.org/
# Then go to the "My Account" page to copy your API Token
neon_token <- "[INSERT YOUR OWN TOKEN HERE, DO NOT COMMIT YOUR TOKEN TO GITHUB]"

# Download the data
source(file.path(gh_repo, "workflows", "flow.neon.data.download.R"))

## -------------------------------------------------- ##
#         Step 2: flow.neon.data.unzip.R ----- 
## -------------------------------------------------- ##

# This step unzips the downloaded data from NEON. 
# Working group members can skip this and download aligned concentrations from the Google Drive. 

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to folder where NEON data will be downloaded and extracted
download_extract_dir <- "[CTRL+F INSERT DOWNLOAD DIR HERE]"

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

# Unzip and stack data
source(file.path(gh_repo, "workflows", "flow.neon.data.unzip.R"))

## -------------------------------------------------- ##
#       Step 3: flow.neon.data.extract.v2.R ----- 
## -------------------------------------------------- ##

# This step extracts the downloaded data from NEON.
# Working group members can skip this and download aligned concentrations from the Google Drive. 

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to folder where NEON data will be downloaded and extracted
download_extract_dir <- "[CTRL+F INSERT DOWNLOAD DIR HERE]"

# Do you want to export the extracted 1min, 9min, 30min, attr, WS2D2min to Google Drive? 
# Set 1 for yes, 0 for no
export_extracted_google <- 0

if (export_extracted_google == 1){
  # Set your Google Drive email
  # Or you can set my_email <- TRUE if you want to manually authorize an email
  my_email <- "[CTRL+F INSERT EMAIL HERE]"
  
  googledrive::drive_auth(email = my_email) 
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
  data_folder <- googledrive::drive_ls(path = drive_url)
}

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

# Extract to 1min, 9min, 30min, attr, WS2D2min
source(file.path(gh_repo, "workflows", "flow.neon.data.extract.v2.R"))

## -------------------------------------------------- ##
# Step 4: flow.neon.data.format.conc.diffs.R -----
#         flow.neon.data.format.conc.diffs.30m.R -----
## -------------------------------------------------- ##

# This step aligns the concentration data

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Do you need to download the 1min, 9min, 30min, attr, WS2D2min data from Google Drive?
# This would download the data to a temporary directory.
# Set 1 for yes I need to download, 0 for no I can use local data
download_google <- 0

if (download_google == 0){
  # Path to downloaded and extracted NEON data folder
  download_extract_dir <- "[CTRL+F INSERT DOWNLOAD DIR HERE]"
  
} 

# Do you want to save the concentration data to a temp directory?
# Set 1 for yes I want to save it to a temp directory (files will disappear unless I also export to Google Drive),
# Set 0 for no I want to save it to a permanent directory so I can have it on my own computer forever
save_temp <- 0

if (save_temp == 0){
  # Path to folder where aligned concentration will be saved
  aligned_conc_dir <- "[CTRL+F INSERT ALIGNED CONC DIR HERE]"
}

# Do you want to export the aligned concentration data to Google Drive?
# Set 1 for yes I want to export it to Google Drive, 0 for no
export_aligned_google <- 0

if (download_google == 1 | export_aligned_google == 1){
  # Set your Google Drive email
  # Or you can set my_email <- TRUE if you want to manually authorize an email
  my_email <- "[CTRL+F INSERT EMAIL HERE]"
  
  googledrive::drive_auth(email = my_email) 
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
  data_folder <- googledrive::drive_ls(path = drive_url)
}

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

# Align 9min concentration data
source(file.path(gh_repo, "workflows", "flow.neon.data.format.conc.diffs.R"))

# Align 30min concentration data
source(file.path(gh_repo, "workflows", "flow.neon.data.format.conc.diffs.30m.R"))

## -------------------------------------------------- ##
#        Step 5: flow.calc.flux.batch.R -----
## -------------------------------------------------- ##

# This step calculates the gradient fluxes

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to aligned concentration folder
aligned_conc_dir <- "[CTRL+F INSERT ALIGNED CONC DIR HERE]"

# Do you want to save the calculated fluxes to a temp directory?
# Set 1 for yes I want to save it to a temp directory (files will disappear unless I also export to Google Drive),
# Set 0 for no I want to save it to aligned_conc_dir so I can have it on my own computer forever
save_calc_temp <- 0

# Do you want to export the calculated fluxes concentration data to Google Drive?
# Set 1 for yes I want to export it to Google Drive, 0 for no
export_calc_google <- 0

if (export_calc_google == 1){
  # Set your Google Drive email
  # Or you can set my_email <- TRUE if you want to manually authorize an email
  my_email <- "[CTRL+F INSERT EMAIL HERE]"
  
  googledrive::drive_auth(email = my_email) 
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
  data_folder <- googledrive::drive_ls(path = drive_url)
}

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

# Calculate gradient fluxes
source(file.path(gh_repo, "workflows", "flow.calc.flux.batch.R"))

## -------------------------------------------------- ##
#      Step 6: flow.evaluation.dataframe.R ----- 
## -------------------------------------------------- ##

# This step creates the validation dataframes needed for evaluation

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to aligned concentration folder
aligned_conc_dir <- "[CTRL+F INSERT ALIGNED CONC DIR HERE]"

# Path to folder where evaluation data will be saved
eval_dir <- "[CTRL+F INSERT EVAL DIR HERE]"

# Do you want to export the evaluation data to Google Drive?
# Set 1 for yes I want to export it to Google Drive, 0 for no
export_eval_google <- 0

if (export_eval_google == 1){
  # Set your Google Drive email
  # Or you can set my_email <- TRUE if you want to manually authorize an email
  my_email <- "[CTRL+F INSERT EMAIL HERE]"
  
  googledrive::drive_auth(email = my_email) 
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
  data_folder <- googledrive::drive_ls(path = drive_url)
}

# Ustar Threshold:
# Also found at /Volumes/MaloneLab/Research/FluxGradient/UstarNeonSites.csv
ustar.neon.sites <- read.csv(file.path(gh_repo, "UstarNeonSites.csv"))

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

source(file.path(gh_repo, "workflows", "flow.evaluation.dataframe.R"))

## -------------------------------------------------- ##
#      Step 7: flow.evaluation.dataframe_EDI.R ----- 
## -------------------------------------------------- ##

# This step formats the evaluation data for publication on EDI

# Load packages
library(tidyverse)
# Clear workspace
rm(list=ls())

# Path to lterwg-flux-gradient GitHub repo
gh_repo <- "[CTRL+F INSERT GH REPO HERE]"

# Path to evaluation data folder
eval_dir <- "[CTRL+F INSERT EVAL DIR HERE]"

# Path to folder where the EDI-ready data will be saved
edi_dir <- "[CTRL+F INSERT EDI DIR HERE]"

# Grab list of sites
metadata <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
site.list <- metadata$Site_Id.NEON %>% unique()

source(file.path(gh_repo, "workflows", "flow.evaluation.dataframe_EDI.R"))

# -------------------------------------------------------
# Storage Flux ####
# source(file.path(gh_repo, "workflows", "flow.neon.storage.R"))

# -------------------------------------------------------
# Canopy Complexity Workflow ####
# ...