# Workflow:
library(tidyverse)
# Paths
data.local.dir <- '/Volumes/MaloneLab/Research/FluxGradient'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"


googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
# The Data 
data_folder <- googledrive::drive_ls(path = drive_url)


# Import Data:
metadata <- read.csv(paste( data.local.dir, '/Ameriflux_NEON field-sites.csv', sep="")) # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# Step 1: Download NEON DATA: 
setwd(paste(data.local.dir,"/NEON_Tower_Data", sep="")) # Data will be saved here
source( paste(DirRepo, '/workflows/flow.neon.data.download.R', sep=""))
# Unzip and Stack Data: 
source( paste(DirRepo, '/workflows/flow.neon.data.unzip.R', sep=""))
#flow.neon.data.extract.R:
source( paste(DirRepo, '/workflows/flow.neon.data.extract.R', sep=""))

# Step 2: 
#flow.neon.data.format.conc.diffs.R`
#`flow.neon.data.format.conc.diffs.30m.R`

# STEP 4: 
#flow.calc.flux.batch.R

# STEP 5: 
#flow.evaluation.dataframe.R

# Edit this to also produce a site CSV that will be used in the Evaluation workflow: