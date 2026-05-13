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

# -------------------------------------------------------

# Step 1: Download NEON DATA: ####

# This section downloads data from NEON, reformat and aligns the concentration. Working group members can skip this and download aligned concentrations from the google drive. 

dir.create(paste(data.local.dir,"/NEON_Tower_Data", sep="" ))
setwd(paste(data.local.dir,"/NEON_Tower_Data", sep="")) # Data will be saved here
source( paste(DirRepo, '/workflows/flow.neon.data.download.R', sep=""))
# Unzip and Stack Data: 
source( paste(DirRepo, '/workflows/flow.neon.data.unzip.R', sep=""))
#flow.neon.data.extract.R:
source( paste(DirRepo, '/workflows/flow.neon.data.extract.V2.R', sep=""))

#flow.neon.data.format.conc.diffs.R`
#`flow.neon.data.format.conc.diffs.30m.R`

# -------------------------------------------------------
# If you skipped STEP 1: ####

# -------------------------------------------------------
# STEP 2: Gradient Flux Calculation ####
source( paste(DirRepo, '/workflows/flow.calc.flux.batch.R', sep=""))

localdir.savedata <- '/Volumes/MaloneLab/Research/FluxGradient/NEON_GradientFlux_Data'
localdir.ac <-  '/Volumes/MaloneLab/Research/FluxGradient/NEON_Aligned_Concentrations' 

source( paste(DirRepo, '/workflows/flow.evaluation.dataframe.R', sep="")) # Dataframe
source( paste(DirRepo, '/workflows/flow.evaluation.dataframe_EDI.R', sep="")) # Dataframe
# -------------------------------------------------------
# STEP 3: Storage Flux ####
source( paste(DirRepo, '/workflows/flow.neon.storage.R', sep=""))
# -------------------------------------------------------
# STEP 4: Canopy Complexity Workflow ####

# -------------------------------------------------------
# STEP 5: Format Data for Evaluation Workflow ####

#flow.evaluation.dataframe.R
# Edit this to also produce a site CSV that will be used in the Evaluation workflow:
