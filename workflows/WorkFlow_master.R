# Workflow:
library(tidyverse)
# Set run.steps <- 2L before sourcing to run only fluxes, evaluation frames and EDI.
if (!exists("run.steps")) run.steps <- c(1L, 2L, 3L)


# Paths
data.local.dir <- '/Volumes/MaloneLab/Research/FluxGradient'
DirRepo <-"/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"

# Data periods and outputs ----------------------------------------------------
# One aligned-concentration folder per NEON download period. Step 2 stacks them per site so the
# fluxes, aerodynamic canopy height (d, z0) and every later step use one continuous record.
ac.dirs <- c("NEON_Aligned_Concentrations",                # Aug 2021 - Jun 2024
             "NEON_Aligned_Concentrations_202407_202608")  # Jul 2024 - Aug 2026
run.tag <- "_2021_2026"
localdir.ac       <- file.path(data.local.dir, paste0("NEON_Aligned_Concentrations", run.tag)) # stacked inputs + SITE_{MBR,AE,WP}_9min
localdir.savedata <- file.path(data.local.dir, paste0("NEON_GradientFlux_Data", run.tag))      # SITE_Evaluation.RDATA
localdir.edi      <- file.path(data.local.dir, paste0("NEON_GradientFlux_EDI", run.tag))       # EDI csv export
upload.drive <- FALSE   # TRUE overwrites the per-site files on the shared Google Drive
for (d in c(localdir.ac, localdir.savedata, localdir.edi)) dir.create(d, showWarnings = FALSE)
# To redo a single period: ac.dirs <- "NEON_Aligned_Concentrations"; run.tag <- ""

if (upload.drive) googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 

# The Data 
data_folder <- if (upload.drive) googledrive::drive_ls(path = drive_url) else data.frame(id = character(), name = character())


# Import Data:
metadata <- read.csv(paste( data.local.dir, '/Ameriflux_NEON field-sites.csv', sep="")) # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique

# -------------------------------------------------------

# Step 1: Download NEON DATA: ####

# This section downloads data from NEON, reformat and aligns the concentration. Working group members can skip this and download aligned concentrations from the google drive. 

if (1L %in% run.steps) {
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
}
# If you skipped STEP 1: ####

# -------------------------------------------------------
# STEP 2: Gradient Flux Calculation ####
if (2L %in% run.steps) {
setwd(DirRepo)
source( paste(DirRepo, '/workflows/flow.calc.flux.batch.R', sep=""))

# localdir.ac / localdir.savedata / localdir.edi are set in the "Data periods and outputs" block above

setwd(DirRepo)
source( paste(DirRepo, '/workflows/flow.evaluation.dataframe.R', sep="")) # Dataframe
source( paste(DirRepo, '/workflows/flow.evaluation.dataframe_EDI.R', sep="")) # Dataframe
# -------------------------------------------------------
# STEP 3: Storage Flux ####
}
if (3L %in% run.steps) {
setwd(DirRepo)
source( paste(DirRepo, '/workflows/flow.neon.storage.R', sep=""))

# I can bypass running the whole script and just complie the site files:
source("workflows/flow.neon.storage.compile.R")
}
 
# -------------------------------------------------------
# STEP 4: Canopy Complexity Workflow ####

# -------------------------------------------------------
# STEP 5: Format Data for Evaluation Workflow ####

#flow.evaluation.dataframe.R
# Edit this to also produce a site CSV that will be used in the Evaluation workflow:
