## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Uses the aligned concentration file combined with the 30min and 9min data files to calculates fluxes and saves locally. 

# You must download the aligned concentration data using flow.download.aligned.conc.flux.R.
# All data periods listed in ac.dirs (one aligned-concentration folder per NEON download period)
# are stacked per site BEFORE fluxes are calculated, so the aerodynamic canopy height (and d, z0),
# stability terms and fluxes come from one continuous record. The stacked aligned files and the
# flux outputs are written to localdir.ac/SITE. Set ac.dirs, run.tag, localdir.ac and upload.drive
# in WorkFlow_master.R; the defaults below reproduce the original single-period behaviour.

# Output(s):
# SITE_AE_9min.Rdata (local & Google Drive)
# SITE_AE_9min.zip (local)
# SITE_MBR_9min.RData (local)
# SITE_MBR_9min.zip (local & Google Drive)
# SITE_WP_9min.Rdata (local) - Eq. 7 wind-profile fluxes, written by flow.calc.flag.aero.batch.R
# SITE_WP_9min.zip (local & Google Drive)

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

# Add all sites here:
site.attributes <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Site_Attributes.csv') # has a list of all the sites

# Get unique sites
site.list <- site.attributes$Site %>% unique()

# Add local directory for downloaded data here:
data.root <- '/Volumes/MaloneLab/Research/FluxGradient' # MaloneLab Server
if (!exists("ac.dirs")) ac.dirs <- "NEON_Aligned_Concentrations"   # aligned-concentration period folders to stack
if (!exists("localdir.ac")) localdir.ac <- file.path(data.root, ac.dirs[1]) # stacked inputs + flux outputs go here
if (!exists("upload.drive")) upload.drive <- TRUE
dir.create(localdir.ac, showWarnings = FALSE)

localdir2 <- '/Volumes/MaloneLab/Research/FluxGradient/Attributes' # MaloneLab Server

# Add local directory for your Flux repo here:
if (!exists("DirRepo")) DirRepo <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
setwd(DirRepo)

## --------------------------------------------- ##
#               Authenticate -----
## --------------------------------------------- ##

email <- 'sparklelmalone@gmail.com'
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_auth(email = TRUE) 

# Authenticate with Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

# Data on google drive
data_folder <- if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_ls(path = drive_url) else data.frame(id = character(), name = character())

## --------------------------------------------- ##
#         Gradient Flux Calculations -----
## --------------------------------------------- ##

for(site in site.list){
  
  setwd(DirRepo)
  
  sitecode <- site
  print(sitecode)
  
  # Load Data: stack every period in ac.dirs (per gas), drop duplicate windows at period boundaries
  stack.periods <- function(file.suffix, obj.name, key.cols) {
    parts <- list()
    for (pd in ac.dirs) {
      f <- fs::path(data.root, pd, site, paste0(site, file.suffix))
      if (!file.exists(f)) { message("  no ", file.suffix, " for ", site, " in ", pd); next }
      e <- new.env(); load(f, envir = e); parts[[pd]] <- get(obj.name, envir = e)
    }
    if (length(parts) == 0) return(NULL)
    gases <- unique(unlist(lapply(parts, names)))
    out <- lapply(gases, function(g) {
      d <- dplyr::bind_rows(lapply(parts, function(x) x[[g]]))
      k <- intersect(key.cols, names(d))
      if (length(k) > 0) d <- d[!duplicated(d[, k]), ]
      d[order(d[[k[1]]]), ]
    })
    names(out) <- gases
    out
  }
  min9Diff.list  <- stack.periods('_aligned_conc_flux_9min.RData',  'min9Diff.list',
                                  c("timeBgn_A", "timeBgn_B", "TowerPosition_A", "TowerPosition_B"))
  min30Diff.list <- stack.periods('_aligned_conc_flux_30min.RData', 'min30Diff.list',
                                  c("timeBgn_A", "timeBgn_B", "TowerPosition_A", "TowerPosition_B"))
  if (is.null(min9Diff.list)) { message("Skipping ", site, ": no aligned 9-min data"); next }

  dirTmp <- file.path(localdir.ac, site)
  dir.create(dirTmp, showWarnings = FALSE)
  if (length(ac.dirs) > 1) {   # keep the stacked aligned inputs next to the fluxes
    save(min9Diff.list,  file = fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.RData')))
    if (!is.null(min30Diff.list)) save(min30Diff.list, file = fs::path(dirTmp, paste0(site, '_aligned_conc_flux_30min.RData')))
  }
    
  print('Data Loaded')
  
  print('Running MBR')
  source(file.path("workflows", "flow.calc.flag.mbr.batch.R"))
  print('MBR Done')
  
  setwd(DirRepo)
  print('Running AE')
  source(file.path("workflows", "flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  # WP fluxes (Eq. 7, using z - d and z0 = 0.1 h) are calculated and saved in flow.calc.flag.aero.batch.R
  # (DoWP = 1). flow.calc.flag.windprof.batch.R used a different K_WP (geometric-mean z without d,
  # NEON roughLength_interp) and overwrote SITE_WP_9min.Rdata, so it is no longer run.
  # setwd(DirRepo)
  # print('Running WP')
  # source(file.path("workflows", "flow.calc.flag.windprof.batch.R"))
  # print('WP Done')
  
  print('done')
  rm(min9, min9Diff.list, min30Diff.list); gc()
}

message('Next run the flow.evaluation.dataframe.R')
