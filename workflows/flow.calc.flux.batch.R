## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Uses the aligned concentration file combined with the 30min and 9min data files to calculates fluxes and saves locally. 

# You must download the aligned concentration data using flow.download.aligned.conc.flux.R.
# Optional: if ac.dirs (a vector of aligned-concentration folders, one per NEON download period)
# is set in WorkFlow_master.R, the periods are stacked per site BEFORE fluxes are calculated, so the
# aerodynamic canopy height (and d, z0), stability terms and fluxes come from one continuous record.
# The stacked aligned files and the flux outputs are then written to aligned_conc_dir/SITE.

# Output(s):
# SITE_AE_9min.Rdata (local & Google Drive)
# SITE_AE_9min.zip (local)
# SITE_MBR_9min.RData (local)
# SITE_MBR_9min.zip (local & Google Drive)
# SITE_WP_9min.Rdata (local) - Eq. 7 wind-profile fluxes, written by flow.calc.flag.aero.batch.R
# SITE_WP_9min.zip (local & Google Drive)

# See WorkFlow_master.R to set the variables needed to run this script
# Variables needed: gh_repo, aligned_conc_dir, save_calc_temp, export_calc_google,
# data_folder (if export_calc_google == 1), site.list; optional: ac.dirs

# Load packages
library(fs)
library(googledrive)
library(dplyr)
library(stringr)
library(tidyverse)

## --------------------------------------------- ##
#         Gradient Flux Calculations -----
## --------------------------------------------- ##

for(site in site.list){
  
  if (save_calc_temp == 0){
    dirTmp <- file.path(aligned_conc_dir, site)
  } else if (save_calc_temp == 1){
    dirTmp <- fs::path(tempdir(), site)
  }
  dir.create(dirTmp, recursive = TRUE, showWarnings = FALSE)

  if (exists("ac.dirs") && length(ac.dirs) > 1) {
    # Load Data: stack every NEON download period in ac.dirs (per gas) and drop duplicate
    # windows at period boundaries, so the aerodynamic canopy height (d, z0), stability
    # terms and fluxes come from one continuous record (used for the 2021-2026 paper run).
    stack.periods <- function(file.suffix, obj.name, key.cols) {
      parts <- list()
      for (pd in ac.dirs) {
        f <- fs::path(pd, site, paste0(site, file.suffix))
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
    key.cols <- c("timeBgn_A", "timeBgn_B", "TowerPosition_A", "TowerPosition_B")
    min9Diff.list  <- stack.periods('_aligned_conc_flux_9min.RData',  'min9Diff.list',  key.cols)
    min30Diff.list <- stack.periods('_aligned_conc_flux_30min.RData', 'min30Diff.list', key.cols)
    if (is.null(min9Diff.list)) { message("Skipping ", site, ": no aligned 9-min data"); next }
    # keep the stacked aligned inputs next to the fluxes
    save(min9Diff.list, file = fs::path(dirTmp, paste0(site, '_aligned_conc_flux_9min.RData')))
    if (!is.null(min30Diff.list)) save(min30Diff.list, file = fs::path(dirTmp, paste0(site, '_aligned_conc_flux_30min.RData')))
  } else {
    # Load Data:
    load(fs::path(aligned_conc_dir, site, paste0(site, '_aligned_conc_flux_30min.RData')))
    load(fs::path(aligned_conc_dir, site, paste0(site, '_aligned_conc_flux_9min.RData')))
  }
  print('Data Loaded')
  
  print('Running MBR')
  source(file.path(gh_repo, "workflows", "flow.calc.flag.mbr.batch.R"))
  print('MBR Done')
  
  print('Running AE')
  source(file.path(gh_repo, "workflows", "flow.calc.flag.aero.batch.R"))
  print('AE Done')
  
  # WP fluxes (Eq. 7 of the ESSD paper: K_WP = k^2 (z - d) u / (phi_h ln((z - d)/z0)), with
  # d = 0.66 h and z0 = 0.1 h from the aerodynamic canopy height) are calculated and saved in
  # flow.calc.flag.aero.batch.R (DoWP = 1). flow.calc.flag.windprof.batch.R uses a different K_WP
  # (geometric-mean z without d, NEON roughLength_interp) and would overwrite SITE_WP_9min.Rdata,
  # so it is not run here.
  # print('Running WP')
  # source(file.path(gh_repo, "workflows", "flow.calc.flag.windprof.batch.R"))
  # print('WP Done')
  
  print('done')
  rm(list = intersect(c("min9", "min9Diff.list", "min30Diff.list"), ls())); gc()
}

message('Next run the flow.evaluation.dataframe.R')
