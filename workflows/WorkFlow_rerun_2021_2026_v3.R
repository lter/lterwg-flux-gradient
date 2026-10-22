## -------------------------------------------------- ##
#   Rerun of the 2021-2026 gradient fluxes: version 3
## -------------------------------------------------- ##
# Filled-in copy of Steps 5-7 of WorkFlow_master.R for the ESSD revision (essd-2026-413), Oct 2026.
# Version 3 includes: aerodynamic canopy height from the top-inlet height with d = 0.66 h and a 20-day
# window; MBR flux = median of the bootstrap draws (MBR output carries the AE/WP canopy height); k = 0.4
# everywhere; per-pair canopy.flag and zeta; WP diffusivity without the 0.1 m floor.
#
# Everything is written to NEW "_v3" folders so the published 2021-2026 results are not overwritten:
#   NEON_Aligned_Concentrations_2021_2026      published fluxes (unchanged)
#   NEON_Aligned_Concentrations_2021_2026_v3   stacked aligned inputs + v3 fluxes (SITE_{MBR,AE,WP}_9min)
#   NEON_GradientFlux_Data_2021_2026_v3        v3 SITE_Evaluation.RDATA
#   NEON_GradientFlux_EDI_2021_2026_v3         v3 EDI CSVs (the evaluation repo also writes the ensemble CSVs here)
# Then run lterwg-flux-gradient-eval/workflows/flow.evaluation.batch_V1.R (run.tag "_AA_AW_2021_2026_v3").
# Steps 1-4 (download, unzip, extract, align) do not need to be rerun.

library(tidyverse)

gh_repo  <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient"
data.dir <- "/Volumes/MaloneLab/Research/FluxGradient"

v3 <- list(
  ac.dirs          = file.path(data.dir, c("NEON_Aligned_Concentrations",                 # Aug 2021 - Jun 2024
                                           "NEON_Aligned_Concentrations_202407_202608")), # Jul 2024 - Aug 2026
  aligned_conc_dir = file.path(data.dir, "NEON_Aligned_Concentrations_2021_2026_v3"),
  eval_dir         = file.path(data.dir, "NEON_GradientFlux_Data_2021_2026_v3"),
  edi_dir          = file.path(data.dir, "NEON_GradientFlux_EDI_2021_2026_v3"))
for (d in v3[c("aligned_conc_dir", "eval_dir", "edi_dir")]) dir.create(d, showWarnings = FALSE, recursive = TRUE)
stopifnot(dir.exists(v3$ac.dirs))

metadata  <- read.csv(file.path(gh_repo, "metadata", "Ameriflux_NEON field-sites.csv"))
all.sites <- metadata$Site_Id.NEON %>% unique()
# To test first, run one short and one tall site: all.sites <- c("KONZ", "HARV")

## Step 5: gradient fluxes (periods stacked per site) -------------------------
ac.dirs <- v3$ac.dirs; aligned_conc_dir <- v3$aligned_conc_dir
save_calc_temp <- 0; export_calc_google <- 0
site.list <- all.sites
source(file.path(gh_repo, "workflows", "flow.calc.flux.batch.R"))
gc()

## Step 6: evaluation dataframes --------------------------------------------
eval_dir <- v3$eval_dir; export_eval_google <- 0
ustar.neon.sites <- read.csv(file.path(gh_repo, "metadata", "UstarNeonSites.csv"))
site.list <- all.sites
source(file.path(gh_repo, "workflows", "flow.evaluation.dataframe.R"))
gc()

## Step 7: EDI export --------------------------------------------------------
edi_dir <- v3$edi_dir
site.list <- all.sites
source(file.path(gh_repo, "workflows", "flow.evaluation.dataframe_EDI.R"))

message("v3 fluxes, evaluation files and EDI export done. Next: lterwg-flux-gradient-eval/workflows/flow.evaluation.batch_V1.R")
