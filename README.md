# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems


## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University

## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Repository Structure

```
lterwg-flux-gradient/
├── functions/               # Core analysis functions
│   ├── calc.*.R             # Calculation functions
│   ├── flag.*.R             # Quality control functions
│   ├── plot.*.R             # Visualization functions
│   └── compile.*.R          # Data processing functions
│
├── workflows/               # Complete analysis workflows
│   ├── flow.neon.data.*.R   # Data acquisition workflows
│   ├── flow.calc.*.R        # Flux calculation workflows
│   └── flow.*.R             # Analysis workflows
│
├── exploratory/             # Preliminary analyses and development
│   └── ...
│
└── deprecated/              # Unused workflows and functions
```

## Data Frame Organization
- Column name should use snake case include units, last "_" proceeds units (i.e. var_molperm3)
- no "- or /" in column names

## Getting Started

1. Clone this repository
   ```bash
   git clone https://github.com/lter/lterwg-flux-gradient.git
   cd lterwg-flux-gradient
   ```

2. Install required R packages
   ```r
   # Core packages
   install.packages(c("tidyverse", "neonUtilities", "rhdf5", "googledrive", 
                     "foreach", "doParallel", "lubridate", "ggplot2"))
   
   # Additional packages
   install.packages(c("gslnls", "terra", "sf", "ggh4x"))
   
   # If using BiocManager
   if (!requireNamespace("BiocManager", quietly = TRUE))
       install.packages("BiocManager")
   BiocManager::install("rhdf5")
   ```

## Main Flux Workflow

### Data Acquisition and Processing

1. `flow.neon.data.download.R` → Workflow script that downloads and unzips NEON HDF5 (eddy covariance files) files for all sites and time periods of interest. ALSO downloads all required MET data products that are not in the bundled HDF5 file.

2. `flow.neon.data.unzip.R` → Unzips all downloaded NEON data files.

3. `flow.neon.data.extract.R` → Extracts and stacks downloaded and unzipped data into R objects for each data averaging interval, saved in their own RData file. These are currently min9.list (9-min/6-min concentrations), min30.list (30-min met and flux data), and min1.list (1-min met data), WS2D (2D wind speed data). Also extracts and saves site attributes from the HDF5 files into an R object called attr.df. Zips and saves objects to Google Drive. For example, `googledrive::drive_upload(media = path to the local file to upload, overwrite = T, path = googledrive::as_id("url to Drive folder"))`.

   ```
   NEON Data → flow.neon.data.download.R → flow.neon.data.unzip.R → flow.neon.data.extract.R 
                                                                   → min9.list, min30.list, min1.list, attr.df (saved as RData files)
   ```

### Concentration Processing

4. `flow.neon.data.format.conc.diffs.R` & `flow.neon.data.format.conc.diffs.30m.R` → Grabs output from `flow.neon.data.extract.R` from Google Drive. Align the 9-min or 30-min concentration data among adjacent tower levels (and also the bottom-top levels). The base conc.diffs file interpolates 30-min eddy flux and MET data to the 9-min/6-min concentrations, including but not limited to u*, ubar (profile), roughness length. The conc.diff.30m file connects the nearest 9-min/6-min data to each 30-min eddy covariance measurement. Also derives kinematic water flux (LE -> w'q'), heat flux (w'T'), aerodynamic canopy height, displacement height, that are needed for the various methods. Differences the concentrations for CH4, CO2, and H2O for adjacent tower levels (and bottom-top). Saves output as SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive.

5. `flow.download.aligned.conc.flux.R` → Grabs the output from `flow.neon.data.format.conc.diffs.R` SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive.

   ```
   min9.list, min30.list, min1.list → flow.neon.data.format.conc.diffs.R → SITE_aligned_conc_flux_9min.RData
                                   → flow.neon.data.format.conc.diffs.30m.R → SITE_aligned_conc_flux_30min.RData
   
   SITE_aligned_conc_flux_9min.RData → flow.download.aligned.conc.flux.R (downloads aligned data)
   ```

### Exploratory Workflows

6. `flow.calc.flux.batch.R` → Grabs aligned concentration & flux data and calculates the fluxes using MBR (`flow.calc.flag.mbr.batch.R`), AE (`flow.calc.flag.aero.batch.R`), and WP (`flow.calc.flag.windprof.batch.R`) methods and adds quality flag columns, month, hour, residual, rmse for calculated fluxes. Save output as SITE_METHOD.RData, where SITE is NEON site code, METHOD is the computation method (e.g. MBR=modified bowen ratio, aero = aerodynamic, windprof=wind profile). 

7. `flow.evaluation.dataframe.R` → Standardizes the data format from the MBR, AE, and WP. This file uses the product of `flow.calc.flux.batch.R` to develop the validation dataframes needed to perform the evaluation. This file produces a list of dataframes in an object called SITE_Evaluation.RData.

## Other Workflows

### Non-NEON Processing
`flow.non.neon.attribute.tables.R` → Creates attribute tables for non-NEON sites that are consistent with those of NEON sites. Uploads to Google Drive as SITE_attr.RData and SITE_attr.zip where SITE is the non-NEON site.

`flow.non.neon.data.harmonize.ch4.R` → Harmonizes methane data from non-NEON sites. Uploads to Google Drive as methane_non-neon_harmonized.csv.

`flow.SE-Sto.data.format.conc.diffs.R` → Merges together flux, met, and profile concentration data for site SE-Sto. Aligns the profile concentration data (CH4, CO2, and H2O) among adjacent tower levels (and also the bottom-top levels) and computes the difference in mean concentration. Aligns non-concentration data with the mid-point of the paired-level concentration differences. 

`flow.SE-Svb.data.format.conc.diffs.R` → Merges together flux, met, and profile concentration data for site SE-Svb. Aligns the profile concentration data (CH4, CO2, and H2O) among adjacent tower levels (and also the bottom-top levels) and computes the difference in mean concentration. Aligns non-concentration data with the mid-point of the paired-level concentration differences. 

`flow.US-Uaf.data.format.conc.diffs.R` → Merges together flux, met, and profile concentration data for site US-Uaf. Aligns the profile concentration data (CH4, CO2, and H2O) among adjacent tower levels (and also the bottom-top levels) and computes the difference in mean concentration. Aligns non-concentration data with the mid-point of the paired-level concentration differences. 

### Misc

`flow.eval.fluxes.MBR.bootstrap.R` → Computes diel averages after filtering the MBR fluxes for tracer concentrations that are close to zero.

`flow.eval.plots.R` → Creates linear 1 to 1 plots across all sites and bar plots of variable across all sites. Also plots light response curves for daytime CO2 vs daytime PAR for FG and EC calculated fluxes, and temperature response curves for nighttime CO2 vs nighttime air temperature for FG and EC calculated fluxes. Plots diurnal averages for all sites.

`flow.flag.flux.stats.R` → Runs quality flag functions and calculates residuals. Uploads to Google Drive as SITES_WP_val.Rdata, SITES_AE_val.Rdata, and SITES_MBR_val.zip. 

`flow.icos.data.1sec.summarize.R` → Aggregates ICOS high frequency data.

## Function Folder
- Use hierarchical naming with the active verb first, i.e. "flag.iqr.R"
- This is where all functions called by flow. scripts in Workflow Folder are stored

## Exploratory Folder
- Wild West, this is where preliminary functions and workflows are stored

## Deprecated Folder
- This is where unused workflows and functions are stored

**NOTE**: Feel free to contact Nick and Angel during their office hours for coding/git help

## NEON Data Products
ADD LINKS TO NEON DATA PRODUCTS PAGE

## Related Repositories

- [lterwg-flux-gradient-eval](https://github.com/lter/lterwg-flux-gradient-eval): Code for the evaluation paper
- [lterwg-flux-gradient-methane](https://github.com/lter/lterwg-flux-gradient-methane): Code for the methane paper

## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)
