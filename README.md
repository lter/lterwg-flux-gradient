# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems


## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University


## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Current code workflow

1. **flow.downloadNEON.R** Workflow script that download and unzip NEON HDF5 (eddy covariance files) files for all sites and time periods of interest. ALSO downloads all required MET data products that are not in the bundled HDF5 file.
2. **flow.unzipNEON.R** Workflow. Unzips all downloaded NEON data files.
3. **flow.siteDF.R** Extract and stack downloaded and unzipped data into R objects for each data averaging interval, saved in their own RData file. These are currently min9.list (9-min concentrations), min30.list (30-min met and flux data), and min1.list (1-min met data), WS2D (2D wind speed data). Also extracts and saves site attributes from the HDF5 files into an R object called attr.df. Zips and saves objects to Google Drive (`googledrive::drive_upload`).
4. **flow.formatConcentrationDiffs** Grabs output from flow.siteDF.R from Google Drive. Align the 9-min concentration data among adjacent tower levels (and also the bottom-top levels). Interpolates 30-min eddy flux and MET data to the 9-min concentrations, including but not limited to u*, ubar (profile), roughness length. Also derives kinematic water flux (LE -> w'q'), heat flux (w'T'), aerodynamic canopy height, displacement height, that are needed for the various methods. Differences the concentrations for CH4, CO2, and H2O for adjacent tower levels (and bottom-top). Saves output as SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive.
5. **flow.computeGradientFluxes** Grab aligned concentration & flux data from Google Drive and calculate the fluxes using all methods (MBR, Aero, wind profile). Save output as SITE_METHOD_USER_DATE.RData, where SITE is NEON site code, METHOD is the computation method (e.g. MBR=modified bowen ratio, AE = aerodynamic, WP=wind profile), USER is the username, date is the run date (YYYY-MM-DD).

## Current (2023-10-20) assignments
- Workflow steps 1-3: Alexis. Due 2023-10-27
- Workflow step 4: Cove & Jackie. Due 2023-11-03
    - Testing by Kyle and David
- Workflow step 5:
    - MBR: Roisin, Cove, Jackie. Due 2023-12-06 meeting
    - Aero: Sam, Camilo, Alexis. Due 2023-12-06 meeting
    - WP: Sam, Camilo, Alexis. Due 2023-12-06 meeting
- Find available data with concurrent concentration profile and EC methane flux: Kyle. Due 2023-12-06 meeting
- Manuscripts
    - Methods paper(s): Which method works when and where? uncertainty.
    - Big-picture science paper: CH4 fluxes and controls

**NOTE**: Feel free to contact Nick and Angel during their office hours for coding/git help

## Guidelines for sharing scripts

- All code should include sufficient annotation and documentation so that other users can understand and run the scripts 
- Use RStudio projects to manage working directory
- Write your scripts such that other users can run the code without modifications. Keep file paths and other variables that might need to be changed at the beginning of the script, just after attaching the necessary libraries
- Check out the Scientific Computing Team’s best practice tips for [storing file paths](https://nceas.github.io/scicomp.github.io/best_practices.html#file-paths)
- Include an attribution header to your scripts or Rmarkdown documents

Example:

```r
## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: 
##
## Email: 
##
## ---------------------------

library(tidyverse)

```

## Supplementary Resources

NCEAS Scientific Computing Support Team page [link](https://nceas.github.io/scicomp.github.io)
