# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems


## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University

## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Data Frame Organization
- Column name should use snake case include units, last "_" proceeds units (i.e. var_molperm3)
- no "- or /" in column names

## Flux Workflow

1. **flow.NEON.data.download.R** Workflow script that download and unzip NEON HDF5 (eddy covariance files) files for all sites and time periods of interest. ALSO downloads all required MET data products that are not in the bundled HDF5 file.
2. **flow.NEON.data.unzip.R** Workflow. Unzips all downloaded NEON data files.
3. **flow.NEON.data.extract.R** Extract and stack downloaded and unzipped data into R objects for each data averaging interval, saved in their own RData file. These are currently min9.list (9-min/6-min concentrations), min30.list (30-min met and flux data), and min1.list (1-min met data), WS2D (2D wind speed data). Also extracts and saves site attributes from the HDF5 files into an R object called attr.df. Zips and saves objects to Google Drive. For example, `googledrive::drive_upload(media = path to the local file to upload, overwrite = T, path = googledrive::as_id("url to Drive folder"))`.
4. **flow.NEON.data.format.conc.diffs.R** Grabs output from flow.NEON.data.extract.R from Google Drive. Align the 9-min concentration data among adjacent tower levels (and also the bottom-top levels). Interpolates 30-min eddy flux and MET data to the 9-min/6-min concentrations, including but not limited to u*, ubar (profile), roughness length. Also derives kinematic water flux (LE -> w'q'), heat flux (w'T'), aerodynamic canopy height, displacement height, that are needed for the various methods. Differences the concentrations for CH4, CO2, and H2O for adjacent tower levels (and bottom-top). Saves output as SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive.
5.**flow.download.aligned_conc_flux.R** Grabs the output from **flow.NEON.data.format.conc.diffs.R** SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive. **Exploratory**
6.**flow.calc.flux.batch.R** Grab aligned concentration & flux data and calculates the fluxes using MBR (**flow.calc.flag.mbr.batch.R**), AE (**flow.calc.flag.aero.batch.R**), and WP (**flow.calc.flag.windprof.batch.R**) methods and adds quality flag columns, month, hour, residual, rmse for calculated fluxes. Save output as SITE_METHOD.RData, where SITE is NEON site code, METHOD is the computation method (e.g. MBR=modified bowen ratio, aero = aerodynamic, windprof=wind profile). **Exploratory**
7.**flow.validation.dataframe.batch.R** standardizes the data fromat from the MBR, AE, and WP. This files uses the product of **flow.calc.flux.batch.R**. This file produces a list of dataframes in an .rdata objected named SITES_METHOD_30min. **Exploratory**
8.**flow.evaluation.batch** Runs the analysis workflow for the list of dataframes for sites produced in **flow.validation.dataframe.batch.R**.
First data is filtered (**flow.evaluation.filter.R**) to produce: FilteredData_ALLSites.Rdata and FilterReport_ALLSites.Rdata. Next the One2One analysis (**flow.evaluation.One2One.R**) is done on filtered data to produce:  One2One_ALLSites.Rdata and FilteredData_ALLSites_BH.Rdata. BH stands for the best height, which is determined by the height levels with the highest R2. Next the diurnal analysis (flow.evaluation.diurnal.R) produces:DiurnalSummary_ALLSites_BH.Rdata. Finally, we fit carbon exchange parameters (flow.evaluation.cparms.R) to produce: CarbonParms.Rdata. **Exploratory**

## AOP Workflow

1.**flow.NEONAOP.Download.R** Downloads and mosaics NEON AOP Data. Data is stored locally on the Malone Lab server.
2. **flow.neon.site.squarebuffers.R** Creates square buffers for LTER-NEON co-located sites using this file: Ameriflux_NEON field-sites.csv to produce: NEONLTERsiteBuffers.Rdata.
3. **flow.neon.site.simplefeatures.R** This script uses the NEONLTERsiteBuffers.Rdata and breaks each buffer into wedges to produce: FG_Site_Wdges.RDATA.
4. **flow_AOP_FormatLayers** Extracts AOP information for each wedge to produce ...

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

## Supplementary Resources

NCEAS Scientific Computing Support Team page [link](https://nceas.github.io/scicomp.github.io)
