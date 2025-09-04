The purpose of this file is to remind Sparkle what the hell is going on. Don't judge me, I am old.

1.**flow.NEON.data.download.R** Workflow script that download and unzip NEON HDF5 (eddy covariance files) files for all sites and time periods of interest. ALSO downloads all required MET data products that are not in the bundled HDF5 file.

2. **flow.NEON.data.unzip.R** Workflow. Unzips all downloaded NEON data files.

3. **flow.NEON.data.extract.R** Extract and stack downloaded and unzipped data into R objects for each data averaging interval, saved in their own RData file. These are currently min9.list (9-min/6-min concentrations), min30.list (30-min met and flux data), and min1.list (1-min met data), WS2D (2D wind speed data). Also extracts and saves site attributes from the HDF5 files into an R object called attr.df. Zips and saves objects to Google Drive. For example, `googledrive::drive_upload(media = path to the local file to upload, overwrite = T, path = googledrive::as_id("url to Drive folder"))`.

4. **flow.NEON.data.format.conc.diffs.R** Grabs output from flow.NEON.data.extract.R from Google Drive. Align the 9-min concentration data among adjacent tower levels (and also the bottom-top levels). Interpolates 30-min eddy flux and MET data to the 9-min/6-min concentrations, including but not limited to u*, ubar (profile), roughness length. Also derives kinematic water flux (LE -> w'q'), heat flux (w'T'), aerodynamic canopy height, displacement height, that are needed for the various methods. Differences the concentrations for CH4, CO2, and H2O for adjacent tower levels (and bottom-top). Saves output as SITE_aligned_conc_flux_9min.RData, where SITE is the NEON site code. Zips and uploads to Google Drive.

5. **flow.Download.GoogleDriveData.R** Downloads the SITE_aligned_conc_flux data from google drive.

6. **flow.calc.flux.batch.R** Calculates the gradient fluxes and aggregates datasetsby method and site.
Files are saved locally and on google drive. 

7. **flow.evaluations.R** Workflow to evaluate CO2 flues. This flow pulls in **FUNCTION_Filter.R**, **FUNCTION_One2One.R**, and **FUNCTION_DIURNAL.R**.

8. **flow.evaluations_H2O.R** ....

9. **flow.Heterogeneity_H2O.R** ....


# AOP Workflow:

1. **flow.NEONAOP.EVI.Download.R** - creates a list of available data at each site and download LAI, vegetation indices, and canopy height data

2. **flow.neon.site.squarebuffers.R** - Creates a sites simple feature and generates spatialbuffers.

3. **flow_AOP_FormatLayers.R** - Make site level raster tifs for AOP data of interest to be used in **Site.Spatial.Homo.R** .

4. **Site.Spatial.Homo.R** - Summarizes spectral and structural diversity of site foot prints.



