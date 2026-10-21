# Keep bootstrap warnings summarized instead of printing once per missing observation.
options(warn = 0)

DoWP <- 1 # Do Wind Profile Method here as well? 1 for true 0 for False.
# WP fluxes for the ESSD paper (Eq. 7) are calculated here from EddyDiff_WP (calc.eddydiff.aero), so
# flow.calc.flag.windprof.batch.R is not run in flow.calc.flux.batch.R and WP is exported only once.
Savecsv <- 0 # Save csv files to analyze in matlab? 1 for true 0 for False.

# Optional: specify path to save csv files for matlab
#path_for_matlab <- "Q:/My Drive/NC-State/flux_gradient/data"

# Load functions in this repo
source(file.path(gh_repo, "functions", "calc.MO.length.R"))
source(file.path(gh_repo, "functions", "calc.eddydiff.aero.R"))
source(file.path(gh_repo, "functions", "calc.gas.aero.windprof.flux.R"))
source(file.path(gh_repo, "functions", "calc.eqn.aero.windprof.flux.R"))
source(file.path(gh_repo, "functions", "calc.stability.correction.R"))
source(file.path(gh_repo, "functions", "calc.aerodynamic.canopy.height.R"))

# Calculate eddy diffusivity with the aerodynamic method
min9.K.AE.list <- calc.eddydiff.aero(sitecode = site, min9 = min9Diff.list)

#min30.K.AE.list <- calc.eddydiff.aero(sitecode = site, min9 = min30Diff.list)

# Compute aerodynamic flux gradient fluxes for all gases
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
min9.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.AE.list,
                                               bootstrap = 1, 
                                               nsamp = 1000)
#min30.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.AE.list,
#                                               bootstrap = 1, nsamp = 1000)

if (DoWP==1){
  # Apply Wind Profile Method
  
  # calc.gas.aero.windprof.flux.WP() was moved to deprecated/; the only difference was its default
  # eddy.diff.name, so the WP column is named explicitly here.
  min9.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.AE.list,
                                                 eddy.diff.name = "EddyDiff_WP",
                                                 bootstrap = 1, 
                                                 nsamp = 1000)
  # WP (Eq. 7): K_WP = k^2 (z - d) u / (phi_h ln((z - d)/z0)), z0 = 0.1 h, from calc.eddydiff.aero().
  # Store it as EddyDiff so downstream code (Bad_Eddy, cross-gradient, evaluation) uses the WP K;
  # the aerodynamic K is kept as EddyDiff_AE for reference.
  min9.FG.WP.list <- lapply(min9.FG.WP.list, function(d) {
    d$EddyDiff_AE <- d$EddyDiff
    d$EddyDiff <- d$EddyDiff_WP
    d
  })
  #min30.FG.WP.list <- calc.gas.aero.windprof.flux.WP(min9.K = min30.K.AE.list,
  #                                                bootstrap = 1, nsamp = 1000)
  
  fileSave <- fs::path(dirTmp, paste0(site, "_WP_9min.Rdata"))
  fileZip <- fs::path(dirTmp, paste0(site, "_WP_9min.zip"))
  save(min9.FG.WP.list, file = fileSave)
  utils::zip(zipfile = fileZip, files = fileSave)
  
  #googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work

  if (export_calc_google == 1){
    googledrive::drive_upload(media = fileSave, 
                              overwrite = T, 
                              path = data_folder$id[data_folder$name==site]) 
  }
}

# Save 9-minute 
fileSave <- fs::path(dirTmp, paste0(site, "_AE_9min.Rdata"))
fileZip <- fs::path(dirTmp, paste0(site, "_AE_9min.zip"))
save(min9.FG.AE.list, file = fileSave)
utils::zip(zipfile = fileZip, files = fileSave)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work

if (export_calc_google == 1){
  googledrive::drive_upload(media = fileSave, 
                            overwrite = T, 
                            path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)
}

# Save 30-minute
#fileSave <- fs::path(dirTmp,paste0(site,"_AE_30min.Rdata"))
#fileZip <- fs::path(dirTmp,paste0(site,"_AE_30min.zip"))
#save(min30.FG.AE.list,file=fileSave)
#setwd(dirTmp)
#utils::zip(zipfile=fileZip,files=paste0(site,"_AE_30min.Rdata"))
#setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
#googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

# Optional. Save csv to analyze in Matlab

if (Savecsv==1){
  ## AE Method
  # Min 9
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_AE_CO2_data_", site, ".csv"))
  write.csv(min9.FG.AE.list$CO2, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_AE_CH4_data_", site, ".csv"))
  write.csv(min9.FG.AE.list$CH4, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_AE_H2O_data_", site, ".csv"))
  write.csv(min9.FG.AE.list$H2O, MyFile, row.names = FALSE)
  
  # Min 30
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_AE_CO2_data_", site, ".csv"))
  write.csv(min30.FG.AE.list$CO2, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_AE_CH4_data_", site, ".csv"))
  write.csv(min30.FG.AE.list$CH4, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_AE_H2O_data_", site, ".csv"))
  write.csv(min30.FG.AE.list$H2O, MyFile, row.names = FALSE)
  
  ## WP Method
  # Min 9
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_WP_CO2_data_", site, ".csv"))
  write.csv(min9.FG.WP.list$CO2, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_WP_CH4_data_", site, ".csv"))
  write.csv(min9.FG.WP.list$CH4, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min9_WP_H2O_data_", site, ".csv"))
  write.csv(min9.FG.WP.list$H2O, MyFile, row.names = FALSE)
  
  # Min 30
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_WP_CO2_data_", site, ".csv"))
  write.csv(min30.FG.WP.list$CO2, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_WP_CH4_data_", site, ".csv"))
  write.csv(min30.FG.WP.list$CH4, MyFile, row.names = FALSE)
  
  MyFile = file.path(path_for_matlab, site, paste0("min30_WP_H2O_data_", site, ".csv"))
  write.csv(min30.FG.WP.list$H2O, MyFile, row.names = FALSE)
}
