
DoWP = 1 # Do Wind Profile Method here as well? 1 for true O for False.
Savecsv = 0 # Save csv files to analyze in matlab? 1 for true 0 for False.

# Load functions in this repo
source(file.path("functions/MO_Length_CRS.R"))
source(file.path("functions/calc.eddydiff.aero.R"))
source(file.path("functions/calc.gas.aero.windprof.flux.R"))
source(file.path("functions/calc.gas.aero.windprof.flux_WP.R"))
source(file.path("functions/calc.eqn.aero.windprof.flux.R"))
source(file.path("functions/calculate.stability.correction.R"))
source(file.path("functions/calc.aerodynamic.canopy.height.R"))

min9 =min9Diff.list
# Calculate eddy diffusivity with the aerodynamic method
min9.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 =min9Diff.list)

#min30.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 = min30Diff.list)

# Compute aerodynamic flux gradient fluxes for all gases
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
min9.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.AE.list,bootstrap = 1, nsamp = 1000)
#min30.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.AE.list,
#                                               bootstrap = 1, nsamp = 1000)

if (DoWP==1){
# Apply Wind Profile Method
min9.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min9.K.AE.list,
                                               bootstrap = 1, nsamp = 1000)
#min30.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min30.K.AE.list,
#                                                bootstrap = 1, nsamp = 1000)
}


# Save 9-minute 
fileSave <- fs::path(dirTmp,paste0(site,"_AE_9min.Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_AE_9min.zip"))
save(min9.FG.AE.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_AE_9min.Rdata"))
setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

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

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_CO2_data_", site, ".csv")
write.csv(min9.FG.AE.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_CH4_data_", site, ".csv")
write.csv(min9.FG.AE.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_AE_H2O_data_", site, ".csv")
write.csv(min9.FG.AE.list$H2O, MyFile, row.names = FALSE)

# Min 30

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_CO2_data_", site, ".csv")
write.csv(min30.FG.AE.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_CH4_data_", site, ".csv")
write.csv(min30.FG.AE.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_AE_H2O_data_", site, ".csv")
write.csv(min30.FG.AE.list$H2O, MyFile, row.names = FALSE)

## WP Method
# Min 9

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_CO2_data_", site, ".csv")
write.csv(min9.FG.WP.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_CH4_data_", site, ".csv")
write.csv(min9.FG.WP.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min9_WP_H2O_data_", site, ".csv")
write.csv(min9.FG.WP.list$H2O, MyFile, row.names = FALSE)

# Min 30

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_CO2_data_", site, ".csv")
write.csv(min30.FG.WP.list$CO2, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_CH4_data_", site, ".csv")
write.csv(min30.FG.WP.list$CH4, MyFile, row.names = FALSE)

MyFile=paste0("Q:/My Drive/NC-State/flux_gradient/data/", site, "/min30_WP_H2O_data_", site, ".csv")
write.csv(min30.FG.WP.list$H2O, MyFile, row.names = FALSE)
}
