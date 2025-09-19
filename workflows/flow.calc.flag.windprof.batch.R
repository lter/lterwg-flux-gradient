
# Load functions for the wind profile flux gradient calculation
source(file.path("functions", "calc.MO.length.R"))
source(file.path("functions", "calc.eddydiff.windprof.R"))
source(file.path("functions", "calc.gas.aero.windprof.flux.R"))
source(file.path("functions", "calc.eqn.aero.windprof.flux.R"))
source(file.path("functions", "calc.stability.correction.R"))
source(file.path("functions", "calc.aerodynamic.canopy.height.R"))

# Calculate eddy diffusivity with the wind profile method
min9.K.WP.list <- calc.eddydiff.windprof(site = site, min9 = min9Diff.list)
#min30.K.WP.list <- calc.eddydiff.windprof(site = site, min9 = min30Diff.list)

# Compute wind profile flux gradient fluxes for all gases.
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"

min9.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.WP.list, 
                                               bootstrap = 1, 
                                               nsamp = 1000)
#min30.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.WP.list, 
#                                               bootstrap = 1, nsamp=1000)

# #Upload to Google Drive
# Save 9-minute 
fileSave <- fs::path(dirTmp, paste0(site, "_WP_9min.Rdata"))
fileZip <- fs::path(dirTmp, paste0(site, "_WP_9min.zip"))
save(min9.FG.WP.list, file = fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile = fileZip, files = paste0(site, "_WP_9min.Rdata"))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, 
                          overwrite = T, 
                          path = data_folder$id[data_folder$name==site]) 

# Save 30-minute
#fileSave <- fs::path(dirTmp,paste0(site,"_WP_30min.Rdata"))
#fileZip <- fs::path(dirTmp,paste0(site,"_WP_30min.zip"))
#save(min30.FG.WP.list,file=fileSave)
#wdPrev <- getwd()
#setwd(dirTmp)
#utils::zip(zipfile=fileZip,files=paste0(site,"_WP_30min.Rdata"))
#setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work

