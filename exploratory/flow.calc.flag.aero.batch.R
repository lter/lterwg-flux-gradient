
DoWP=1 # Do Wind Profile Method here as well? 1 for true O for False.
Savecsv=0 # Save csv files to analyze in matlab? 1 for true 0 for False.

# Load functions in this repo
source(file.path("functions/MO_Length_CRS.R"))
source(file.path("functions/calc.eddydiff.aero.R"))
source(file.path("functions/calc.gas.aero.windprof.flux.R"))
source(file.path("functions/calc.gas.aero.windprof.flux_WP.R"))
source(file.path("functions/calc.eqn.aero.windprof.flux.R"))
source(file.path("functions/calculate.stability.correction.R"))
source(file.path("functions/calc.aerodynamic.canopy.height.R"))

# Calculate eddy diffusivity with the aerodynamic method
min9.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 = min9Diff.list)

min30.K.AE.list <- calc.eddydiff.aero(sitecode = sitecode, min9 = min30Diff.list)

# Compute aerodynamic flux gradient fluxes for all gases
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
min9.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.AE.list,
                                               bootstrap = 1, nsamp = 1000)
min30.FG.AE.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.AE.list,
                                                
                                               bootstrap = 1, nsamp = 1000)

# # Plot FH2O comparison between FG and EC
# data <- dplyr::filter(min9.FG.AE.list$H2O, dLevelsAminusB=="4_1")[c("FH2O_interp","FG_mean")]
# dataComp <- data[complete.cases(data),]
# RFH2O <- cor.test(data$FH2O_interp,data$FG_mean)
# print(paste0('FH2O R-squared = ',round(RFH2O$estimate^2,2),' %'))
# 
# ggplot(data=dplyr::filter(min9.FG.AE.list$H2O, dLevelsAminusB=="4_1")) +
#   #ggplot(data=min9.FG.AE.list$H2O) +
#   geom_point(aes(x=FH2O_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-3000,3000)) +
#   xlim(c(-3000,3000)) +
#   labs(title=paste0(site, ' Aerodynamic method (levels 4-1); R-squared = ',round(RFH2O$estimate^2,2)*100,'%')) +
#   theme_minimal()
# 
# # Plot CO2 comparison between FG and EC
# data <- dplyr::filter(min9.FG.AE.list$CO2, dLevelsAminusB=="4_1")[c("FC_turb_interp","FG_mean")]
# dataComp <- data[complete.cases(data),]
# RFCO2 <- cor.test(data$FC_turb_interp,data$FG_mean)
# print(paste0('FCO2 R-squared = ',round(RFCO2$estimate^2,2)*100,'%'))
# 
# ggplot(data=dplyr::filter(min9.FG.AE.list$CO2, dLevelsAminusB=="4_1")) +
#   #ggplot(data=min9.FG.AE.list$CO2) +
#   geom_point(aes(x=FC_turb_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=paste0(site, ' Aerodynamic method (levels 4-1); R-squared = ',round(RFCO2$estimate^2,2)*100,'%')) +
#   theme_minimal()
# 
# # Plot CH4 comparison between FG and EC
# data <- dplyr::filter(min9.FG.AE.list$CH4, dLevelsAminusB=="4_1")[c("FCH4_turb_interp","FG_mean")]
# dataComp <- data[complete.cases(data),]
# RFCH4 <- cor.test(data$FCH4_turb_interp,data$FG_mean)
# print(paste0('FCH4 R-squared = ',round(RFCH4$estimate^2,2)*100,' %'))
# 
# ggplot(data=dplyr::filter(min9.FG.AE.list$CH4, dLevelsAminusB=="4_1")) +
#   #ggplot(data=min9.FG.AE.list$CH4) +
#   geom_point(aes(x=FCH4_turb_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-75,75)) +
#   xlim(c(-75,75)) +
#   labs(title=paste0(site, ' Aerodynamic method (levels 4-1); R-squared = ',round(RFCH4$estimate^2,2)*100,'%')) +
#   theme_minimal()

if (DoWP==1){
# Apply Wind Profile Method
min9.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min9.K.AE.list,
                                               bootstrap = 1, nsamp = 1000)
min30.FG.WP.list <- calc.gas.aero.windprof.flux_WP(min9.K = min30.K.AE.list,
                                                bootstrap = 1, nsamp = 1000)
}


# Save calculated aerodynamic flux gradient fluxes as R.data objects
# save(min9.FG.AE.list, file = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
# #zip R.data objects
# zip(zipfile = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), files = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".Rdata")))
# #upload to Google Drive
# #IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
# #NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
# googledrive::drive_upload(media = file.path("data", sitecode, paste0(sitecode,"_AE_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)


# Save 9-minute 
fileSave <- fs::path(dirTmp,paste0(site,"_AE_9min.Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_AE_9min.zip"))
save(min9.FG.AE.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_AE_9min.Rdata"))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
#googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

# Save 30-minute
fileSave <- fs::path(dirTmp,paste0(site,"_AE_30min.Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_AE_30min.zip"))
save(min30.FG.AE.list,file=fileSave)
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_AE_30min.Rdata"))
setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work
googledrive::drive_upload(media = fileSave, overwrite = T, path = data_folder$id[data_folder$name==site]) # couldn't make zip work (crs)

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
