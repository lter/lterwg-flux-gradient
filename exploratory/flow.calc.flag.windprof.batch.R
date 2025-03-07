
# Load functions for the wind profile flux gradient calculation
source(file.path("functions/MO_Length_CRS.R"))
source(file.path("functions/calc.eddydiff.windprof.R"))
source(file.path("functions/calc.gas.aero.windprof.flux.R"))
source(file.path("functions/calc.eqn.aero.windprof.flux.R"))
source(file.path("functions/calculate.stability.correction.R"))
source(file.path("functions/calc.aerodynamic.canopy.height.R"))

# Calculate eddy diffusivity with the wind profile method
#min9.K.WP.list <- calc.eddydiff.windprof(site = site, min9 = min9Diff.list)
min30.K.WP.list <- calc.eddydiff.windprof(site = site, min9 = min30Diff.list)

# Compute wind profile flux gradient fluxes for all gases.
# Optional bootstrap (1) or skip bootstrap (0) for gas conc uncertainty
# function contains option to manual set name of eddy diffusivity column default is "EddyDiff"
#min9.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min9.K.WP.list, 
#                                               bootstrap = 1, nsamp=1000)
min30.FG.WP.list <- calc.gas.aero.windprof.flux(min9.K = min30.K.WP.list, 
                                               bootstrap = 1, nsamp=1000)


# Plot FCO2 comparison between FG and EC
data <- dplyr::filter(min9.FG.WP.list$CO2, dLevelsAminusB=="4_1")[c("FC_turb_interp","FG_mean")]
dataComp <- data[complete.cases(data),]
RFCO2 <- cor.test(data$FC_turb_interp,data$FG_mean)
print(paste0('FCO2 R-squared = ',round(RFCO2$estimate^2,2)*100,'%'))


# ggplot(data=dplyr::filter(min9.FG.WP.list$CO2, dLevelsAminusB=="4_2")) +
#   # ggplot(data=min9.FG.WP.list$CO2) +
#   geom_point(aes(x=FC_turb_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=paste0(site, ' Wind Profile method (levels 4-1); R-squared = ',round(RFCO2$estimate^2,2)*100,'%')) +
#   theme_minimal()

# 
# # Plot FH2O comparison between FG and EC
# data <- dplyr::filter(min9.FG.WP.list$H2O, dLevelsAminusB=="4_1")[c("FH2O_interp","FG_mean")]
# dataComp <- data[complete.cases(data),]
# RFH2O <- cor.test(data$FH2O_interp,data$FG_mean)
# print(paste0('FH2O R-squared = ',round(RFH2O$estimate^2,2),' %'))
# 
# ggplot(data=dplyr::filter(min9.FG.WP.list$H2O, dLevelsAminusB=="4_1")) +
#   # ggplot(data=min9.FG.WP.list$H2O) +
#   geom_point(aes(x=FH2O_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=paste0(site, ' Wind Profile method (levels 4-1); R-squared = ',round(RFH2O$estimate^2,2)*100,'%')) +
#   theme_minimal()
# 
# # Plot CH4 comparison between FG and EC
# data <- dplyr::filter(min9.FG.WP.list$CH4, dLevelsAminusB=="4_1")[c("FCH4_turb_interp","FG_mean")]
# dataComp <- data[complete.cases(data),]
# RFCH4 <- cor.test(data$FCH4_turb_interp,data$FG_mean)
# print(paste0('FCH4 R-squared = ',round(RFCH4$estimate^2,2)*100,' %'))
# 
# ggplot(data=dplyr::filter(min9.FG.WP.list$CH4, dLevelsAminusB=="4_1")) +
#   # ggplot(data=min9.FG.WP.list$CH4) +
#   geom_point(aes(x=FCH4_turb_interp, y=FG_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=paste0(site, ' Wind Profile method (levels 4-1); R-squared = ',round(RFCH4$estimate^2,2)*100,'%')) +
#   theme_minimal()

# Save calculated wind profile flux gradient fluxes as R.data objects
# save(min9.FG.WP.list, file = file.path("data", site, paste0(site,"_WP_", user, "_", Sys.Date(),".Rdata")))
# #zip R.data objects
# zip(zipfile = file.path("data", site, paste0(site,"_WP_", user, "_", Sys.Date(),".zip")), files = file.path("data", site, paste0(site,"_WP_", user, "_", Sys.Date(),".Rdata")))
# #upload to Google Drive
# #IMPORTANT REMINDER if you have not gone through the process of valdiating your email with googledrive in R this code will not work please refer to https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
# #NOTE: you will be asked to re authenticate if your OAuth token is stale, select your already authenticated email from the list
# googledrive::drive_upload(media = file.path("data", site, paste0(site,"_WP_", user, "_", Sys.Date(),".zip")), overwrite = T, path = drive_url)

# Save 9-minute 
#fileSave <- fs::path(dirTmp,paste0(site,"_WP_9min.Rdata"))
#fileZip <- fs::path(dirTmp,paste0(site,"_WP_9min.zip"))
#save(min9.FG.WP.list,file=fileSave)
wdPrev <- getwd()
#setwd(dirTmp)
#utils::zip(zipfile=fileZip,files=paste0(site,"_WP_9min.Rdata"))
#setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work

# Save 30-minute
fileSave <- fs::path(dirTmp,paste0(site,"_WP_30min.Rdata"))
fileZip <- fs::path(dirTmp,paste0(site,"_WP_30min.zip"))
save(min30.FG.WP.list,file=fileSave)
wdPrev <- getwd()
setwd(dirTmp)
utils::zip(zipfile=fileZip,files=paste0(site,"_WP_30min.Rdata"))
setwd(wdPrev)
googledrive::drive_upload(media = fileZip, overwrite = T, path = data_folder$id[data_folder$name==site]) # path might need work

