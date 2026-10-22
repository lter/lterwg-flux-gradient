# Keep bootstrap warnings summarized instead of printing once per missing observation.
options(warn = 0)
source(file.path(gh_repo, "functions", "calc.mbr.R"))
source(file.path(gh_repo, "functions", "calc.MO.length.R"))
source(file.path(gh_repo, "functions", "calc.stability.correction.R"))
source(file.path(gh_repo, "functions", "calc.aerodynamic.canopy.height.R"))

# Same aerodynamic canopy height (z_veg_aero), d (z_displ_calc), z0 (roughLength_calc), L and pair stability
# terms as the AE and WP files. Without this, the MBR output kept the older wind-based canopy height from
# flow.neon.data.format.conc.diffs.R. The MBR flux itself does not use these columns.
min9Diff.list.mbr <- lapply(min9Diff.list, function(g)
  calc.stability.correction(sitecode = site, gas = g, min9 = min9Diff.list))

# Calculate MBR flux combos 9 min (CO2 with H2O trace, CH4 with H2O trace, etc)
MBRflux_align = calc.mbr(min9 = min9Diff.list.mbr, 
                         bootstrap = 1,
                         nsamp = 1000)


# Calculate MBR flux combos 30 min (e.g. CO2 with H2O tracer ...)
#MBRflux_align_30min = calc.mbr(min9=min30Diff.list, bootstrap=1, 
#                               nsamp=1000)

# # FC with H2O as tracer
# data <- MBRflux_align[c("FC_turb_interp_CO2","FCO2_MBR_H2Otrace_mean")]
# dataComp <- data[complete.cases(data),]
# RFCO2 <- cor.test(data$FC_turb_interp_CO2,data$FCO2_MBR_H2Otrace_mean)
# print(paste0('RFCO2 R-squared = ',round(RFCO2$estimate^2,2)*100,' %'))

# ggplot(data=dplyr::filter(MBRflux_align, dConc_H2O_bin==0)) +
#   geom_point(aes(x=FC_turb_interp_CO2, y=FCO2_MBR_H2Otrace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-15,15)) +
#   xlim(c(-15,15)) +
#   labs(title=paste0(site, ' MBR method (levels 4-3); R-squared = ',round(RFCO2$estimate^2,2)*100,'%')) +
#   theme_minimal()
# 
# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FH2O_interp_H2O, y=FH2O_MBR_CO2trace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-1,5)) +
#   xlim(c(-1,5)) +
#   labs(title = site) +
#   theme_minimal()
# 
# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FCH4_turb_interp_CH4, y=FCH4_MBR_H2Otrace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-50,50)) +
#   xlim(c(-50,50)) +
#   labs(title=site) +
#   theme_minimal()

# #data <- dplyr::filter(MBRflux_align, dLevelsAminusB_CH4=="4_1")[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
# data <- MBRflux_align[c("FCH4_turb_interp_CH4","FCH4_MBR_CO2trace_mean")]
# dataComp <- data[complete.cases(data),]
# RFCH4 <- cor.test(data$FCH4_turb_interp_CH4,data$FCH4_MBR_CO2trace_mean)
# print(paste0('FCH4 R-squared = ',round(RFCH4$estimate^2,2)*100,' %'))

# ggplot(data=dplyr::filter(MBRflux_align, dConc_CO2_bin==0)) +
#   geom_point(aes(x=FCH4_turb_interp_CH4, y=FCH4_MBR_CO2trace_mean)) +
#   geom_abline(aes(intercept=0,slope=1),lty=2) +
#   ylim(c(-100,100)) +
#   xlim(c(-100,100)) +
#   labs(title=paste0(site, ' MBR method (levels 4-3); R-squared = ',round(RFCH4$estimate^2,2)*100,'%')) +
#   theme_minimal()

rm(min9Diff.list.mbr)

# -------- Save and zip the file to the temp directory. Upload to google drive. -------
fileSave <- fs::path(dirTmp, paste0(site, '_MBR_9min.RData'))
fileZip <- fs::path(dirTmp, paste0(site, '_MBR_9min.zip'))
save(MBRflux_align, file = fileSave)

utils::zip(zipfile = fileZip, files = fileSave)

if (export_calc_google == 1){
  googledrive::drive_upload(media = fileZip, 
                            overwrite = T, 
                            path = data_folder$id[data_folder$name==site]) 
}


#fileSave <- fs::path(dirTmp,paste0(site,'_MBR_30min.RData'))
#fileZip <- fs::path(dirTmp,paste0(site,'_MBR_30min.zip'))
#save(MBRflux_align_30min,file=fileSave)
#wdPrev <- getwd()
#setwd(dirTmp)
#utils::zip(zipfile=fileZip,files=paste0(site,'_MBR_30min.RData'))
#setwd(wdPrev)
#googledrive::drive_upload(media = fileZip, overwrite = T, 
#                          path = data_folder$id[data_folder$name==site]) # path might need work
