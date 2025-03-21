# Filter flux data:

source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_Filter_FG.R' )
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_SITELIST_FORMATTING.R' )


SITES_WP_30min.report <-  Generate.filter.report( site.tibble = SITES_AE_30min,
                                                  flux.limit = 50, 
                                                  ustar.filter= 0.3, 
                                                  FG_sd.limit = 3,
                                                  diff.limit = 1000,
                                                  dConc.limit = 3,
                                                  approach = "WP")

SITES_AE_30min.report <-  Generate.filter.report( site.tibble = SITES_AE_30min,
                                                  flux.limit = 50, 
                                                  ustar.filter= 0.3, 
                                                  FG_sd.limit = 3,
                                                  diff.limit = 1000,
                                                  dConc.limit = 3,
                                                  approach = "AE")

SITES_MBR_30min.report <-  Generate.filter.report( site.tibble = SITES_MBR_30min,
                                                   flux.limit = 50, 
                                                   ustar.filter= 0.3, 
                                                   FG_sd.limit = 3,
                                                   diff.limit = 1000,
                                                   dConc.limit = 3,
                                                   approach = "MBR")

save( SITES_WP_30min.report,SITES_AE_30min.report, SITES_MBR_30min.report ,
      file="/Volumes/MaloneLab/Research/FluxGradient/FilterReport_ALLSites.Rdata")

fileSave <- file.path("/Volumes/MaloneLab/Research/FluxGradient/FilterReport_ALLSites.Rdata")
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

SITES_MBR_30min_FILTER <- Apply.filter( site.tibble = SITES_MBR_30min,
                                        flux.limit = 50, 
                                        ustar.filter= 0.3, 
                                        FG_sd.limit = 3,
                                        diff.limit = 1000,
                                        dConc.limit = 3,
                                        approach = "MBR")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_AE_30min_FILTER <- Apply.filter( site.tibble = SITES_AE_30min,
                                       flux.limit = 50, 
                                       ustar.filter= 0.3, 
                                       FG_sd.limit = 3,
                                       diff.limit = 1000,
                                       dConc.limit = 3,
                                       approach = "AE")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_WP_30min_FILTER <- Apply.filter ( site.tibble = SITES_WP_30min,
                                        flux.limit = 50, 
                                        ustar.filter= 0.3, 
                                        FG_sd.limit = 4,
                                        diff.limit = 1000,
                                        dConc.limit = 3,
                                        approach = "WP")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

