# Filter flux data:

DirRepo <- "." # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!localdir <- tempdir()
source(fs::path(DirRepo,'exploratory/FUNCTION_Filter_FG.R' ))
source(fs::path(DirRepo,'exploratory/FUNCTION_SITELIST_FORMATTING.R' ))

SITES_WP_9min.report <-  Generate.filter.report( site.tibble = SITES_AE_9min,
                                                  flux.limit = 50, 
                                                  ustar.filter= 0.3, 
                                                  FG_sd.limit = 3,
                                                  diff.limit = 1000,
                                                  dConcSNR.min = 3,
                                                  approach = "WP")

SITES_AE_9min.report <-  Generate.filter.report( site.tibble = SITES_AE_9min,
                                                  flux.limit = 50, 
                                                  ustar.filter= 0.3, 
                                                  FG_sd.limit = 3,
                                                  diff.limit = 1000,
                                                  dConcSNR.min = 3,
                                                  approach = "AE")

SITES_MBR_9min.report <-  Generate.filter.report( site.tibble = SITES_MBR_9min,
                                                   flux.limit = 50, 
                                                   ustar.filter= 0.3, 
                                                   FG_sd.limit = 3,
                                                   diff.limit = 1000,
                                                   dConcSNR.min = 3,
                                                   approach = "MBR")

fileSave <- fs::path(localdir,"FilterReport_ALLSites.Rdata")
save( SITES_WP_9min.report,SITES_AE_9min.report, SITES_MBR_9min.report ,
      file=fileSave)

googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

SITES_MBR_9min_FILTER <- Apply.filter( site.tibble = SITES_MBR_9min,
                                        flux.limit = 50, 
                                        ustar.filter= 0.3, 
                                        FG_sd.limit = 3,
                                        diff.limit = 1000,
                                        dConcSNR.min = 3,
                                        approach = "MBR")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_AE_9min_FILTER <- Apply.filter( site.tibble = SITES_AE_9min,
                                       flux.limit = 50, 
                                       ustar.filter= 0.3, 
                                       FG_sd.limit = 3,
                                       diff.limit = 1000,
                                       dConcSNR.min = 3,
                                       approach = "AE")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_WP_9min_FILTER <- Apply.filter ( site.tibble = SITES_WP_9min,
                                        flux.limit = 50, 
                                        ustar.filter= 0.3, 
                                        FG_sd.limit = 4,
                                        diff.limit = 1000,
                                        dConcSNR.min = 3,
                                        approach = "WP")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

