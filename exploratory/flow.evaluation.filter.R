# Filter flux data:

source(fs::path(DirRepo,'exploratory/FUNCTION_Filter_FG.R' ))
source(fs::path(DirRepo,'exploratory/FUNCTION_SITELIST_FORMATTING.R' ))

SITES_WP_9min.report <-  Generate.filter.report( site.tibble = SITES_AE_9min,
                                                  flux.limit = 5000, 
                                                  ustar.filter= 0.1, 
                                                  FG_sd.limit = 3000,
                                                  diff.limit = 100000,
                                                  dConcSNR.min = 0,
                                                  rmvCrossGrad = TRUE,
                                                  rmvEddyOutlier = TRUE,
                                                  approach = "WP")

SITES_AE_9min.report <-  Generate.filter.report( site.tibble = SITES_AE_9min,
                                                  flux.limit = 5000, 
                                                  ustar.filter= 0.1, 
                                                  FG_sd.limit = 3000,
                                                  diff.limit = 100000,
                                                  dConcSNR.min = 0,
                                                  rmvCrossGrad = TRUE,
                                                  rmvEddyOutlier = TRUE,
                                                  approach = "AE")

SITES_MBR_9min.report <-  Generate.filter.report( site.tibble = SITES_MBR_9min,
                                                  flux.limit = 5000, 
                                                  ustar.filter= 0.1, 
                                                  FG_sd.limit = 3000,
                                                  diff.limit = 100000,
                                                  dConcSNR.min = 3,
                                                  rmvCrossGrad = FALSE,
                                                  rmvEddyOutlier = FALSE,
                                                  approach = "MBR")


SITES_MBR_9min_FILTER <- Apply.filter( site.tibble = SITES_MBR_9min,
                                       flux.limit = 5000, 
                                       ustar.filter= 0.1, 
                                       FG_sd.limit = 3000,
                                       diff.limit = 100000,
                                       dConcSNR.min = 3,
                                       rmvCrossGrad = FALSE,
                                       rmvEddyOutlier = FALSE,
                                       approach = "MBR")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_AE_9min_FILTER <- Apply.filter( site.tibble = SITES_AE_9min,
                                      flux.limit = 5000, 
                                      ustar.filter= 0.1, 
                                      FG_sd.limit = 3000,
                                      diff.limit = 100000,
                                      dConcSNR.min = 0,
                                      rmvCrossGrad = TRUE,
                                      rmvEddyOutlier = TRUE,
                                      approach = "AE")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

SITES_WP_9min_FILTER <- Apply.filter ( site.tibble = SITES_WP_9min,
                                       flux.limit = 5000, 
                                       ustar.filter= 0.1, 
                                       FG_sd.limit = 3000,
                                       diff.limit = 100000,
                                       dConcSNR.min = 0,
                                       rmvCrossGrad = TRUE,
                                       rmvEddyOutlier = TRUE,
                                       approach = "WP")  %>% TIME_TOWER_LEVEL_FORMAT( time.col='match_time', dLevelsAminusB.colname= 'dLevelsAminusB')

