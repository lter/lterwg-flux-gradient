# Filter flux data:

source(fs::path(DirRepo,'exploratory/FUNCTION_Filter_FG.R' ))
source(fs::path(DirRepo,'exploratory/FUNCTION_SITELIST_FORMATTING.R' ))

for( site in site.list){
  
  print( site)

  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_Evaluation.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  # Run the functions:
  
  WP_9min.report <-  filter_report(df = WP_9min.df.final,
                                   flux.limit = 5000, 
                                   ustar.filter= 0.1, 
                                   FG_sd.limit = 3000,
                                   diff.limit = 100000,
                                   dConcSNR.min = 0,
                                   rmvCrossGrad = TRUE,
                                   rmvEddyOutlier = TRUE,
                                   approach = "WP")
  
  AE_9min.report <-  filter_report( df = AE_9min.df.final,
                                    flux.limit = 5000, 
                                    ustar.filter= 0.1, 
                                    FG_sd.limit = 3000,
                                    diff.limit = 100000,
                                    dConcSNR.min = 0,
                                    rmvCrossGrad = TRUE,
                                    rmvEddyOutlier = TRUE,
                                    approach = "AE"
  )
  
  MBR_9min.report <-  filter_report( df = MBR_9min.df.final,
                                     flux.limit = 5000, 
                                     ustar.filter= 0.1, 
                                     FG_sd.limit = 3000,
                                     diff.limit = 100000,
                                     dConcSNR.min = 3,
                                     rmvCrossGrad = FALSE,
                                     rmvEddyOutlier = FALSE,
                                     approach = "MBR")
  
  SITE_9min.report <- rbind( WP_9min.report, AE_9min.report, MBR_9min.report)
  
  MBR_9min_FILTER <- filter_fluxes( df = MBR_9min.df.final,
                                    flux.limit = 5000, 
                                    ustar.filter= 0.1, 
                                    FG_sd.limit = 3000,
                                    diff.limit = 100000,
                                    dConcSNR.min = 3,
                                    rmvCrossGrad = FALSE,
                                    rmvEddyOutlier = FALSE,
                                    approach = "MBR")  
  
  AE_9min_FILTER <- filter_fluxes( df = AE_9min.df.final,
                                   flux.limit = 5000, 
                                   ustar.filter= 0.1, 
                                   FG_sd.limit = 3000,
                                   diff.limit = 100000,
                                   dConcSNR.min = 0,
                                   rmvCrossGrad = TRUE,
                                   rmvEddyOutlier = TRUE,
                                   approach = "AE") 
  
  WP_9min_FILTER <- filter_fluxes ( df = WP_9min.df.final,
                                    flux.limit = 5000, 
                                    ustar.filter= 0.1, 
                                    FG_sd.limit = 3000,
                                    diff.limit = 100000,
                                    dConcSNR.min = 0,
                                    rmvCrossGrad = TRUE,
                                    rmvEddyOutlier = TRUE,
                                    approach = "WP") 
  
  
  # Output the files
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  write.csv( SITE_9min.report,  paste(localdir.site, "/", site,"_9min.report.csv", sep=""))
  
  save( AE_9min_FILTER,
        WP_9min_FILTER,
        MBR_9min_FILTER, file = paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  # Upload files to the google
  
  site_folder <-data_folder$id[data_folder$name==site]
  
  fileSave <- paste(localdir.site, "/", site, "_FILTER.Rdata", sep="")
  googledrive::drive_upload(media = fileSave, overwrite = T, path = site_folder)
  
  fileSave <- paste(localdir.site, "/", site,"_9min.report.csv", sep="")
  googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)
  
  message( paste("Done with filtering", site))
  
  rm( AE_9min_FILTER,
      WP_9min_FILTER,
      MBR_9min_FILTER,SITE_9min.report, WP_9min.report, AE_9min.report, MBR_9min.report)
}
