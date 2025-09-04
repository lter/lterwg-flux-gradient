# Filter flux data:

source(fs::path(DirRepo,'exploratory/FUNCTION_Filter_FG.R' ))
source(fs::path(DirRepo,'exploratory/FUNCTION_SITELIST_FORMATTING.R' ))

for( site in site.list){
  
  print( site)

  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_Evaluation.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  # Change the time to local:
  
  library(lutz)
  # Get NEON sites from the server and find the time zones: https://cran.r-project.org/web/packages/lutz/readme/README.html
  sites.location <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>%  st_as_sf(coords = c("Longitude..degrees.", "Latitude..degrees."),
                                                                                                                      crs = "+proj=longlat +datum=WGS84")
  
  sites.location$TZ <- tz_lookup(sites.location, method = "accurate")
  sites.location.sub <- sites.location %>%  select( "Site_Id.NEON" , "TZ")
  
  site.tz <- sites.location.sub$TZ[which( sites.location.sub$Site_Id.NEON == site)]
  
  MBR_9min.df.final$timeEndA.local <- MBR_9min.df.final$timeEndA %>%  as.POSIXlt( tz = site.tz)
  AE_9min.df.final$timeEndA.local <- AE_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  WP_9min.df.final$timeEndA.local <- WP_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  
  MBR_9min.df.final$time.local <- MBR_9min.df.final$timeEndA.local %>% format("%H:%M")
  AE_9min.df.final$time.local <- AE_9min.df.final$timeEndA.local %>% format("%H:%M")
  WP_9min.df.final$time.local <- WP_9min.df.final$timeEndA.local %>% format("%H:%M")
  
  MBR_9min.df.final$hour.local <- MBR_9min.df.final$timeEndA.local %>% format("%H")
  AE_9min.df.final$hour.local <- AE_9min.df.final$timeEndA.local %>% format("%H")
  WP_9min.df.final$hour.local <- WP_9min.df.final$timeEndA.local %>% format("%H")
  
  # Run the filter functions... Report:

  WP_9min.report <-  filter_report(df = WP_9min.df.final,
                                   dConcSNR.min = 3,
                                   approach = "WP")
  
  AE_9min.report <-  filter_report( df = AE_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "AE")
  
  MBR_9min.report <-  filter_report( df = MBR_9min.df.final,
                                     dConcSNR.min = 3,
                                     approach = "MBR")
  
  
  WP_9min.report.stability <-  filter_report_stability(df = WP_9min.df.final,
                                   dConcSNR.min = 3,
                                   approach = "WP")
  
  AE_9min.report.stability <-  filter_report_stability( df = AE_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "AE")
  
  MBR_9min.report.stability <-  filter_report_stability( df = MBR_9min.df.final,
                                     dConcSNR.min = 3,
                                     approach = "MBR")
  
  # Add the approach into the file:
  MBR_9min.report$approach = "MBR"
  AE_9min.report$approach = "AE" 
  WP_9min.report$approach = "WP"
  
  MBR_9min.report.stability $approach = "MBR"
  AE_9min.report.stability $approach = "AE" 
  WP_9min.report.stability $approach = "WP"
  
  SITE_9min.report <- rbind( WP_9min.report, AE_9min.report, MBR_9min.report)
  SITE_9min.report.stability <- rbind( WP_9min.report.stability, AE_9min.report.stability, MBR_9min.report.stability)
  
  # Add the site into the file:
  SITE_9min.report$site <- site
  SITE_9min.report.stability$site <- site
  
  # Run the filter functions... FILTER data:
  MBR_9min_FILTER <- filter_fluxes( df = MBR_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "MBR")  
  
  AE_9min_FILTER <- filter_fluxes( df = AE_9min.df.final,
                                   dConcSNR.min = 3,
                                   approach = "AE") 
  
  WP_9min_FILTER <- filter_fluxes ( df = WP_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "WP") 
 
  # Output the files
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  write.csv( SITE_9min.report.stability,  paste(localdir.site, "/", site,"_9min.report.stability.csv", sep=""))
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
  
  fileSave <- paste(localdir.site, "/", site,"_9min.report.stability.csv", sep="")
  googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)

  message( paste("Done with filtering", site))
  
  rm( AE_9min_FILTER,
      WP_9min_FILTER,
      MBR_9min_FILTER,SITE_9min.report, WP_9min.report, AE_9min.report, MBR_9min.report,
      SITE_9min.report.stability, WP_9min.report.stability, AE_9min.report.stability, MBR_9min.report.stability,
      SITE_9min.report, sample.diel, sample.month)
}