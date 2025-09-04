# Creates the list of dataframes used by One2One, Diel, and Cparms. This also compiles the filter reports for all sites.  

SITES_MBR_9min_FILTER <- list()
SITES_AE_9min_FILTER <- list()
SITES_WP_9min_FILTER <- list()
filter.report <- data.frame()
filter.report.stability <- data.frame()
# Site List DF and filter report
for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_FILTER.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  
  SITES_MBR_9min_FILTER[[site]] <- MBR_9min_FILTER
  SITES_AE_9min_FILTER[[site]] <- AE_9min_FILTER
  SITES_WP_9min_FILTER[[site]] <- WP_9min_FILTER
  
  files <- paste(site, "_9min.report.csv", sep = "")
  
  report <- read.csv( paste( localdir.site,"/", files, sep="" ))
  filter.report <- rbind(filter.report,  report )
  
  message("Done with ", site)
}

for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")

  files <- paste(site, "_9min.report.stability.csv", sep = "")
  
  report <- read.csv( paste( localdir.site,"/", files, sep="" ))
  filter.report.stability <- rbind(filter.report.stability,  report )
  
  message("Done with ", site)
}