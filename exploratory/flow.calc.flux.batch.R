# flow.batch.flux

library(fs)
library(googledrive)

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient")

# Add all sites here:
site.list <- c('HARV' ,'KONZ', 'GUAN', 'JORN')

for( site in site.list ){
  site <- site
  print(site)
  
  # Download the aligned concentration data for the site
  source(file.path("exploratory/flow.calc.flag.mbr.batch.R"))
  print('data downloaded')
  
  print( 'Running MBR')
  source(file.path("exploratory/flow.calc.flag.mbr.batch.R"))
  print( 'Running AE')
  source(file.path("exploratory/flow.calc.flag.aero.batch.R"))
  print( 'Running WP')
  source(file.path("exploratory/flow.calc.flag.windprof.batch.R"))

}
