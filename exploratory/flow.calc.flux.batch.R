# flow.batch.flux

email <- 'sparklelmalone@gmail.com'


googledrive::drive_auth(email = email) 

# Add all sites here:
site.list <- c('KONZ', 'GUAN', 'JORN')

for( site in site.list){
  print(site)
  #print( 'Running MBR')
 # source(file.path("workflows/flow.calc.flag.mbr.R"))
  print( 'Running AE')
  source(file.path("workflows/flow.calc.flag.aero.R"))
  print( 'Running WP')
  source(file.path("workflows/flow.calc.flag.windprof.R"))

}
