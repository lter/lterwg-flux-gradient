# flow.batch.flux

email <- 'sparklelmalone@gmail.com'


googledrive::drive_auth(email = email) 

# Add all sites here:
site.list <- c( 'HARV', 'KONZ', 'GUAN', 'JORN')

for( site in site.list){
  print(site)
  
  #source(file.path("workflows/flow.calc.flag.mbr.R"))
  source(file.path("workflows/flow.calc.flag.aero.R"))
  source(file.path("workflows/flow.calc.flag.windprof.R"))

}
