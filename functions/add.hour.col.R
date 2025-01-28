#' add.hour.col
#'
#' @param site individual site data frame
#' @param site.name NEON site code
#'
#' @return data frame with hour column and timeMid set to local time to site location
#' 
#'
#' @author Alexis Helgeson and Sam Jurado
add.hour.col <- function(site, site.name){
  #fix timezone and adjust for daylight savings
  #these sites are all in MT
  if(site.name == "NIWO" | site.name == "BONA" | site.name == "CPER" | site.name == "JORN"){
    site$timeMid <- as.POSIXct(site$timeMid, tz ="MST7MDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
    site$hour <- substr(as.character(site$timeMid), start = 12, stop = 13)
  }
  #this site is in EST
  if(site.name == "HARV"){
    site$timeMid <- as.POSIXct(site$timeMid, tz ="EST5EDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
    site$hour <- substr(as.character(site$timeMid), start = 12, stop = 13)
  }
  #this site is in AKT
  if(site.name == "TOOL"){
    site$timeMid <- as.POSIXct(site$timeMid, tz ="ASKT9AKDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
    site$hour <- substr(as.character(site$timeMid), start = 12, stop = 13)
  }
  #this site is in CT
  if(site.name == "KONZ"){
    site$timeMid <- as.POSIXct(site$timeMid, tz ="CST6CDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
    site$hour <- substr(as.character(site$timeMid), start = 12, stop = 13)
  }
  #this site is in AST
  if(site.name == "GUAN"){
    site$timeMid <- as.POSIXct(site$timeMid, tz ="EST5EDT,M3.2.0/2:00:00,M11.1.0/2:00:00") #NEED TO FIND PROPER TZ CORRECTION CODE
    site$hour <- substr(as.character(site$timeMid), start = 12, stop = 13)
  }
  
  return(site)
}