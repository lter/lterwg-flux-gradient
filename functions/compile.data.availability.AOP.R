
Site.Data.Availability.AOP <- function(dpID ){
  info <- neonUtilities::getProductInfo(dpID=dpID)
  
  availability <- info$siteCodes
  
  availabilityList <- lapply(availability$siteCode,FUN=function(siteIdx){
    idxRow <- which(availability$siteCode == siteIdx)
    yearMnths <- unlist(availability$availableMonths[idxRow])
    year <- substr(yearMnths,start=1,stop=4)
    mnth <- substr(yearMnths,start=6,stop=7)
    url <- unlist(availability$availableDataUrls[idxRow])
    rpt <- data.frame(site=siteIdx,year=year,month=mnth,URL=url,stringsAsFactors = FALSE)
    return(rpt)
  })
  
  LAI.availabilityDf <- do.call(rbind,availabilityList) 
  
  return(LAI.availabilityDf [, 1:3])
}
#EOF