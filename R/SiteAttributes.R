#' SiteAttributes
#'
#' @param sitecode NEON site code
#' @param hd.files file type h5 containg NEON site specific data
#'
#' @return df containing site attributes
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
SiteAttributes <- function(hd.files, sitecode){
  #all attributes grabbed are: "DistZaxsCnpy""DistZaxsDisp""DistZaxsGrndOfst""DistZaxsLvlMeasTow""DistZaxsTow""ElevRefeTow""LatTow""LonTow""LvlMeasTow""Pf.AngEnuXaxs""Pf.AngEnuYaxs""Pf.Ofst""TimeDiffUtcLt""TimeTube""TypeEco""ZoneTime""ZoneUtm"
  #we are only using "DistZaxsLvlMeasTow" (i.e. the measurement heights) in the MBR calculation
  #this fcn only uses 1st file in the list which corresponds to specific month, we are assuming all of these attributes are consistent across all months
  attr <- data.frame(rhdf5::h5readAttributes(hd.files[1], name = paste0("/", sitecode)))
  attr <- dplyr::select(attr, DistZaxsLvlMeasTow)
  #add NEON sitecide as column
  attr$Site <- sitecode 
  
  return(attr)
  
}
