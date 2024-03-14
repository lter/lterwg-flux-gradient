#' compile.neon.site.attr
#'
#' @param sitecode NEON site code
#' @param hd.files file type h5 containg NEON site specific data
#'
#' @return df containing site attributes
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
compile.neon.site.attr <- function(hd.files, sitecode){
  #all attributes grabbed are: "DistZaxsCnpy""DistZaxsDisp""DistZaxsGrndOfst""DistZaxsLvlMeasTow""DistZaxsTow""ElevRefeTow""LatTow""LonTow""LvlMeasTow""Pf.AngEnuXaxs""Pf.AngEnuYaxs""Pf.Ofst""TimeDiffUtcLt""TimeTube""TypeEco""ZoneTime""ZoneUtm"
  #we are only using "DistZaxsLvlMeasTow" (i.e. the measurement heights) in the MBR calculation
  #this fcn only uses 1st file in the list which corresponds to specific month, we are assuming all of these attributes are consistent across all months
  attr.list <- rhdf5::h5readAttributes(hd.files[1], name = paste0("/", sitecode))
  #removing TypeEco from the list becasue this returns an error from data.frame when run at GUAN this site 
  attr.list <- attr.list[-which(names(attr.list) == "TypeEco")]
  attr.df <- data.frame(attr.list)
  attr.df$TowerPosition <- seq(1,dim(attr.df)[1], 1)
  #add NEON sitecide as column
  attr.df$Site <- sitecode 
  
  return(attr.df)
  
}
