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
  #removing TypeEco from the list because this returns an error from data.frame when run at GUAN this site 
  attr.list <- attr.list[-which(names(attr.list) == "TypeEco")]
  #if site is UKFS, do this workaround to fix the typo in TimeTube 
  if (sitecode == "UKFS"){
    third_element <- attr.list$TimeTube[3]
    fourth_element <- attr.list$TimeTube[4]
    fifth_element <- attr.list$TimeTube[5]
    if (nchar(attr.list$TimeTube[3]) == 10){
      attr.list$TimeTube[3] <- stringr::str_extract_all(third_element, "[:digit:]+\\.[:digit:]+")[[1]][1]
      attr.list$TimeTube[4] <- stringr::str_extract_all(third_element, "[:digit:]+\\.[:digit:]+")[[1]][2]
      attr.list$TimeTube[5] <- fourth_element
      attr.list$TimeTube[6] <- fifth_element
    }
  }
  attr.df <- data.frame(attr.list)
  attr.df$TowerPosition <- seq(1,dim(attr.df)[1], 1)
  #add NEON sitecide as column
  attr.df$Site <- sitecode 
  
  return(attr.df)
  
}
