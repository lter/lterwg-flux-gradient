#' downloadUnzipNEON
#'
#' @param zip.dir filepath location of downloaded NEON zip folder
#' @param sitecode NEON site code
#'
#' @return print message identifying where unzipped files can be found
#'
#' @author Alexis Helgeson
downloadUnzipNEON <- function(sitecode, zip.dir){
  #downloaded NEON eddy-co data as zip file
  #this fcn is a rate limiter, downloads can be lengthy, quicker to download off website?
  #zipsByProduct(dpID="DP4.00200.001", sitecode,startdate, enddate,package="basic", check.size=F)
  
  #unzip files
  #I think the stackEddy fcn can be used here but currently does not list CH4 concentrations as an option
  #test <- stackEddy(filepath = paste0(site.dir, "/filesToStack00200/"), level="dp04")
  setwd(zip.dir)
  zip.files <-list.files(pattern=".zip")
  for(j in 1:length(zip.files)){
    print(j)
    unzip(zip.files[j]) 
  }
  gz.files <-list.files(pattern=".gz")
  for(j in 1:length(gz.files)){
    print(j)
    gunzip(gz.files[j], remove=FALSE)
  }
  
  return(print(paste0("NEON files for ", sitecode, " are downloaded and unzipped h5 files can be found at ", zip.dir)))
}
