#' downloadUnzipNEON
#'
#' @param zip.dir filepath location of NEON zip folder
#' @param sitecode NEON site code
#' @param startdate startdate for download
#' @param enddate enddate for download
#'
#' @return print message identifying where unzipped files can be found
#'
#' @author Alexis Helgeson
downloadUnzipNEON <- function(sitecode, zip.dir, startdate, enddate){
  #downloaded NEON eddy-co data as zip file
  #this fcn is a rate limiter, downloads can be lengthy, quicker to download off website?
  zipsByProduct(dpID="DP4.00200.001", sitecode,startdate, enddate,package="basic", check.size=F)

  #unzip files
  #I think the stackEddy fcn can be used here but currently does not list CH4 concentrations as an option
  #test <- stackEddy(filepath = paste0(site.dir, "/filesToStack00200/"), level="dp04")
  #setwd(zip.dir)
  setwd(paste0(zip.dir, "/filesToStack00200"))

  zip.files <-list.files(pattern=".zip")
  for(j in 1:length(zip.files)){
    print(j)
    unzip(zip.files[j]) 
  }
  folders <- list.files(pattern = "KONZ.DP4.00200.001")
 
  for(j in 3:length(folders)){
    gz.files <-list.files(path = file.path(folders[j]), pattern=".gz", full.names = TRUE)
    print(j)
    gunzip(gz.files, remove=FALSE)
  }
  
  return(print(paste0("NEON files for ", sitecode, " are downloaded and unzipped h5 files can be found at ", zip.dir)))
}
