#' @title Un-ZIP NEON Data Products
#' 
#' @description NEON data products are downloaded as ZIP files which unzip to .gz files. This function accepts the path to those 
#'
#' @param zip.dir filepath location of NEON zip folder
#' @param sitecode NEON site code
#' @param startdate startdate for download
#' @param enddate enddate for download
#'
#' @return print message identifying where unzipped files can be found
#'
#' @author Alexis Helgeson, Nick Lyon
#' 
#' 
unzip_neon <- function(in_path = NULL, out_path = NULL){

  # Identify names of downloaded ZIP files
  zip.files <- dir(path = in_path, pattern = ".zip")

# Unzip those files!
for(j in 1:length(zip.files)){
  unzip(file.path(sitename, "filesToStack00200", zip.files[j]), exdir = in_path)
}

# Identify the .gz files created by unzipping
gz.files <- dir(path = in_path, pattern = ".gz")

# Process those as well!
for(k in 1:length(gz.files)){
  R.utils::gunzip(file.path(in_path, gz.files[k]), 
                  destname = file.path(out_path,
                                       gsub(pattern = ".gz", replacement = "", 
                                            x = gz.files[k])), 
                  remove = F, overwrite = T)
}

}



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
