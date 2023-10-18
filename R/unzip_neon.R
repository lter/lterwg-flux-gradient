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
unzip_neon <- function(in_path = NULL, out_path = NULL, quiet = FALSE){

  # Identify names of downloaded ZIP files
  zip.files <- dir(path = in_path, pattern = ".zip")
  
  # Error out for no ZIP files found
  if(length(zip.files) == 0)
    stop("No ZIP files found at specified `in_path`")
  
  # Message ZIP success if `quiet` argument doesn't specify the opposite
  if(quiet != TRUE){
    message("Found ", length(zip.files), " ZIP files")
  }
  
  # Unzip those files!
  for(j in 1:length(zip.files)){
    unzip(file.path(in_path, zip.files[j]), exdir = in_path)
  }
  
  # Identify the .gz files created by unzipping
  gz.files <- dir(path = in_path, pattern = ".gz")
  
  # Error out if no .gz files are found
  if(length(gz.files) == 0)
    stop("No .gz files found in specified ZIP files")
  
  # Message ZIP success if `quiet` argument doesn't specify the opposite
  if(quiet != TRUE){
    message("Found ", length(gz.files), " .gz files")
  }
  
  # Process those as well!
  for(k in 1:length(gz.files)){
    R.utils::gunzip(file.path(in_path, gz.files[k]), 
                    destname = file.path(out_path,
                                         gsub(pattern = ".gz", replacement = "", 
                                              x = gz.files[k])), 
                    remove = F, overwrite = T) }
}
