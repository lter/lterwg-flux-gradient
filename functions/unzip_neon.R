#' @title Un-ZIP NEON Data Products
#' 
#' 
#' @param in_path location of download zip file
#' @param out_path location to store h5 files
#' @param quiet update function messages on/off
#' 
#' @description NEON data products are downloaded as ZIP files which unzip to .gz files. This function accepts the path to those 
#'
#' @return print message identifying where unzipped files can be found
#'
#' @author Alexis Helgeson, Nick Lyon, Sparkle Malone
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
    message("No .gz files found in specified ZIP files")
  
  # Message ZIP success if `quiet` argument doesn't specify the opposite
  if(quiet != TRUE & length(gz.files) > 0){
    message("Found ", length(gz.files), " .gz files")
    
    # Process those as well!
    for(k in 1:length(gz.files)){
      
      R.utils::gunzip(file.path(in_path, gz.files[k]), 
                      destname = file.path(out_path,
                                           gsub(pattern = ".gz", replacement = "", 
                                                x = gz.files[k])), 
                      remove = F, overwrite = T) }
  }
  
  
}


  
  # Process those as well!
  
