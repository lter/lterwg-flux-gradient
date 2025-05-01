
# Authenticate with Google Drive
email <- 'sam.jurado@yale.edu'
googledrive::drive_auth(email = email)

# Define Google Drive file IDs for required data files
file_ids <- list(
  "SITES_AE_9min.Rdata" = "12nE75Zh-tQ8oXOXARj2_3J2HjA6Lf1wl",
  "SITES_WP_9min.Rdata"= "17Y-QYKSof3nMOAz6L8QsCxtLSoqObvn-",
  "FilteredData_ALLSites_BH.Rdata" = "1bZw9bXN6YmvvtRvG4FGXZ2Zwbsj30ObR"
)

# Define local directory for downloads
localdir <- "/Users/jurado/flux_group_git/lterwg-flux-gradient/data"

# Create local directory if it doesn't exist
if (!dir.exists(localdir)) {
  dir.create(localdir, recursive = TRUE)
}

# Function to download files from Google Drive
download_googledrive_file <- function(file_name, file_id, localdir) {
  message(paste0("Downloading ", file_name, " from Google Drive..."))
  
  # Define local path
  local_path <- file.path(localdir, file_name)
  
  # Download the file
  googledrive::drive_download(
    googledrive::as_id(file_id),  # Explicitly call as_id from googledrive
    path = local_path,
    overwrite = TRUE
  )
  
  
  message(paste0(file_name, " downloaded successfully to ", local_path))
}

# Loop through the files and download them
for (file_name in names(file_ids)) {
  download_googledrive_file(file_name, file_ids[[file_name]], localdir)
}

# Load the downloaded RData files
for (file_name in names(file_ids)) {
  local_path <- file.path(localdir, file_name)
  if (file.exists(local_path)) {
    message(paste0("Loading ", file_name, "..."))
    load(local_path)
  } else {
    warning(paste0("File ", file_name, " not found after download."))
  }
}

message("All files processed successfully.")










