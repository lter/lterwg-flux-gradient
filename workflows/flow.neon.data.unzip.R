## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Unzips downloaded NEON data files

# NOTE IMPORTANT INFORMATION: 
# all of the flow scripts are written assuming the end user has connected their R studio project to the lterwg-flux-gradient GitHub repo 
# AND that they have created a data folder 
# AND that within that data folder there are site folders named with the NEON sitecode

# See WorkFlow_master.R to set the variables needed to run this script
# Variables needed: gh_repo, download_extract_dir, site.list

# Source unzip.neon fcn
source(file.path(gh_repo, "functions", "unzip.neon.R"))

## --------------------------------------------- ##
#                Unzipping -----
## --------------------------------------------- ##

for(sitecode in site.list){
  
  print(sitecode)
  
  # Unzip eddy-co bundled files
  unzip.neon(in_path = file.path(download_extract_dir, "data", sitecode, "filesToStack00200"), 
             out_path = file.path(download_extract_dir, "data", sitecode), 
             quiet = FALSE)
  
}

# EOF