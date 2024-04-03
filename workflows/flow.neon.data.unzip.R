#NOTE IMPORTANT INFORMATION: all of the .flow scripts are written assuming the end user has connect their R studio project to lterwg-flux-gradient GitHub AND that they have created a data folder AND that within that data folder there are site folders named with the NEON sitecode
#load libraries
#source unzip_neon fcn
source(file.path("functions/unzip_neon.R"))
#set NEON site code
sitecode <- 'TOOL'
#unzip eddy-co bundled files
unzip_neon(in_path = file.path("data", sitecode, "filesToStack00200"), out_path = file.path("data", sitecode), quiet = TRUE)
