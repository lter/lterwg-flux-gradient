#load libraries
#source unzip_neon fcn
source(file.path("R/unzip_neon.R"))
#set NEON site code
sitecode <- 'GUAN'
#unzip eddy-co bundled files
unzip_neon(in_path = file.path("data", sitecode, "filesToStack00200"), out_path = file.path("data", sitecode), quiet = TRUE)
