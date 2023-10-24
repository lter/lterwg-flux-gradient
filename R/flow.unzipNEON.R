#load libraries
#source unzip_neon fcn
source(file.path("R/unzip_neon.R"))
#set NEON site code
sitecode <- 'KONZ'
#unzip eddy-co bundled files
unzip_neon(in_path = file.path("data", sitecode), out_path = file.path("data", sitecode), quiet = TRUE)