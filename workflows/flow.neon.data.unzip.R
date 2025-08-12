#NOTE IMPORTANT INFORMATION: all of the .flow scripts are written assuming the end user has connect their R studio project to lterwg-flux-gradient GitHub AND that they have created a data folder AND that within that data folder there are site folders named with the NEON sitecode

#load libraries
# Set local dir
#setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient')

#source unzip.neon fcn
source(file.path("functions/unzip.neon.R"))

# Add all sites here:
# site.list <- c("BONA","CPER","GUAN","HARV","JORN","KONZ","NIWO","TOOL")
site.list <- c("ABBY","BARR","BART","BLAN",
               "CLBJ","DCFS","DEJU","DELA",
               "DSNY","GRSM","HEAL","JERC",
               "KONA","LAJA","LENO","MLBS",
               "MOAB","NOGP","OAES","ONAQ",
               "ORNL","OSBS","PUUM","RMNP",
               "SCBI","SERC","SJER","SOAP",
               "SRER","STEI","STER","TALL",
               "TEAK","TREE","UKFS","UNDE",
               "WOOD","WREF","YELL")

#localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient'
#setwd(localdir)

for( sitecode in site.list){
  print(sitecode)
  
  #unzip eddy-co bundled files
  unzip_neon(in_path = file.path("data", sitecode, "filesToStack00200"), out_path = file.path("data", sitecode), quiet = FALSE)
  
}

# EOF


