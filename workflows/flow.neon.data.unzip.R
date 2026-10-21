## --------------------------------------------- ##
#               Housekeeping -----
## --------------------------------------------- ##
# Purpose:
# Unzips downloaded NEON data files

# NOTE IMPORTANT INFORMATION: 
# all of the flow scripts are written assuming the end user has connected their R studio project to the lterwg-flux-gradient GitHub repo 
# AND that they have created a data folder 
# AND that within that data folder there are site folders named with the NEON sitecode

# Set local dir
#setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient')

# Source unzip.neon fcn
source(file.path("functions", "unzip.neon.R"))

# Add all sites here:
site.list <- c("ABBY", "BARR", "BART", "BLAN",
               "BONA", "CLBJ", "CPER", "DCFS",
               "DEJU", "DELA", "DSNY", "GRSM",
               "GUAN", "HARV", "HEAL", "JERC",
               "JORN", "KONA", "KONZ", "LAJA",
               "LENO", "MLBS", "MOAB", "NIWO",
               "NOGP", "OAES", "ONAQ", "ORNL",
               "OSBS", "PUUM", "RMNP", "SCBI",
               "SERC", "SJER", "SOAP", "SRER",
               "STEI", "STER", "TALL", "TEAK",
               "TOOL", "TREE", "UKFS", "UNDE",
               "WOOD", "WREF", "YELL")

## --------------------------------------------- ##
#                Unzipping -----
## --------------------------------------------- ##

for(sitecode in site.list){
  
  print(sitecode)
  
  # Unzip eddy-co bundled files
  unzip.neon(in_path = file.path("data", sitecode, "filesToStack00200"), 
             out_path = file.path("data", sitecode), 
             quiet = FALSE)
  
}

# EOF


