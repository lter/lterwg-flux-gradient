# NOTE IMPORTANT INFORMATION: 
# all of the flow scripts are written assuming the end user has connected their R studio project to the lterwg-flux-gradient GitHub repo 
# AND that they have created a data folder 
# AND that within that data folder there are site folders named with the NEON sitecode

# Load libraries
library(neonUtilities)
library(dplyr)

# Set start and end dates for data, REMEMBER CH4 is only available (august 2021 - present)
startdate <- "2021-08"
enddate <- "2024-06"

# Add all sites here:
# site.list <- c("BONA","CPER","GUAN","HARV")
# site.list <- c("JORN","KONZ","NIWO","TOOL")
# site.list <- c("ABBY","BARR","BART","BLAN")
# site.list <- c("CLBJ","DCFS","DEJU","DELA")
# site.list <- c("DSNY","GRSM","HEAL","JERC")
# site.list <- c("KONA","LAJA","LENO","MLBS")
# site.list <- c("MOAB","NOGP","OAES","ONAQ")
# site.list <- c("ORNL","OSBS","PUUM","RMNP")
# site.list <- c("SCBI","SERC","SJER","SOAP")
# site.list <- c("SRER","STEI","STER","TALL")
# site.list <- c("TEAK","TREE","UKFS","UNDE")
# site.list <- c("WOOD","WREF","YELL")
site.list <- c("HARV")

# Add local directory for downloaded data here:
#localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient'
#setwd(localdir)

# Set include.provisional = T to get full time series of data up to present

for (sitecode in site.list){
  
  print(sitecode)
  
  # Grab relative humidity at 1 min resolution
  RH1min <- neonUtilities::loadByProduct("DP1.00098.001", 
                                         site = sitecode,
                                         timeIndex = 1, 
                                         package = "basic",
                                         startdate = startdate, 
                                         enddate = enddate,
                                         check.size = F, 
                                         include.provisional = T)
  
  RH1min <- RH1min$RH_1min %>%
    dplyr::filter(horizontalPosition == "000") %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  
  # Grab relative humidity at 30 min resolution
  RH30min <- neonUtilities::loadByProduct("DP1.00098.001", 
                                          site = sitecode,
                                          timeIndex = 30, 
                                          package = "basic",
                                          startdate = startdate, 
                                          enddate = enddate,
                                          check.size = F, 
                                          include.provisional = T)
  
  RH30min <- RH30min$RH_30min %>%
    dplyr::filter(horizontalPosition == "000") %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  
  # Grab 2D horizontal wind speed at 30 min resolution
  WS2D30min <- neonUtilities::loadByProduct("DP1.00001.001", 
                                            site = sitecode,
                                            timeIndex = 30, 
                                            package = "basic",
                                            startdate = startdate, 
                                            enddate = enddate,
                                            check.size = F, 
                                            include.provisional = T)
  
  WS2D30min <- WS2D30min$twoDWSD_30min %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
  
  # Grab 2D horizontal wind speed at 2 min resolution
  WS2D2min <- neonUtilities::loadByProduct("DP1.00001.001", 
                                           site = sitecode,
                                           timeIndex = 2, 
                                           package = "basic",
                                           startdate = startdate, 
                                           enddate = enddate,
                                           check.size = F, 
                                           include.provisional = T)
  
  WS2D2min <- WS2D2min$twoDWSD_2min %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
  
  # Grab PAR at 1 min resolution
  PAR1min <- neonUtilities::loadByProduct("DP1.00024.001", 
                                          site = sitecode,
                                          timeIndex = 1, 
                                          package = "basic",
                                          startdate = startdate, 
                                          enddate = enddate,
                                          check.size = F, 
                                          include.provisional = T)
  
  PAR1min <- PAR1min$PARPAR_1min %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, PARMean, PARFinalQF)
  
  # Grab PAR at 30 min resolution
  PAR30min <- neonUtilities::loadByProduct("DP1.00024.001", 
                                           site = sitecode,
                                           timeIndex = 30, 
                                           package = "basic",
                                           startdate = startdate, 
                                           enddate = enddate,
                                           check.size = F, 
                                           include.provisional = T)
  
  PAR30min <- PAR30min$PARPAR_30min %>%
    dplyr::mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    dplyr::select(TowerPosition, startDateTime, endDateTime, PARMean, PARFinalQF)
  
  # load(paste0("data/", sitecode, "/", sitecode,"_NonEddyMetVars.Rdata"))
  # DATA$PAR1min = PAR1min
  # DATA$PAR30min = PAR30min
  # save as .Rdata object to be called again in flow.siteDF
  
  DATA <- list(RH30min = RH30min, 
               RH1min = RH1min, 
               WS2D2min = WS2D2min, 
               WS2D30min = WS2D30min, 
               PAR1min = PAR1min, 
               PAR30min = PAR30min)
  
  # Create necessary sub-folder(s)
  dir.create(path = file.path("data"), showWarnings = F)
  dir.create(path = file.path("data", sitecode), showWarnings = F)
  save(DATA, file = file.path("data", sitecode, paste0(sitecode, "_NonEddyMetVars.Rdata")))
  
  # Grab bundled eddy-covariance data
  neonUtilities::zipsByProduct(dpID = "DP4.00200.001", 
                               site = sitecode, 
                               startdate = startdate, 
                               enddate = enddate, 
                               package = "expanded", 
                               check.size = F, 
                               savepath = file.path("data", sitecode), 
                               include.provisional = T)
  
}

