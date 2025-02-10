#NOTE IMPORTANT INFORMATION: all of the .flow scripts are written assuming the end user has connect their R studio project to lterwg-flux-gradient GitHub AND that they have created a data folder AND that within that data folder there are site folders named with the NEON sitecode

#load libraries
library(neonUtilities)
library(dplyr)

#set start and end dates for data, REMEMBER CH4 is only available (august 2021 - present)
startdate <- "2021-08"
enddate <- "2023-09"

# Add all sites here:
site.list <- c("BONA","CPER","GUAN","HARV","JORN","KONZ","NIWO","TOOL")

# Add local directory for downloaded data here:
localdir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient'
setwd(localdir)

#set include.provisional = T to get full time series of data up to present, currently provisional covers (2022-07:2023-09) 
#grab relative humidity at 1 min resolution

for ( sitecode in site.list){
  print(sitecode)
  RH1min <- loadByProduct("DP1.00098.001", site=sitecode,
                          timeIndex=1, package="basic",
                          startdate=startdate, enddate=enddate,
                          check.size=F, include.provisional = T)
  RH1min <- RH1min$RH_1min %>%
    filter(horizontalPosition == "000") %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  #grab relative humidity at 30 min resolution
  RH30min <- loadByProduct("DP1.00098.001", site=sitecode,
                           timeIndex=30, package="basic",
                           startdate=startdate, enddate=enddate,
                           check.size=F, include.provisional = T)
  RH30min <- RH30min$RH_30min %>%
    filter(horizontalPosition == "000") %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
  #grab 2D horizontal wind speed at 30 min resolution
  WS2D30min <- loadByProduct("DP1.00001.001", site=sitecode,
                             timeIndex=30, package="basic",
                             startdate=startdate, enddate=enddate,
                             check.size=F, include.provisional = T)
  WS2D30min <- WS2D30min$twoDWSD_30min %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
  #grab 2D horizontal wind speed at 2 min resolution
  WS2D2min <- loadByProduct("DP1.00001.001", site=sitecode,
                            timeIndex=2, package="basic",
                            startdate=startdate, enddate=enddate,
                            check.size=F, include.provisional = T)
  WS2D2min <- WS2D2min$twoDWSD_2min %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
  #grab PAR at 1 min resolution
  PAR1min <- loadByProduct("DP1.00024.001", site=sitecode,
                           timeIndex=1, package="basic",
                           startdate=startdate, enddate=enddate,
                           check.size=F, include.provisional = T)
  PAR1min <- PAR1min$PARPAR_1min %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, PARMean, PARFinalQF)
  #grab PAR at 30 min resolution
  PAR30min <- loadByProduct("DP1.00024.001", site=sitecode,
                            timeIndex=30, package="basic",
                            startdate=startdate, enddate=enddate,
                            check.size=F, include.provisional = T)
  PAR30min <- PAR30min$PARPAR_30min %>%
    mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    select(TowerPosition, startDateTime, endDateTime, PARMean, PARFinalQF)
  
  # load(paste0("data/", sitecode, "/", sitecode,"_NonEddyMetVars.Rdata"))
  # DATA$PAR1min = PAR1min
  # DATA$PAR30min = PAR30min
  #save as .Rdata object to be called again in flow.siteDF
  DATA <- list(RH30min = RH30min, RH1min = RH1min, WS2D2min = WS2D2min, WS2D30min = WS2D30min, PAR1min = PAR1min, PAR30min = PAR30min)
  # #create necessary sub-folder(s)
  dir.create(path = file.path("data"), showWarnings = F)
  dir.create(path = file.path("data", sitecode), showWarnings = F)
  save(DATA, file = paste0("data/", sitecode, "/", sitecode,"_NonEddyMetVars.Rdata"))
  #grab bundled eddy-covariance data
  zipsByProduct(dpID="DP4.00200.001", sitecode,startdate, enddate,package="basic", check.size=F, savepath = file.path("data", sitecode), include.provisional = T)
  
}

