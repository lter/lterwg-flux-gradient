#load libraries
library(neonUtilities)
library(dplyr)
#set start and end dates for data, remember CH4 is only available (august 2021 - present)
startdate <- "2021-08"
enddate <- "2021-09"
#set NEON site code
sitecode <- 'KONZ'
#grab relative humidity at 1 min resolution
RH1min <- loadByProduct("DP1.00098.001", site=sitecode,
                    timeIndex=1, package="basic",
                    startdate=startdate, enddate=enddate,
                    check.size=F)
RH1min <- RH1min$RH_1min %>%
  filter(horizontalPosition == "000") %>%
  mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
  select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
#grab relative humidity at 30 min resolution
RH30min <- loadByProduct("DP1.00098.001", site=sitecode,
                        timeIndex=30, package="basic",
                        startdate=startdate, enddate=enddate,
                        check.size=F)
RH30min <- RH30min$RH_30min %>%
  filter(horizontalPosition == "000") %>%
  mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
  select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
#grab 2D horizontal wind speed at 30 min resolution
WS2D30min <- loadByProduct("DP1.00001.001", site=sitecode,
                      timeIndex=30, package="basic",
                      startdate=startdate, enddate=enddate,
                      check.size=F)
WS2D30min <- WS2D30min$twoDWSD_30min %>%
  mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
  select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
#grab 2D horizontal wind speed at 2 min resolution
WS2D2min <- loadByProduct("DP1.00001.001", site=sitecode,
                           timeIndex=2, package="basic",
                           startdate=startdate, enddate=enddate,
                           check.size=F)
WS2D2min <- WS2D2min$twoDWSD_2min %>%
  mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
  select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
#save as .Rdata object to be called again in flow.siteDF
DATA <- list(RH30min = RH30min, RH1min = RH1min, WS2D2min = WS2D2min, WS2D30min = WS2D30min)
save(DATA, file = paste0("data/", sitecode,"_NonEddyMetVars.Rdata"))
#grab bundled eddy-covariance data
zipsByProduct(dpID="DP4.00200.001", sitecode,startdate, enddate,package="basic", check.size=F, savepath = file.path("data", sitecode))