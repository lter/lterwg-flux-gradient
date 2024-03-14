#' ustar.threshold
#'
#' @param site df of calculated flux from specific site taken from Validation df
#'
#' @return df with flag for ustar values beyond threshold
#' 
#'
#' @author Alexis Helgeson
ustar.threshold <- function(site){
  #grab NEE, Tair, SW_in, Ustar, RH and filter for good data
  F_CO2 <- min30.list$F_co2 %>% filter(nsae.qfFinl==0)
  Tair <- min30.list$Tair %>% filter(qfFinl==0) %>% filter(TowerPosition==max(TowerPosition)) #filter to top of tower
  SW_in <- min30.list$SWin %>% filter(qfFinl==0)
  Ustar <- min30.list$Ufric %>% filter(qfFinl==0)
  RH <- min30.list$RH %>% filter(RHFinalQF==0)
  #select for only variable and timeEnd
  F_CO2 <- F_CO2 %>% select(timeEnd, nsae)
  Tair <- Tair %>% select(timeEnd, mean)
  SW_in <- SW_in %>% select(timeEnd, mean)
  Ustar <- Ustar %>% select(timeEnd, veloFric)
  RH <- RH %>% select(endDateTime, RHMean)
  #rename columns to match REddyProc
  names(F_CO2) <- c("DateTime", "NEE")
  names(Tair) <- c("DateTime", "Tair")
  names(SW_in) <- c("DateTime", "Rg")
  names(Ustar) <- c("DateTime", "Ustar")
  names(RH) <- c("DateTime", "RH")
  #fix DateTime column
  F_CO2$DateTime <- as.POSIXct(strptime(F_CO2$DateTime,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  Tair$DateTime <- as.POSIXct(strptime(Tair$DateTime,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  SW_in$DateTime <- as.POSIXct(strptime(SW_in$DateTime,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  Ustar$DateTime <- as.POSIXct(strptime(Ustar$DateTime,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  RH$DateTime <- round(as.POSIXct(RH$DateTime),units='mins')
  #combine df
  EddyData <- merge(F_CO2, Tair, all = TRUE)
  EddyData <- merge(EddyData, SW_in, all = TRUE)
  EddyData <- merge(EddyData, Ustar, all = TRUE)
  EddyData <- merge(EddyData, RH, all = TRUE)
  #calculate VPD from air temperature, Tair in C need to convert to K for calculation by adding 273.15
  EddyData$esat_Pa = 611.2*exp(17.67*((EddyData$Tair + 273.15)-273.16)/((EddyData$Tair + 273.15)-29.65))
  EddyData$e_Pa = EddyData$RH*EddyData$esat_Pa/100 #[Pa] vapor water pressure
  EddyData$VPD = (EddyData$esat_Pa-EddyData$e_Pa)/1000
  #add Year, DoY, Hour columns match class to REddyProc
  EddyData$Year <- as.integer(year(EddyData$DateTime))
  EddyData$DoY <- as.integer(yday(EddyData$DateTime))
  EddyData$Hour <- as.numeric(hour(EddyData$DateTime))
  #call REddyProc functions
  outDir <- tempdir()
  #remove rows of not equidistant half hours
  #round to nearest half hour
  EddyData$DateTime <- round_date(EddyData$DateTime, "30 minutes")
  diff.time <- round(as.numeric(diff(EddyData$DateTime))/60)
  keep.rows <- which(diff.time==30)
  #add row column for easy removal of non equidistant half hours
  #EddyData$ID <- seq(1, nrow(EddyData), 1)
  EddyData.HH <- as.data.frame(EddyData[c(keep.rows, keep.rows + 1),])
  #round DateTime column to nearest half hour for fcn
  #EddyData.HH$DateTime <- round_date(EddyData.HH$DateTime, "30 minutes")
  #remove additional rows not captured by diff.time (given by fcn)
  # remove.rows <- c(2195, 2196, 6276, 6750, 6770, 7244, 8578, 13559, 13560, 15372, 15547, 15892, 16555, 17016, 17710, 19395, 19610, 19870, 19939, 20873, 22275, 22710, 23084, 23182, 23313, 23390, 23410, 23571, 23972, 24265, 24282, 24939, 25491, 26089, 26326, 26834, 26941, 27449, 27457, 28107, 30772, 31806, 31891, 32220, 32346, 32355, 32394, 32440, 32524, 32721, 32940, 33180, 33215, 33217, 33225, 33257, 33308, 33488, 33495, 33553, 34064, 35192, 36440)
  # EddyData.HH <- EddyData.HH[-remove.rows,]
  #add NA rows so that every row is 30min apart
  datetime.range <- seq(from = min(EddyData$DateTime), to = max(EddyData$DateTime), by = "30 min")
  # datetime.range <- datetime.range[!datetime.range %in% EddyData.HH$DateTime]
  datetime_df <- data.frame(DateTime = datetime.range, NEE = NA, Tair = NA, Rg = NA, Ustar = NA, Year = year(datetime.range), DoY = yday(datetime.range), Hour = hour(datetime.range), RH = NA, esat_Pa = NA, e_Pa = NA, VPD = NA)
  # EddyData.HH <- rbind(EddyData.HH, datetime_df)
  #reorder dataframe so datetime is sequential
  # EddyData.HH <- EddyData.HH[order(EddyData.HH$DateTime),]
  
  #create new dataframe for fcn
  datetime_df <- data.frame(DateTime = datetime.range, NEE = NA, Tair = NA, Rg = NA, Ustar = NA, Year = year(datetime.range), DoY = yday(datetime.range), Hour = hour(datetime.range), RH = NA, esat_Pa = NA, e_Pa = NA, VPD = NA)
  #populate with data
  
  
  # #filter for long runs of equal NEE values
  # EddyDataWithPosix.test <- fConvertTimeToPosix(
  #   filterLongRuns(EddyData, "NEE")
  #   , 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
  #calculate ustar threshold
  EProc.test <- sEddyProc$new(
    'DE-Tha', EddyData.HH, c('NEE','Rg','Tair','VPD', 'Ustar'))
  #estimating the thresholds based on the data (without bootstrap)
  (uStarTh <- EProc$sEstUstarThold())
  # may plot saturation of NEE with UStar for a specified season to pdf
  EProc$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3], dir = outDir )
  
}