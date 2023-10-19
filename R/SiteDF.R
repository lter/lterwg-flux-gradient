#' Site.DF
#'
#' @param sitecode NEON site code
#' @param frequency high (9m or 2m) low (30m)
#' @param startdate start date of desired data
#' @param enddate enddate of desired data
#' @param folder containing monthly h5 files
#'
#' @return df containing site co2, h2o, ch4 measurements at various tower heights
#' 
#'
#' @author Alexis Helgeson, Sam Jurado, David Reed, and Sparkle Malone
Site.DF <- function(folder, sitecode, frequency = "low", startdate, enddate){
  
  # #pulls 30m concentration, flux, and met data
  # if(frequency == "low"){
  #   ALL.data = data.frame()
  #   #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  #   for(i in 1:length(hd.files)){
  #     print(i)
  #     month.data <- hdf2df(hd.files[i], sitecode)
  #     ALL.data <-bind_rows(ALL.data, month.data)
  #     
  #   }
  #   #remove looping variable
  #   rm(month.data)
  #   #add date/time column
  #   ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  # }
  # #pulls 9m CH4 concentration, 2m co2/h2o concentration
  # if(frequency == "high"){
  #   ALL.data = data.frame()
  #   #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  #   for(i in 1:length(hd.files)){
  #     print(i)
  #     month.data <- cont.HF(hd.files[i], sitecode)
  #     ALL.data <-bind_rows(ALL.data, month.data)
  #     
  #   }
  #   #remove looping variable
  #   rm(month.data)
  #   #add date/time column
  #   #ALL.data$datetime <- as.POSIXct(ALL.data$timeEnd, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  # }
  
    ALL.data = list()
  
    #looping over all h5 files and extracting data over timeseries (startdate:enddate)
    for(i in 1:length(folder)){
      hd.file <-list.files(path = file.path(folder[i]), pattern="\\.h5$", full.names = T)
      print(i)
      # month.data <- met.cont.30m(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
      # month.data <- cont.9m(hd.file = hd.file, sitecode = sitecode)
      month.data <- met.1m(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
      
      ALL.data[[i]] <- month.data

    }
    #remove looping variable
    rm(month.data)
    
    #rbind similar df FOR 1M MET
    Tair.all <- data.frame()
    Press.all <- data.frame()
    WS3D.all <- data.frame()
    SWin.all <- data.frame()
    SWout.all <- data.frame()
    LWin.all <- data.frame()
    LWout.all <- data.frame()
    SoilHF.all <- data.frame()

    for (k in 1:length(ALL.data)) {
      grabMonth <- ALL.data[[k]]
      #grab air temp for all months and combine into one df
      grabTair <- grabMonth[[which(names(grabMonth) == "TAir")]]
      Tair.all <- bind_rows(Tair.all, grabTair)
      #grab air pressure for all months and combine into one df
      grabPress <- grabMonth[[which(names(grabMonth) == "Press")]]
      Press.all <- bind_rows(Press.all, grabPress)
      #grab 3D wind speed for all months and combine into one df
      grabWS3D <- grabMonth[[which(names(grabMonth) == "WS3D")]]
      WS3D.all <- bind_rows(WS3D.all, grabWS3D)
      #grab shortwave in for all months and combine into one df
      grabSWin <- grabMonth[[which(names(grabMonth) == "SWin")]]
      SWin.all <- bind_rows(SWin.all, grabSWin)
      #grab shortwave out for all months and combine into one df
      grabSWout <- grabMonth[[which(names(grabMonth) == "SWout")]]
      SWout.all <- bind_rows(SWout.all, grabSWout)
      #grab longwave in for all months and combine into one df
      grabLWin <- grabMonth[[which(names(grabMonth) == "LWin")]]
      LWin.all <- bind_rows(LWin.all, grabLWin)
      #grab longwave out for all months and combine into one df
      grabLWout <- grabMonth[[which(names(grabMonth) == "LWout")]]
      LWout.all <- bind_rows(LWout.all, grabLWout)
      #grab soil heat flux for all months and combine into one df
      grabSoilHF <- grabMonth[[which(names(grabMonth) == "SoilHF")]]
      SoilHF.all <- bind_rows(SoilHF.all, grabSoilHF)

    }
    
    #rbind similar df FOR 9M GAS
    # CH4.all <- data.frame()
    # CO2.all <- data.frame()
    # H2O.all <- data.frame()
    # 
    # for (k in 1:length(ALL.data)) {
    #   grabMonth <- ALL.data[[k]]
    #   #grab ch4 for all months and combine into one df
    #   grabCH4 <- grabMonth[[which(names(grabMonth) == "CH4")]]
    #   CH4.all <- bind_rows(CH4.all, grabCH4)
    #   #grab co2 for all months and combine into one df
    #   grabCO2 <- grabMonth[[which(names(grabMonth) == "CO2")]]
    #   CO2.all <- bind_rows(CO2.all, grabCO2)
    #   #grab h2o for all months and combine into one df
    #   grabH2O <- grabMonth[[which(names(grabMonth) == "H2O")]]
    #   H2O.all <- bind_rows(H2O.all, grabH2O)
    #   
    # }
    
    #rbind similar df FOR 30M MET/GAS
    # Tair.all <- data.frame()
    # Press.all <- data.frame()
    # WS3D.all <- data.frame()
    # SWin.all <- data.frame()
    # SWout.all <- data.frame()
    # LWin.all <- data.frame()
    # LWout.all <- data.frame()
    # SoilHF.all <- data.frame()
    # CH4.all <- data.frame()
    # CO2.all <- data.frame()
    # H2O.all <- data.frame()
    # MomRough.all <- data.frame()
    # Ufric.all <- data.frame()
    # H2O.850.all <- data.frame()
    # CO2.850.all <- data.frame()
    # F_co2.all <- data.frame()
    # F_H.all <- data.frame()
    # F_LE.all <- data.frame()
    # 
    # for (k in 1:length(ALL.data)) {
    #   grabMonth <- ALL.data[[k]]
    #   #grab air temp for all months and combine into one df
    #   grabTair <- grabMonth[[which(names(grabMonth) == "TAir")]]
    #   Tair.all <- bind_rows(Tair.all, grabTair)
    #   #grab air pressure for all months and combine into one df
    #   grabPress <- grabMonth[[which(names(grabMonth) == "Press")]]
    #   Press.all <- bind_rows(Press.all, grabPress)
    #   #grab 3D wind speed for all months and combine into one df
    #   grabWS3D <- grabMonth[[which(names(grabMonth) == "WS3D")]]
    #   WS3D.all <- bind_rows(WS3D.all, grabWS3D)
    #   #grab shortwave in for all months and combine into one df
    #   grabSWin <- grabMonth[[which(names(grabMonth) == "SWin")]]
    #   SWin.all <- bind_rows(SWin.all, grabSWin)
    #   #grab shortwave out for all months and combine into one df
    #   grabSWout <- grabMonth[[which(names(grabMonth) == "SWout")]]
    #   SWout.all <- bind_rows(SWout.all, grabSWout)
    #   #grab longwave in for all months and combine into one df
    #   grabLWin <- grabMonth[[which(names(grabMonth) == "LWin")]]
    #   LWin.all <- bind_rows(LWin.all, grabLWin)
    #   #grab longwave out for all months and combine into one df
    #   grabLWout <- grabMonth[[which(names(grabMonth) == "LWout")]]
    #   LWout.all <- bind_rows(LWout.all, grabLWout)
    #   #grab soil heat flux for all months and combine into one df
    #   grabSoilHF <- grabMonth[[which(names(grabMonth) == "SoilHF")]]
    #   SoilHF.all <- bind_rows(SoilHF.all, grabSoilHF)
    #   #grab ch4 for all months and combine into one df
    #   grabCH4 <- grabMonth[[which(names(grabMonth) == "CH4")]]
    #   CH4.all <- bind_rows(CH4.all, grabCH4)
    #   #grab co2 for all months and combine into one df
    #   grabCO2 <- grabMonth[[which(names(grabMonth) == "CO2")]]
    #   CO2.all <- bind_rows(CO2.all, grabCO2)
    #   #grab h2o for all months and combine into one df
    #   grabH2O <- grabMonth[[which(names(grabMonth) == "H2O")]]
    #   H2O.all <- bind_rows(H2O.all, grabH2O)
    #   #grab roughness length for all months and combine into one df
    #   grabMomRough <- grabMonth[[which(names(grabMonth) == "MomRough")]]
    #   MomRough.all <- bind_rows(MomRough.all, grabMomRough)
    #   #grab friction velocity for all months and combine into one df
    #   grabUfric <- grabMonth[[which(names(grabMonth) == "Ufric")]]
    #   Ufric.all <- bind_rows(Ufric.all, grabUfric)
    #   #grab 850 h2o for all months and combine into one df
    #   grabH2O.850 <- grabMonth[[which(names(grabMonth) == "H2O.850")]]
    #   H2O.850.all <- bind_rows(H2O.850.all, grabH2O.850)
    #   #grab 850 co2 for all months and combine into one df
    #   grabCO2.850 <- grabMonth[[which(names(grabMonth) == "CO2.850")]]
    #   CO2.850.all <- bind_rows(CO2.850.all, grabCO2.850)
    #   #grab co2 flux for all months and combine into one df
    #   grabF_co2 <- grabMonth[[which(names(grabMonth) == "F_co2")]]
    #   F_co2.all <- bind_rows(F_co2.all, grabF_co2)
    #   #grab H flux for all months and combine into one df
    #   grabF_H <- grabMonth[[which(names(grabMonth) == "F_H")]]
    #   F_H.all <- bind_rows(F_H.all, grabF_H)
    #   #grab LE flux for all months and combine into one df
    #   grabF_LE <- grabMonth[[which(names(grabMonth) == "F_LE")]]
    #   F_LE.all <- bind_rows(F_LE.all, grabF_LE)
    #   
    # 
    # }
  
    #grab relative humidity
    RH <- loadByProduct("DP1.00098.001", site="KONZ",
                        timeIndex=1, package="basic",
                        startdate=startdate, enddate=enddate,
                        check.size=F)
    RH <- RH$RH_1min %>%
      filter(horizontalPosition == "000") %>%
      mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
      select(TowerPosition, startDateTime, endDateTime, RHMean, RHFinalQF)
       

    # #grab 2D horizontal wind speed
    # WS2D <- loadByProduct("DP1.00001.001", site="KONZ",
    #                       timeIndex=30, package="basic",
    #                       startdate=startdate, enddate=enddate,
    #                       check.size=F)
    # WS2D <- WS2D$twoDWSD_30min %>%
    #   mutate(TowerPosition = as.numeric(verticalPosition)/10) %>%
    #   select(TowerPosition, startDateTime, endDateTime, windSpeedMean, windSpeedFinalQF)
    
    #FOR 1M MET
    DATA <- list(RH = RH, SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, Tair = Tair.all, Press = Press.all, WS3D = WS3D.all, SoilHF = SoilHF.all)
    
    #FOR 9M GAS
    #DATA <- list(CH4 = CH4.all, CO2 = CO2.all, H2O = H2O.all)
    
    #FOR 30M MET AND GAS
    # DATA <- list(RH = RH, WS2D = WS2D, SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, Tair = Tair.all, Press = Press.all, WS3D = WS3D.all, SoilHF = SoilHF.all, CH4 = CH4.all, CO2 = CO2.all, H2O = H2O.all, MomRough = MomRough.all, Ufric = Ufric.all, H2O.850 = H2O.850.all, CO2.850 = CO2.850.all, F_co2 = F_co2.all, F_H = F_H.all, F_LE = F_LE.all)
  
  return(DATA)
}
