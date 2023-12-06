#' Compile30min
#'
#' @param sitecode NEON site code 
#' @param h5files list of h5 filepaths
#'
#' @return list of dataframes containing Tair, Press, WS3D, SWin, Swout, LWin, LWout, SoilHF, CH4, CO2, H2O gas concentrations and fluxes, MomRough, and Ufric at 30 min resolution 
#'
#' @author Alexis Helgeson
#' 
Compile30min <- function(h5files, sitecode){
  #create empty list to store monthly data
  ALL.data = list()
  
  #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  for(i in 1:length(h5files)){
    hd.file <- h5files[i]
    print(i)
    month.data <- met.cont.30min(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
    
    ALL.data[[i]] <- month.data
    
  }
  #remove looping variable
  rm(month.data)
  
  #rbind similar df FOR 30M MET/GAS
  Tair.all <- data.frame()
  Press.all <- data.frame()
  WS3D.all <- data.frame()
  SWin.all <- data.frame()
  SWout.all <- data.frame()
  LWin.all <- data.frame()
  LWout.all <- data.frame()
  SoilHF.all <- data.frame()
  CH4.all <- data.frame()
  CO2.all <- data.frame()
  H2O.all <- data.frame()
  MomRough.all <- data.frame()
  Ufric.all <- data.frame()
  H2O.850.all <- data.frame()
  CO2.850.all <- data.frame()
  F_co2.all <- data.frame()
  F_H.all <- data.frame()
  F_LE.all <- data.frame()

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
    #grab ch4 for all months and combine into one df
    grabCH4 <- grabMonth[[which(names(grabMonth) == "CH4")]]
    CH4.all <- bind_rows(CH4.all, grabCH4)
    #grab co2 for all months and combine into one df
    grabCO2 <- grabMonth[[which(names(grabMonth) == "CO2")]]
    CO2.all <- bind_rows(CO2.all, grabCO2)
    #grab h2o for all months and combine into one df
    grabH2O <- grabMonth[[which(names(grabMonth) == "H2O")]]
    H2O.all <- bind_rows(H2O.all, grabH2O)
    #grab roughness length for all months and combine into one df
    grabMomRough <- grabMonth[[which(names(grabMonth) == "MomRough")]]
    MomRough.all <- bind_rows(MomRough.all, grabMomRough)
    #grab friction velocity for all months and combine into one df
    grabUfric <- grabMonth[[which(names(grabMonth) == "Ufric")]]
    Ufric.all <- bind_rows(Ufric.all, grabUfric)
    #grab 850 h2o for all months and combine into one df
    grabH2O.850 <- grabMonth[[which(names(grabMonth) == "H2O.850")]]
    H2O.850.all <- bind_rows(H2O.850.all, grabH2O.850)
    #grab 850 co2 for all months and combine into one df
    grabCO2.850 <- grabMonth[[which(names(grabMonth) == "CO2.850")]]
    CO2.850.all <- bind_rows(CO2.850.all, grabCO2.850)
    #grab co2 flux for all months and combine into one df
    grabF_co2 <- grabMonth[[which(names(grabMonth) == "F_co2")]]
    F_co2.all <- bind_rows(F_co2.all, grabF_co2)
    #grab H flux for all months and combine into one df
    grabF_H <- grabMonth[[which(names(grabMonth) == "F_H")]]
    F_H.all <- bind_rows(F_H.all, grabF_H)
    #grab LE flux for all months and combine into one df
    grabF_LE <- grabMonth[[which(names(grabMonth) == "F_LE")]]
    F_LE.all <- bind_rows(F_LE.all, grabF_LE)


  }
  
  
  #FOR 30M MET AND GAS
  DATA <- list(SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, Tair = Tair.all, Press = Press.all, WS3D = WS3D.all, SoilHF = SoilHF.all, CH4 = CH4.all, CO2 = CO2.all, H2O = H2O.all, MomRough = MomRough.all, Ufric = Ufric.all, H2O.850 = H2O.850.all, CO2.850 = CO2.850.all, F_co2 = F_co2.all, F_H = F_H.all, F_LE = F_LE.all)
  
  return(DATA)
  
}