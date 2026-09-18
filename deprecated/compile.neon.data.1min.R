#' compile.neon.data.1min
#'
#' @param sitecode NEON site code 
#' @param h5files list of h5 files
#'
#' @return list of data frames containing Tair, Press, WS3D, SWin, Swout, LWin, LWout, SoilHF at 1 min resolution
#'
#' @author Alexis Helgeson
compile.neon.data.1min <- function(h5files, sitecode){
  #create empty list to store monthly data
  ALL.data = list()
  
  #looping over all h5 files and extracting data over timeseries (startdate:enddate)
  for(i in 1:length(h5files)){
    hd.file <- h5files[i]
    print(i)
    month.data <- grab.neon.met.1min(hd.file = hd.file, sitecode = sitecode, startdate = startdate, enddate = enddate)
    
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
  
  #FOR 1M MET
  DATA <- list(SWin = SWin.all, SWout = SWout.all, LWin = LWin.all, LWout = LWout.all, Tair = Tair.all, Press = Press.all, WS3D = WS3D.all, SoilHF = SoilHF.all)
  
  return(DATA)
}