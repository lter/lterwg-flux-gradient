# Pull data from google drive
email <- 'jaclyn_matthes@g.harvard.edu'
site <- 'KONZ'

# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email)
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)
#focal_file = "KONZ_30m.zip"
for(focal_file in site_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(site_folder, name == focal_file)
  
  # Download that file
  pathDnld <- fs::path(dirTmp,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
# Unzip
  if(grepl(pattern='.zip',focal_file)){
    utils::unzip(pathDnld,exdir=dirTmp)
  }
  
}

# Extract 9 minute data
fileIn <- fs::path(dirTmp,paste0(site,'_9m.Rdata'))
load(fileIn)

# For each concentration, compute difference in concentation among tower levels
# m9.list <- list(Cont=list(CH4=CH4))
list.idx = seq_len(length(m9.list))
m9Diff.list <- lapply(list.idx,FUN=function(idx){
  var = m9.list[[idx]]
  scalar = names(m9.list)[idx]
  var$timeBgn <- as.POSIXct(strptime(var$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  var$timeEnd <- as.POSIXct(strptime(var$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
  var <- dplyr::arrange(var,timeBgn)
  var$TowerPosition <- as.numeric(var$TowerPosition)
  
  # Without any filtering, this provides concentration diff for each adjacent tower level AND the top/bottom. 
  # Do any filtering desired to restrict to particular tower levels (e.g. 1 & 3 to get that diff)
  
  # Create output data frame where conc diffs are in the same row
  OUT <- var
  var <- names(OUT)
  ncol <- ncol(OUT)
  nrow <- nrow(OUT)
  OUT[1:(nrow-1),(ncol+1):(ncol*2)] <- OUT[2:(nrow),]
  OUT <- OUT[1:(nrow-1),]
  
  names(OUT)[1:ncol] <- paste0(var,'_A')
  names(OUT)[(ncol+1):(ncol*2)] <- paste0(var,'_B')
  
  # Apply quality control
  timeChk <- OUT$timeBgn_B-OUT$timeEnd_A # looking for short positive lag (flush time is 1 min)
  if(scalar == "CH4"){
    bad <- timeChk < 45 | timeChk > 100 | OUT$qfFinl_A == 1 | OUT$qfFinl_B == 1
  } else {
    bad <- timeChk < 200 | timeChk > 300 | OUT$qfFinl_A == 1 | OUT$qfFinl_B == 1
  }
  OUT <- OUT[!bad,]
  
  # Compute tower level diff A-B
  # Swap any in which the diff is negative bc we always want higher level minus lower level
  diffTowerPosition<- OUT$TowerPosition_A-OUT$TowerPosition_B
  idxNeg <- diffTowerPosition < 0
  A <- OUT[idxNeg,1:ncol]
  B <- OUT[idxNeg,(ncol+1):(ncol*2)]
  OUT[idxNeg,1:ncol] <- B
  OUT[idxNeg,(ncol+1):(ncol*2)] <- A
  OUT$diffTowerPosition<- OUT$TowerPosition_A-OUT$TowerPosition_B
  OUT$dLevelsAminusB <- paste0(OUT$TowerPosition_A,'_',OUT$TowerPosition_B)
  
  # Compute concentration diffs
  OUT$dConc <- OUT$mean_A-OUT$mean_B
  OUT$timeMid <- OUT$timeEnd_A+0.5*(OUT$timeBgn_B-OUT$timeEnd_A)
  
  # Make match column for CH4 & CO2/H2O
  if(scalar == "CH4"){
    #####CHECK THIS WITH DIFFERENT TOWERS
    OUT$match_time <- OUT$timeMid + 1.5*60 # CO2 & H2O starts 1.5 min past CH4
  } else {
    OUT$match_time <- OUT$timeMid 
  }
  return(OUT)
})

# Reassign names from original list
names(m9Diff.list) = names(m9.list)

# Filter for values between 4-3
CO2 = m9Diff.list[["CO2"]][m9Diff.list[["CO2"]]$dLevelsAminusB=="4_3",]
H2O = m9Diff.list[["H2O"]][m9Diff.list[["H2O"]]$dLevelsAminusB=="4_3",]
CH4 = m9Diff.list[["CH4"]][m9Diff.list[["CH4"]]$dLevelsAminusB=="4_3",]

# Combine data frames for two scalars
scalar_combine = dplyr::full_join(CO2, H2O, by="timeMid")
scalar_combine = dplyr::filter(scalar_combine, !is.na(dConc.x) & !is.na(dConc.y))

scalar_combine = dplyr::full_join(CO2, CH4, by="match_time")
scalar_combine = dplyr::filter(scalar_combine, !is.na(dConc.x) & !is.na(dConc.y))
