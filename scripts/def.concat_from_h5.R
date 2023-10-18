def.concat_from_h5 <- function(pathUnzipped,dirHdf5){
  files <- list.files(path=pathUnzipped,pattern='h5')
  
  numDir <- base::length(dirHdf5)
  data <- vector(mode='list',length=numDir)
  names(data) <- dirHdf5
  data <- lapply(data,FUN=function(l){list()})
  for (file in files){
    # Is the file zipped?
    if(grepl(pattern='.gz',file)){
      system(paste0('gzip -d ',fs::path(pathUnzipped,file)))
      file <- base::substr(file,1,nchar(file)-3)
    }
    
    # Grab the file structure
    listObj <- base::try(rhdf5::h5ls(fs::path(pathUnzipped,file), datasetinfo = FALSE),silent=TRUE)
    
    if (base::class(listObj) == "try-error"){
      base::stop('Cannot open file. Aborting...')
    }
    
    # Combine path and name
    listObjName <- base::paste(listObj$group, listObj$name, sep = "/") # combined path and name
    
    # Read in the data
    for(idxDir in base::seq_len(numDir)){
      dataIdx <- base::try(rhdf5::h5read(file=fs::path(pathUnzipped,file),name=dirHdf5[idxDir]),silent=FALSE)
      
      # Error-check
      if(base::class(dataIdx) == 'try-error'){
        # Close the file
        rhdf5::h5closeAll()
        
        # # Remove downloaded file if selected
        # if(Rm){
        #   base::unlink(file)
        # }
        
        # Did we want the whole file?
        if(dirHdf5[idxDir] == '/'){
          base::stop('File is unreadable. Aborting...')
        } else {
          base::stop(base::paste0('Could not retrieve ',dirHdf5[idxDir],' from file ',file$name[idxFile],'. Aborting...'))
        }
      } else if (base::any(base::class(dataIdx) != 'data.frame') && Pack == 'expanded'){
        # Close the file
        rhdf5::h5closeAll()
        
        # Remove downloaded file if selected
        if(Rm){
          base::unlink(file)
        }
        
        stop(base::paste0('Cannot return results from the expanded package when dirHdf5 (',dirHdf5[idxDir],') does not end in a dataset.'))
      }
      
      data[[dirHdf5[idxDir]]][[file]] <- dataIdx
      
    }
    
    # Close the file
    rhdf5::h5closeAll()
    
    # Remove downloaded file if selected
    # if(Rm){
    #   base::unlink(file)
    # }
  }

  # Stack the files
  dataStacked <- lapply(data,FUN=function(idxData){do.call(rbind,idxData)})
  
  # Extract and populate the site, HOR & VER, and quantity (e.g. rtioMoleDryCh4) within the data frame
  for (idxDf in seq_len(length(dataStacked))){
    pathSplt <- strsplit(dirHdf5[idxDf],'/')[[1]]
    site <- pathSplt[2]
    hor_ver_tmi <- pathSplt[6]
    var <- pathSplt[7]
    HVTSplt <- strsplit(hor_ver_tmi,'_')[[1]]
    ver <- HVTSplt[2]
    tmi <- HVTSplt[3]
    
    df <- dataStacked[[idxDf]]
    df[['site']] <- site
    df[['var']] <- var
    df[['ver']] <- ver
    df[['tmi']] <- tmi
    
    #Reassign
    dataStacked[[idxDf]] <- df
  }
  
  # STACK THEM ALL
  dataAll <- do.call('rbind',dataStacked)
  
  # Convert the timestamps
  dataAll$timeBgn <- strptime(dataAll$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')
  dataAll$timeEnd <- strptime(dataAll$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT')
  return(dataAll)
}