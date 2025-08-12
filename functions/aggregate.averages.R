#' Aggregate average measurements over short windows to longer windows
#'
#' @param timeBgn Vector. POSIXct start time associated with measurement
#' @param timeEnd Vector. POSIXct end time associated with measurement
#' @param meas Required. Data frame. Average measurement from each timeBgn to timeEnd. May contain multiple columns.
#' @param timeAgrBgn Required. Vector. POSIXct start time of desired aggregation window
#' @param timeAgrEnd Required. Vector. POSIXct end time of desired aggregation window
#' @param na.rm Optional. Default = FALSE. Ignore NA values within each window? TRUE will
#' return a non-NA value for the aggregated window is any value is not NA in the window. FALSE
#' will return NA for the aggregated window if any value within the window is NA. 
#'
#' @return aggregated measurements for each pair of timeAgrBgn to timeAgrEnd. Only full 
#' windows of timeBgn to timeEnd that fit entirely within timeAgrBgn to timeAgrEnd are 
#' averaged. 
#' 
#' @author Jackie Matthes and Cove Sturtevant
#'
#' 
#' 
# Change-log and author contributions / copyrights
#   Cove Sturtevant (2023-10-26)
#     original creation
#   Cove Sturtevant (2024-11-11)
#     optimize performance
#' ----------------------------------------------------------------------------------
aggregate.averages <- function(timeBgn,
                               timeEnd,
                               meas,
                               timeAgrBgn,
                               timeAgrEnd,
                               na.rm = FALSE){
  
  numMeas <- length(timeBgn)
  numOut <- length(timeAgrBgn)  
  measAgr <- matrix(data = NA, nrow = numOut, ncol = ncol(meas))
  measAgr <- as.data.frame(measAgr)
  names(measAgr) <- names(meas)
  
  # Sort the data
  setOrdr <- base::order(timeBgn,decreasing=FALSE)
  timeBgn <- timeBgn[setOrdr]
  timeEnd <- timeEnd[setOrdr]
  meas <- meas[setOrdr,]
  
  # Error check
  if(length(timeBgn) != numMeas || length(timeEnd) != numMeas){
    stop('Lengths of timeBgn, timeEnd, and meas must be identical.')
  }

  if(length(timeAgrEnd) != numOut){
    stop('Lengths of timeAgrBgn and timeAgrEnd must be identical.')
  }

  # Another approach would be to create a rolling mean for each possible window length, 
  # then assign the outputs based on the window size needed.
  # find start and end index of the window to aggregate for each data value
  # compute the number of data points involved in each window (non-regular data)
  # compute those rolling windows
  

  # If the dataset is large, split it up
  maxLen <- 50000
  numBrk <- ceiling(numMeas/maxLen)
  compAgrAll <- rep(FALSE,numOut) # Make sure we cover all aggregation windows
  setRpt <- seq(from=1,to=numOut,by=5000) # Cadence to report progress
  
  for (idxBrk in 1:numBrk){
    message(paste0(Sys.time(), ': Processing ',idxBrk,' out of ',numBrk, ' data blocks'))

    # Data
    idxBgn <- (idxBrk-1)*maxLen+1
    idxEnd <- min(idxBrk*maxLen,numMeas)
    timeBgnIdx <- timeBgn[idxBgn:idxEnd]
    timeEndIdx <- timeEnd[idxBgn:idxEnd]
    measIdx <- meas[idxBgn:idxEnd,,drop=FALSE]
    
    # Aggregation windows applying to this data
    setAgr <- which(timeAgrBgn >= min(timeBgnIdx) & timeAgrEnd <= max(timeEndIdx))
    if (length(setAgr) == 0){
      next
    }
    timeAgrBgnIdx <- timeAgrBgn[setAgr]
    timeAgrEndIdx <- timeAgrEnd[setAgr]
    compAgrAll[setAgr] <- TRUE # Record which aggregation windows we have covered
    
    # Compute the aggregated averages for each window
    Cmeans <- lapply(seq_len(length(setAgr)),FUN=function(idxAgr){
      idxSetAgr <- setAgr[idxAgr]
      dataIdx <- measIdx[timeBgnIdx >= timeAgrBgnIdx[idxAgr] & timeEndIdx <= timeAgrEndIdx[idxAgr],,drop=FALSE]
      return(colMeans(dataIdx,na.rm=na.rm))
    })
    Cmeans <- as.data.frame(matrix(unlist(Cmeans),ncol=length(meas),byrow=TRUE))
    names(Cmeans) <- names(meas)
    measAgr[setAgr,] <- Cmeans

    # Slower version
    # for(idxAgr in seq_len(length(setAgr))){
    #   idxSetAgr <- setAgr[idxAgr]
    #   dataIdx <- measIdx[timeBgnIdx >= timeAgrBgnIdx[idxAgr] & timeEndIdx <= timeAgrEndIdx[idxAgr],,drop=FALSE]
    #   measAgr[idxSetAgr,] <- colMeans(dataIdx,na.rm=na.rm)
    # }
    
  }  

  # For the windows that spanned the data breaks, compute them with the full dataset
  setAgr <- which(!compAgrAll)
  lenSetAgr <- length(setAgr)
  if(lenSetAgr > 0){
    compAgrAll[setAgr] <- TRUE
    message(paste0('Processing ', lenSetAgr, ' windows missed b/c they span spanned data blocks'))
    
    Cmeans <- lapply(setAgr,FUN=function(idxAgr){
      dataIdx <- meas[timeBgn >= timeAgrBgn[idxAgr] & timeEnd <= timeAgrEnd[idxAgr],,drop=FALSE]
      return(colMeans(dataIdx,na.rm=na.rm))
    })
    Cmeans <- as.data.frame(matrix(unlist(Cmeans),ncol=length(meas),byrow=TRUE))
    names(Cmeans) <- names(meas)
    measAgr[setAgr,] <- Cmeans
    
    # Slower version
    # for(idxAgr in setAgr){
    #   dataIdx <- meas[timeBgn >= timeAgrBgn[idxAgr] & timeEnd <= timeAgrEnd[idxAgr],,drop=FALSE]
    #   measAgr[idxAgr,] <- colMeans(dataIdx,na.rm=na.rm)
    # }
  }
  
  
  return(measAgr)
  
}
  