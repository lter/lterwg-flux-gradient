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
#' @export
#'
#' @examples
#' 
#' 
# Change-log and author contributions / copyrights
#   Cove Sturtevant (2023-10-26)
#     original creation
#' ----------------------------------------------------------------------------------
aggregate_averages <- function(timeBgn,
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
  
  # Error check
  if(length(timeBgn) != numMeas || length(timeEnd) != numMeas){
    stop('Lengths of timeBgn, timeEnd, and meas must be identical.')
  }

  if(length(timeAgrEnd) != numOut){
    stop('Lengths of timeAgrBgn and timeAgrEnd must be identical.')
  }

  # If the dataset is large, split it up
  maxLen <- 100000
  numBrk <- ceiling(numMeas/maxLen)
  compAgrAll <- rep(FALSE,numOut) # Make sure we cover all aggregation windows
  setRpt <- seq(from=1,to=numOut,by=5000) # Cadence to report progress
  for (idxBrk in 1:numBrk){
    # Data
    idxBgn <- (idxBrk-1)*maxLen+1
    idxEnd <- min(idxBrk*maxLen,numMeas)
    timeBgnIdx <- timeBgn[idxBgn:idxEnd]
    timeEndIdx <- timeEnd[idxBgn:idxEnd]
    measIdx <- meas[idxBgn:idxEnd,,drop=FALSE]
    
    # Aggregation windows applying to this data
    setAgr <- which(timeAgrBgn >= min(timeBgnIdx) & timeAgrEnd <= max(timeEndIdx))
    timeAgrBgnIdx <- timeAgrBgn[setAgr]
    timeAgrEndIdx <- timeAgrEnd[setAgr]
    compAgrAll[setAgr] <- TRUE # Record which aggregation windows we have covered
    
    # Compute the aggregated averages for each window
    for(idxAgr in seq_len(length(setAgr))){
      idxSetAgr <- setAgr[idxAgr]
      if(idxSetAgr %in% setRpt){
        message(paste0(Sys.time(), ': Processed ',idxSetAgr,' out of ',numOut))
      }
      dataIdx <- measIdx[timeBgnIdx >= timeAgrBgnIdx[idxAgr] & timeEndIdx <= timeAgrEndIdx[idxAgr],,drop=FALSE]
      measAgr[idxSetAgr,] <- colMeans(dataIdx,na.rm=na.rm)
    }
    
    
  }  

  # For the windows that spanned the data breaks, compute them with the full dataset
  setAgr <- which(!compAgrAll)
  lenSetAgr <- length(setAgr)
  for(idxAgr in setAgr){
    compAgrAll[idxAgr] <- TRUE
    message(paste0('Processing missed window ',idxAgr,' out of ',lenSetAgr, ' total missed windows'))

    dataIdx <- meas[timeBgn >= timeAgrBgn[idxAgr] & timeEnd <= timeAgrEnd[idxAgr],,drop=FALSE]
    measAgr[idxAgr,] <- colMeans(dataIdx,na.rm=na.rm)
  }
  
  return(measAgr)
  
}
  