##############################################################################################
#' @title Compute and plot the diel average pattern. 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Calculates the mean or median value as well as an uncertainty estimate 
#' for each time point of the day and optionally plot the output. 
#' 

#' @param time A POSIX vector of time values
#' @param data A data frame consisting of the data for computing the diel pattern. Must be same row-dimension as time vector.
#' @param Int A difftime object of the binning interval. Default is as.difftime(30,units='mins'))
#' @param Stat Character value. The statistic to compute, 'mean' or 'median' (median). Default is 'mean'
#' @param Ucrt Character value. The uncertainty statistic to compute, 'sd' (standard deviation), 'var' (variance), 'mad' (median absolute deviation). Default is 'sd'
#' @param NumSampMin Integer value. The minimum sample size for each diel bin required in order to 
#' output the average/median at that time value.
#' @param Plot Optional. Logical value. If TRUE, plot the diel pattern. If FALSE (default), no plotting.
#' @param TitlPlot Optional. Character string of the plot title. 

#' @return None

#' @references None

#' @keywords diurnal

#' @examples None

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2024-03-14)
#     original creation
##############################################################################################

calculate.diel.ptrn <- function (time,
                           data,
                           Int=as.difftime(30,units='mins'),
                           Stat=c('mean','median')[1],
                           Ucrt=c('sd','var','mad','se')[1],
                           NumSampMin=1,
                           Plot=FALSE,
                           TitlPlot='Diel Pattern'
){
  # Error checking
  stopifnot("POSIXt" %in% class(time))
  stopifnot(all(unlist(lapply(data,base::is.numeric))))
  stopifnot(class(Int)=='difftime')
  stopifnot(Stat %in% c('mean','median'))
  stopifnot(base::is.numeric(NumSampMin) &&
            NumSampMin > 0 && 
            NumSampMin-base::floor(NumSampMin) == 0) 
  stopifnot(base::is.logical(Plot))
  
  
  # Create the diel time sequence in seconds
  IntSec <- base::as.numeric(Int,units='secs')
  secDiel <- seq.int(from=0,to=86400,by=IntSec)
  if(utils::tail(secDiel,1) != 86400){
    stop('One day must be equally divisible by Int')
  }
  numBin <- base::length(secDiel)-1
  
 
  # Get which diel bin each data value is
  timeVec <- as.POSIXlt(time)
  timeSec <- timeVec$sec+timeVec$min*60+timeVec$hour*60*60
  binDiel <- .bincode(timeSec,breaks=secDiel,right=TRUE)
  
  # 
  # Diel time values for plotting
  timeDiel <- as.difftime(secDiel,units='secs')
  units(timeDiel) <- 'hours'

  # Go through each diel time value, compute the mean/median over the dataset
  dielOut <- list()
  for(idxVar in seq_len(ncol(data))){
    dataDiel <- 
      base::lapply(seq_len(numBin),FUN=function(idxDiel){
        setData <- binDiel==idxDiel
        
        # Check whether we have enough data
        numEnuf <- sum(!is.na(data[setData,idxVar])) >= NumSampMin
        
        # Compute the statistic
        if(numEnuf == FALSE){
          stat <- as.numeric(NA)
        } else if(Stat == 'mean'){
          stat <- base::mean(data[setData,idxVar],na.rm=TRUE)
        } else if(Stat == 'median') {
          stat <- stats::median(data[setData,idxVar],na.rm=TRUE)
        }
        
        # Compute the uncertainty
        if(numEnuf == FALSE){
          ucrt <- as.numeric(NA)
        } else if(Ucrt == 'sd'){
          ucrt <- stats::sd(data[setData,idxVar],na.rm=TRUE)
        } else if(Ucrt == 'var') {
          ucrt <- stats::var(data[setData,idxVar],na.rm=TRUE)
        } else if(Ucrt == 'mad') {
          ucrt <- stats::mad(data[setData,idxVar],na.rm=TRUE)
        } else if(Ucrt == 'se') {
          sd <- stats::sd(data[setData,idxVar],na.rm=TRUE)
          ucrt <- sd/sqrt(sum(!is.na(data[setData,idxVar])))
        }
        
        rpt <- base::data.frame(time=timeDiel[idxDiel],stat=stat,ucrt=ucrt,n=sum(!is.na(data[setData,idxVar])),stringsAsFactors=FALSE)
  
        return(rpt)      
      })
      dataDiel <- base::do.call(base::rbind,dataDiel)
      attr(dataDiel$stat,'statistic',Stat)
      attr(dataDiel$ucrt,'statistic',Ucrt)
      
      dielOut[[idxVar]] <- dataDiel
  }
  
  nameVar <- names(data)  
  names(dielOut) <- nameVar
  
  # Plotting
  if(Plot == TRUE){
     library(plotly)
    
    for (idxVar in seq_len(ncol(data)))
      if(idxVar == 1){
        fig <- plot_ly(data = dielOut[[idxVar]], x = ~time, y = ~stat, type = 'scatter', mode = 'markers+lines',name=nameVar[idxVar],
                       error_y = ~list(array = ucrt,
                                       color = '#999999'))
      } else {
        fig <- add_trace(fig,data = dielOut[[idxVar]], x = ~time, y = ~stat, type = 'scatter', mode = 'markers+lines',name=nameVar[idxVar],
                         error_y = ~list(array = ucrt,
                                         color = '#999999'))
      }
    fig <- fig %>%
      layout(title = TitlPlot,
             xaxis = list(title = 'Time (hours)',
                          range=c(0,24)), 
             yaxis = list(title = Stat))
    print(fig)
   
  }
  
  return(dielOut)
}
