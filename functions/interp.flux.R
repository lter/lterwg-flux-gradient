##############################################################################################
#' @title Interpolate flux data 

#' @author
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description
#' Linearly interpolate window-average flux data (or any other data expressed as a
#' window-average measurement). Time points for the flux data are assumed to be the 
#' mid-point of the averaging window. 

#' @param timeBgn Required. POSIXct Vector. Start time for flux measurement
#' @param timeEnd Required. POSIXct Vector. End time for flux measurement
#' @param flux Required. Numeric vector. Flux measurement (or any other data expressed as a
#' window-average measurement)
#' @param timePred Required. POSIXct Vector. Time at which to interpolate flux measurement at
#'
#' @return Numeric vector. Interpolated fluxes
#' 
#' @export
#'
#' @examples
#' timeBgn <- as.POSIXct(c('2024-03-10 00:00','2024-03-10 00:30','2024-03-10 01:00'),tz='GMT')
#' timeEnd <- as.POSIXct(c('2024-03-10 00:30','2024-03-10 01:00','2024-03-10 01:30'),tz='GMT')
#' flux <- c(10,20,30)
#' timePred <- as.POSIXct(c('2024-03-10 00:20','2024-03-10 00:40'),tz='GMT')
#' fluxPred <- interp.flux(timeBgn,timeEnd,flux,timePred)

# changelog and author contributions / copyrights
#   Cove Sturtevant (2023-10-20)
#     original creation
##############################################################################################
interp.flux <- function(timeBgn,
                        timeEnd,
                        flux,
                        timePred)
{
  
  timeMid <- timeBgn + (timeEnd-timeBgn)/2 # midpoint of flux computation window
  
  fluxPred <- approx(x = timeMid, y = flux, xout = timePred, method="linear",
            rule = 1, f = 0.5, ties = mean, na.rm = FALSE)
  
  return(fluxPred$y)
  
}
  
