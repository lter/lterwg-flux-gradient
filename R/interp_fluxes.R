#' Interpolate fluxes
#'
#' @param timeBgn Vector. POSIXct start time for flux measurement
#' @param timeEnd Vector. POSIXct end time for flux measurement
#' @param flux Required. flux measurement
#' @param timePred Required. Vector. POSIXct time at which to interpolate flux measurement at
#'
#' @return interpolated fluxes
#' 
#' @export
#'
#' @examples
interp_fluxes <- function(timeBgn = NULL,
                          timeEnd = NULL,
                          flux,
                          timePred){
  
  timeMid <- timeBgn + (timeEnd-timeBgn)/2 # midpoint of flux computation window
  
  fluxPred <- approx(x = timeMid, y = flux, xout = timePred, method="linear",
            rule = 1, f = 0.5, ties = mean, na.rm = FALSE)
  
  return(fluxPred$y)
  
}
  