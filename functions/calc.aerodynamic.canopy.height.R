# Estimate Aerodynamic Canopy Height (h) based on Pennypacker and Baldocchi (2015)
# By Camilo Rey_sanchez. May 2024

#Returns:
# zh final= Gap filled series of aerodynamic canopy height (m)
# h_dsk= Smoothed series of aerodynamic canopy height (m, non-gapfilled)


calc.AeroCanopyH <- function(Mdate, ustar, z, L, u, daysAVG, plotYN) {
  k <- 0.4
  d_h <- 0.6 # Default from paper
  z0_h <- 0.1 # Default from paper
  print("Calculating Aerodynamic Canopy Height")
  
  # Define criteria for calculating h
  ind <- which(ustar > 0.35 & ustar < 0.5 & abs(z / L) < 0.1 & u>1)
  # ind <- which(ustar > 0.25 & ustar < 0.35 & abs(z / L)
  
  h <- rep(NA, length(ustar))
  h[ind] <- z / (d_h + z0_h * exp(k * u[ind] / ustar[ind]))
  # Calculate daily mean height using smoothing and despike one last time
  Days <- as.numeric(format(Mdate, "%j"))
  h_despike <- h[ind]# You might need to implement or find a despike function in R. Not implemented for now.
  navg <- daysAVG # number of windows to use for average
  
 if( length(h_despike) < length(navg) ){ # Added this for TEAK????
   cat("Too little data, z_veg will be equal to the average\n")
   zh_final <- rep(mean(h_despike, na.rm=TRUE), length(h))
   h_dsk <- rep(NA, length(ustar)) # Is this ok?

   } else if (length(unique(Days)) > 30) { # higher than 30 days of data
    #navg <- daysAVG # number of windows to use for average
    hsmooth_dsk <- stats::filter(h_despike, rep(1/navg, navg), sides=2) # smoothing
    h_dsk <- rep(NA, length(ustar))
    h_dsk[ind] <- hsmooth_dsk
    # Interpolate daily measurements back to 30 (9) min values
    zh_final <- approx(Mdate[!is.na(h_dsk)], h_dsk[!is.na(h_dsk)], Mdate, method="linear", rule=2)$y
    # Fix the interpolation at the end
    fff <- which(!is.na(h_dsk))
    zh_final[1:fff[1]] <- zh_final[fff[1]]
    zh_final[fff[length(fff)]:length(zh_final)] <- zh_final[fff[length(fff)]]
  } else {
    cat("Too little data, z_veg will be equal to the average\n")
    h_dsk <- rep(NA, length(ustar)) # Is this ok? TOOL
    zh_final <- rep(mean(h_despike, na.rm=TRUE), length(h))
  }
  
  if (plotYN == 1 && length(unique(Days)) > 4) { # higher than 4 days of data
    plot(Mdate, h, pch=19, col="black", cex=1, main=paste("Smoothing based on a", daysAVG, "-day window average"),
         xlab="Date", ylab="Aerodynamic canopy height (m)")
    points(Mdate, h_dsk, pch=19, col="blue", cex=1)
    lines(Mdate, zh_final, col="blue")
    legend("topleft", legend=c("initial", "Smoothed", "Gap filled"), col=c("black", "blue", "blue"), pch=c(19, 19, NA), lty=c(NA, NA, 1))
  }
  
  DATA <- list(h_dsk=h_dsk, zh_final=zh_final)
  return(DATA)
}

