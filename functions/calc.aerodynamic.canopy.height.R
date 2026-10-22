# Estimate aerodynamic canopy height (h) following Pennypacker and Baldocchi (2016)
# By Camilo Rey-Sanchez, May 2024. Revised Oct 2026 (S. Malone):
#   - z is the height at which u is measured (top inlet), not the mean height of the top inlet pair
#   - d/h = 0.66 (default), consistent with z_displ_calc = 0.66 h used for the fluxes
#   - smoothing uses a centred window of daysAVG calendar days (previously daysAVG consecutive valid
#     estimates, which spanned anywhere from hours to weeks)
#
# For a logarithmic wind profile in near-neutral conditions, u/u* = (1/k) ln((z - d)/z0). With d = d_h h
# and z0 = z0_h h this gives h = z / (d_h + z0_h exp(k u / u*)).
#
# Arguments:
#   Mdate   POSIXct times of the samples (uppermost inlet pair)
#   ustar   friction velocity (m s-1)
#   z       height of the wind measurement (m): scalar or vector the length of Mdate
#   L       Obukhov length (m)
#   u       mean horizontal wind speed at height z (m s-1)
#   daysAVG length of the centred smoothing window in days (default 20)
#   plotYN  1 to plot the raw, smoothed and final series
#   d_h, z0_h, k  displacement height and roughness length as fractions of h, von Karman constant
#   min.days minimum number of days with valid estimates inside a window
#
# Returns a list:
#   zh_final  gap-filled aerodynamic canopy height at every Mdate (m)
#   h_dsk     smoothed canopy height at the samples that met the criteria (NA elsewhere)
#   h_raw     un-smoothed estimates at the samples that met the criteria (NA elsewhere)
#   daily     data frame of daily medians (h_day) and their windowed means (h_smooth)

calc.aerodynamic.canopy.height <- function(Mdate, ustar, z, L, u, daysAVG = 20, plotYN = 0,
                                           d_h = 0.66, z0_h = 0.1, k = 0.4, min.days = 3) {
  print("Calculating Aerodynamic Canopy Height")
  n <- length(ustar)
  z <- rep_len(as.numeric(z), n)

  # Near-neutral, well-mixed periods with enough wind (criteria of the original implementation)
  ind <- which(is.finite(ustar) & is.finite(u) & is.finite(L) & is.finite(z) & !is.na(Mdate) &
               ustar > 0.35 & ustar < 0.5 & abs(z / L) < 0.1 & u > 1)

  h_raw <- rep(NA_real_, n)
  h_raw[ind] <- z[ind] / (d_h + z0_h * exp(k * u[ind] / ustar[ind]))
  h_dsk <- rep(NA_real_, n)

  if (length(ind) == 0) {
    cat("No periods met the criteria; z_veg will be NA\n")
    return(list(h_dsk = h_dsk, zh_final = rep(NA_real_, n), h_raw = h_raw, daily = NULL))
  }

  # Daily medians of the valid estimates
  day <- as.Date(Mdate[ind])
  daily <- aggregate(h ~ day, data = data.frame(day = day, h = h_raw[ind]), FUN = median)
  names(daily)[2] <- "h_day"

  if (nrow(daily) < min.days) {
    cat("Too little data, z_veg will be equal to the average\n")
    zh_final <- rep(mean(h_raw[ind]), n)
    h_dsk[ind] <- zh_final[ind]
    daily$h_smooth <- mean(h_raw[ind])
    return(list(h_dsk = h_dsk, zh_final = zh_final, h_raw = h_raw, daily = daily))
  }

  # Centred moving window of daysAVG calendar days (mean of the daily medians inside it)
  half <- daysAVG / 2
  dnum <- as.numeric(daily$day)
  daily$h_smooth <- vapply(dnum, function(d0) {
    w <- abs(dnum - d0) <= half
    if (sum(w) >= min.days) mean(daily$h_day[w]) else NA_real_
  }, numeric(1))
  if (all(is.na(daily$h_smooth))) daily$h_smooth <- mean(daily$h_day)

  ok <- is.finite(daily$h_smooth)
  tday <- as.numeric(as.POSIXct(daily$day[ok])) + 43200   # noon of each day (UTC)
  tall <- as.numeric(Mdate)
  if (sum(ok) >= 2) {
    zh_final <- approx(tday, daily$h_smooth[ok], tall, method = "linear", rule = 2)$y   # constant beyond the ends
  } else {
    zh_final <- rep(daily$h_smooth[ok][1], n)
  }
  h_dsk[ind] <- zh_final[ind]

  if (plotYN == 1) {
    plot(Mdate, h_raw, pch = 19, col = "grey60", cex = 0.4,
         main = paste0("Aerodynamic canopy height (", daysAVG, "-day window)"),
         xlab = "Date", ylab = "Aerodynamic canopy height (m)")
    points(as.POSIXct(daily$day) + 43200, daily$h_day, pch = 19, col = "black", cex = 0.6)
    lines(Mdate[order(Mdate)], zh_final[order(Mdate)], col = "blue", lwd = 2)
    legend("topleft", legend = c("estimates", "daily median", "final"),
           col = c("grey60", "black", "blue"), pch = c(19, 19, NA), lty = c(NA, NA, 1))
  }

  list(h_dsk = h_dsk, zh_final = zh_final, h_raw = h_raw, daily = daily)
}
