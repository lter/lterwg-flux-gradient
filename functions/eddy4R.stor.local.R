## ------------------------------------------------------------------------- ##
# Local eddy4R.stor compatibility functions
## ------------------------------------------------------------------------- ##
# These functions vendor the small exported storage helpers needed from
# eddy4R.stor 0.0.1 so the storage workflow can run outside the NEON Docker
# image. Original package metadata:
#   Package: eddy4R.stor
#   Version: 0.0.1
#   Authors: Ke Xu, David Durden, Stefan Metzger,
#            Natchaya Pingintha-Durden
#   License: GNU Affero General Public License v3
#
# The wrappers below keep the eddy4R.stor function names and arguments where
# possible, but remove dependencies on eddy4R.base, namespace-local helpers,
# and global Para objects that are not available outside the container.

capitalize <- function(x) {
  x <- as.character(x)
  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}


def.idx.diff <- function(PrdWndwAgr, PrdIncrAgr, numDate) {
  n_minutes <- 24 * 60 * numDate
  n_periods <- 24 * 60 / (PrdIncrAgr / 60) * numDate
  half_window_min <- (PrdWndwAgr / 60) / 2

  rpt <- data.frame(
    Bgn = seq(
      from = -half_window_min + 1,
      to = n_minutes + (-half_window_min + 1),
      length.out = n_periods + 1
    ),
    End = seq(
      from = half_window_min,
      to = n_minutes + half_window_min,
      length.out = n_periods + 1
    )
  )

  rpt$Bgn[rpt$Bgn < 1] <- 1
  rpt$End[rpt$End > n_minutes] <- n_minutes
  rpt$Bgn <- as.integer(rpt$Bgn)
  rpt$End <- as.integer(rpt$End)
  rpt
}


def.itpl.time <- function(dataInp, methItpl = "linear", WndwMax) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package `zoo` is required for def.itpl.time().", call. = FALSE)
  }

  Date <- substring(dataInp$timeBgn[1], 1, 10)
  timeOut <- as.POSIXlt(seq.POSIXt(
    from = as.POSIXlt(
      paste(Date, " 00:00:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    to = as.POSIXlt(
      paste(Date, " 23:59:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    by = 60
  ), tz = "UTC")

  timeFracOut <- timeOut$hour + timeOut$min / 60 + timeOut$sec / 3600
  timeInp <- as.POSIXlt(
    dataInp$timeBgn,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  timeInp <- as.POSIXlt(
    timeInp + dataInp$numSamp / 2,
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )

  dataInp$timeFrac <- timeInp$hour + timeInp$min / 60 + timeInp$sec / 3600
  dataInp$DOYFrac <- timeInp$yday + 1 + dataInp$timeFrac / 24

  ok <- !is.na(dataInp$mean) & !is.na(dataInp$timeFrac)
  if (sum(ok) < 2) {
    return(as.numeric(rep(NA, length(timeOut))))
  }

  interp_df <- data.frame(
    x = as.integer(dataInp$timeFrac[ok] * 60),
    mean = as.numeric(dataInp$mean[ok])
  )
  interp_df <- stats::aggregate(mean ~ x, data = interp_df, FUN = mean)

  if (nrow(interp_df) < 2) {
    return(as.numeric(rep(NA, length(timeOut))))
  }

  if (methItpl != "linear") {
    stop("Only methItpl = 'linear' is implemented for def.itpl.time().", call. = FALSE)
  }

  zoo::na.approx(
    object = interp_df$mean,
    x = interp_df$x,
    xout = as.integer(timeFracOut * 60),
    method = "linear",
    maxgap = WndwMax / 60,
    na.rm = FALSE,
    rule = 1,
    f = 0
  )
}


def.time.rate.diff <- function(dataInp, numDate, PrdWndwAgr, PrdIncrAgr,
                               Date, qfqmFlag = FALSE, idxVar = "stor") {
  setData <- def.idx.diff(
    PrdWndwAgr = PrdWndwAgr,
    PrdIncrAgr = PrdIncrAgr,
    numDate = numDate
  )
  rpt <- list()
  rate_name <- paste0("rate", capitalize(idxVar))
  timeOut <- as.POSIXlt(seq.POSIXt(
    from = as.POSIXlt(
      paste(Date, " 00:00:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    to = as.POSIXlt(
      paste(Date, " 23:59:00", sep = ""),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    by = 60
  ), tz = "UTC")

  for (idxAgr in seq_len(length(setData$Bgn) - 1)) {
    rpt[[idxAgr]] <- list()
    bgn_next <- setData$Bgn[idxAgr + 1]
    end_next <- setData$End[idxAgr + 1]
    bgn_now <- setData$Bgn[idxAgr]
    end_now <- setData$End[idxAgr]

    if (qfqmFlag) {
      rpt[[idxAgr]]$qfFinl[[rate_name]] <- as.integer(
        any(dataInp[bgn_next:end_next] == 1) ||
          any(dataInp[bgn_now:end_now] == 1)
      )
    } else {
      rpt[[idxAgr]]$mean[[rate_name]] <- (
        mean(dataInp[bgn_next:end_next], na.rm = TRUE) -
          mean(dataInp[bgn_now:end_now], na.rm = TRUE)
      ) / PrdIncrAgr
    }

    rpt[[idxAgr]]$timeBgn <- list()
    rpt[[idxAgr]]$timeEnd <- list()
    rpt[[idxAgr]]$timeBgn[[rate_name]] <- format(
      timeOut[setData$End[idxAgr] - 1],
      format = "%Y-%m-%d %H:%M:%S"
    )
    rpt[[idxAgr]]$timeEnd[[rate_name]] <-
      timeOut[(setData$End[idxAgr] - 1) + (PrdIncrAgr / 60 - 1)] + 59
  }

  rpt
}


def.itpl.spce <- function(dataInp, methItpl = "linear", resoSpceOut, lvlTowr) {
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Package `zoo` is required for def.itpl.spce().", call. = FALSE)
  }

  lvlTowr <- as.numeric(lvlTowr)
  spceStad <- seq_len(max(lvlTowr) / resoSpceOut) * resoSpceOut
  ok <- !is.na(dataInp) & !is.na(lvlTowr)
  setLgth <- sum(ok)

  if (setLgth >= 2) {
    data_ok <- as.vector(dataInp)[ok]
    lvl_ok <- lvlTowr[ok]

    if (methItpl == "linear") {
      return(zoo::na.approx(
        object = data_ok,
        x = lvl_ok,
        xout = spceStad,
        method = "linear",
        na.rm = TRUE,
        rule = 2
      ))
    }

    if (methItpl == "constant") {
      return(rev(zoo::na.approx(
        object = rev(data_ok),
        x = rev(lvl_ok),
        xout = spceStad,
        method = methItpl,
        na.rm = TRUE,
        rule = 2
      )))
    }

    stop("methItpl must be 'linear' or 'constant'.", call. = FALSE)
  }

  if (setLgth == 1) {
    return(rep(as.vector(dataInp)[ok], length(spceStad)))
  }

  rep(NaN, length(spceStad))
}


def.flux.stor <- function(dataInp, lvlTowr) {
  rpt <- list(
    timeBgn = dataInp$timeBgn,
    timeEnd = dataInp$timeEnd
  )

  dataInp$timeBgn <- NULL
  dataInp$timeEnd <- NULL
  data_mat <- as.matrix(dataInp)
  storage_height <- max(as.numeric(lvlTowr), na.rm = TRUE)
  rpt$mean <- rowMeans(data_mat, na.rm = TRUE) * storage_height
  rpt
}
