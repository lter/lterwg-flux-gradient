#' compile.neon.data.v2
#'
#' Compile NEON HDF5 tower data one frequency at a time.
#'
#' This is a conservative replacement for compile.neon.data(). It keeps the
#' original return shape, avoids direct low-level HDF5 identifier handling, and
#' avoids rhdf5::h5closeAll() by default because forced global HDF5 cleanup can
#' crash R in some rhdf5/HDF5 builds. The high-level h5read()/h5ls() calls used
#' by the grab.neon.* functions manage their own file handles.
#'
#' @param h5files Character vector of HDF5 file paths.
#' @param sitecode NEON site code.
#' @param frequency Frequency to compile. Valid values are "9min", "1min", and
#'   "30min".
#' @param startdate Optional start date passed through to extractors that accept
#'   it.
#' @param enddate Optional end date passed through to extractors that accept it.
#' @param close_hdf5 Logical. If TRUE, call rhdf5::h5closeAll() after each file.
#'   Defaults to FALSE to avoid hard R crashes from forced global HDF5 cleanup.
#' @param skip_errors Logical. If TRUE, files that throw an extraction error are
#'   reported and skipped. Defaults to FALSE so failures stop the workflow.
#' @param close_on_error Logical. If TRUE, call rhdf5::h5closeAll() only after a
#'   file throws an error. This can clear stale HDF5 handles without forcing
#'   cleanup after every successful read.
#'
#' @return Named list of data frames matching compile.neon.data().
#'
#' @author Sparkle Malone Lab
compile.neon.data.v2 <- function(h5files,
                                 sitecode,
                                 frequency,
                                 startdate = NULL,
                                 enddate = NULL,
                                 close_hdf5 = FALSE,
                                 skip_errors = FALSE,
                                 close_on_error = TRUE) {
  valid.frequencies <- c("9min", "1min", "30min")

  if (missing(frequency) || length(frequency) != 1) {
    stop("Please supply one frequency: \"9min\", \"1min\", or \"30min\".")
  }

  if (!frequency %in% valid.frequencies) {
    stop(
      "Unsupported frequency: ",
      frequency,
      ". Valid values are: ",
      paste(valid.frequencies, collapse = ", ")
    )
  }

  if (length(h5files) == 0) {
    stop("No HDF5 files supplied to compile.neon.data.v2().")
  }

  compiled.data <- list()

  for (i in seq_along(h5files)) {
    hd.file <- h5files[[i]]

    message(
      "Processing ",
      basename(hd.file),
      " (",
      i,
      "/",
      length(h5files),
      ")"
    )

    month.data <- tryCatch(
      compile.neon.data.v2.extract.month(
        hd.file = hd.file,
        sitecode = sitecode,
        frequency = frequency,
        startdate = startdate,
        enddate = enddate
      ),
      error = function(e) {
        error.message <- paste0(
          "Failed while compiling frequency '",
          frequency,
          "' from HDF5 file: ",
          hd.file,
          "\nOriginal error: ",
          conditionMessage(e)
        )

        if (isTRUE(close_on_error)) {
          try(rhdf5::h5closeAll(), silent = TRUE)
        }

        if (isTRUE(skip_errors)) {
          warning(error.message, call. = FALSE)
          return(NULL)
        }

        stop(error.message, call. = FALSE)
      }
    )

    if (!is.null(month.data) && length(month.data) > 0) {
      for (output.name in names(month.data)) {
        compiled.data[[output.name]] <- dplyr::bind_rows(
          compiled.data[[output.name]],
          month.data[[output.name]]
        )
      }
    }

    rm(month.data)

    if (isTRUE(close_hdf5)) {
      rhdf5::h5closeAll()
    }

    invisible(gc())
  }

  compiled.data
}


#' Extract one monthly HDF5 file for one frequency.
#'
#' @return Named list of data frames for one HDF5 file.
compile.neon.data.v2.extract.month <- function(hd.file,
                                               sitecode,
                                               frequency,
                                               startdate = NULL,
                                               enddate = NULL) {
  if (frequency == "9min") {
    return(grab.neon.gas.9min.6min(
      hd.file = hd.file,
      sitecode = sitecode
    ))
  }

  if (frequency == "1min") {
    return(grab.neon.met.1min(
      hd.file = hd.file,
      sitecode = sitecode,
      startdate = startdate,
      enddate = enddate
    ))
  }

  if (frequency == "30min") {
    return(grab.neon.met.flux.30min(
      hd.file = hd.file,
      sitecode = sitecode,
      startdate = startdate,
      enddate = enddate
    ))
  }
}


#' Test HDF5 files one at a time for one frequency.
#'
#' This helper is useful when rhdf5 throws low-level HDF5 close/reference count
#' errors. It returns a data frame showing which monthly file fails.
#'
#' @return Data frame with file, frequency, status, and error columns.
test.neon.data.v2.files <- function(h5files,
                                    sitecode,
                                    frequency,
                                    startdate = NULL,
                                    enddate = NULL) {
  results <- lapply(seq_along(h5files), function(i) {
    hd.file <- h5files[[i]]

    message(
      "Testing ",
      basename(hd.file),
      " (",
      i,
      "/",
      length(h5files),
      ")"
    )

    error.message <- NA_character_
    status <- "ok"

    tryCatch(
      {
        month.data <- compile.neon.data.v2.extract.month(
          hd.file = hd.file,
          sitecode = sitecode,
          frequency = frequency,
          startdate = startdate,
          enddate = enddate
        )
        rm(month.data)
        invisible(gc())
      },
      error = function(e) {
        status <<- "error"
        error.message <<- conditionMessage(e)
        try(rhdf5::h5closeAll(), silent = TRUE)
      }
    )

    data.frame(
      file = hd.file,
      frequency = frequency,
      status = status,
      error = error.message,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(results)
}


