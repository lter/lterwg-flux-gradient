## --------------------------------------------- ##
#     Recreate Evaluation RDATA files from EDI CSVs
## --------------------------------------------- ##
# Purpose:
# Rebuild SITE_Evaluation.RDATA files from the three EDI gradient-flux CSVs
# written by workflows/flow.evaluation.dataframe_EDI.R.
#
# Expected input files, inside each site folder:
#   SITE_MBR_9min.df.final.csv
#   SITE_AE_9min.df.final.csv
#   SITE_WP_9min.df.final.csv
#
# Output:
#   SITE_Evaluation.RDATA
#
# By default, outputs are saved back into each EDI site folder. To write to a
# different location, change output.data below.

library(tools)

edi.data <- "/Volumes/MaloneLab/Research/FluxGradient/NEON_GradientFlux_EDI"
output.data <- edi.data
overwrite <- TRUE
strict <- FALSE

datetime.cols <- c(
  "match_time.x", "timeEndA", "timeEndB", "match_time.y", "match_time",
  "timeEnd_A", "timeBgn_A", "timeEnd_B", "timeBgn_B", "timeMid",
  "timeEndA.local"
)

method.files <- c(
  MBR = "_MBR_9min.df.final.csv",
  AE = "_AE_9min.df.final.csv",
  WP = "_WP_9min.df.final.csv"
)

read_edi_csv <- function(path) {
  df <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # flow.evaluation.dataframe_EDI.R uses write.csv() with the default
  # row.names = TRUE, which creates an unnamed row-index column.
  first_col <- names(df)[1]
  if (first_col == "" || first_col == "X" || first_col == "...1") {
    df[[1]] <- NULL
  }

  cols.to.convert <- intersect(datetime.cols, names(df))
  for (col in cols.to.convert) {
    df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
  }

  df
}

site_dirs <- list.dirs(edi.data, recursive = FALSE, full.names = TRUE)
site_dirs <- site_dirs[file.info(site_dirs)$isdir]
site_ids <- basename(site_dirs)

if (length(site_ids) == 0) {
  stop("No site folders found in ", edi.data)
}

recreated <- character()
skipped <- list()

for (site in site_ids) {
  message("Processing ", site)

  input.files <- file.path(edi.data, site, paste0(site, method.files))
  names(input.files) <- names(method.files)

  missing.methods <- names(input.files)[!file.exists(input.files)]
  if (length(missing.methods) > 0) {
    skipped[[site]] <- missing.methods
    msg <- paste0(
      "Skipping ", site, ": missing ",
      paste(missing.methods, collapse = ", "), " CSV file(s)."
    )
    if (strict) {
      stop(msg)
    }
    message(msg)
    next
  }

  MBR_9min.df.final <- read_edi_csv(input.files[["MBR"]])
  AE_9min.df.final <- read_edi_csv(input.files[["AE"]])
  WP_9min.df.final <- read_edi_csv(input.files[["WP"]])

  output.site.dir <- file.path(output.data, site)
  if (!dir.exists(output.site.dir)) {
    dir.create(output.site.dir, recursive = TRUE)
  }

  output.file <- file.path(output.site.dir, paste0(site, "_Evaluation.RDATA"))
  if (file.exists(output.file) && !overwrite) {
    skipped[[site]] <- "output exists and overwrite is FALSE"
    message("Skipping ", site, ": output exists and overwrite is FALSE.")
    next
  }

  save(
    MBR_9min.df.final,
    AE_9min.df.final,
    WP_9min.df.final,
    file = output.file
  )

  recreated <- c(recreated, output.file)
  message("Wrote ", output.file)
}

message("Recreated ", length(recreated), " Evaluation RDATA file(s).")

if (length(skipped) > 0) {
  skipped.df <- data.frame(
    site = names(skipped),
    reason = vapply(skipped, paste, character(1), collapse = "; "),
    stringsAsFactors = FALSE
  )
  skipped.file <- file.path(output.data, "recreate_Evaluation_from_EDI_skipped_sites.csv")
  write.csv(skipped.df, skipped.file, row.names = FALSE)
  message("Skipped site summary written to ", skipped.file)
}
