source("functions/calc.MO.length.R")
# Exercise the actual assignment without loading site files or running downloads.
expr <- parse("workflows/Validation/flow.SE-Sto.data.format.conc.diffs.R")
find_assignment <- function(x) {
  if (!is.call(x)) return(NULL)
  if (identical(x[[1]],as.name("<-")) && identical(x[[2]],quote(var$L_obukhov))) return(x)
  for (child in as.list(x)[-1]) { if (missing(child)) next; result <- find_assignment(child); if (!is.null(result)) return(result) }
  NULL
}
assignment <- NULL
for (x in expr) { assignment <- find_assignment(x); if (!is.null(assignment)) break }
stopifnot(!is.null(assignment))
Tair_C <- c(10,20)
var <- data.frame(P_kPa=c(967,1000), H_turb_interp=c(50,-20), LE_turb_interp=c(30,10), ustar_interp=c(.3,.2))
expected <- calc.MO.length(c(96.7,100),Tair_C,var$H_turb_interp,var$LE_turb_interp,var$ustar_interp)$L
eval(assignment)
stopifnot(isTRUE(all.equal(var$L_obukhov,expected)), identical(var$P_kPa,c(967,1000)))
message("SE-Sto pressure conversion checks passed.")
