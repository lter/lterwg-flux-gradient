#' plot.ustar.NEE.site
#'
#' @param NEE dataframe of nighttime NEE and ustar values
#'
#' @return plot of NEE vs ustar with threshold marked in red
#' 
#'
#' @author Alexis Helgeson
plot.ustar.NEE.site <- function(NEE){
  ustar.0NEE <- NEE[which(NEE$FC_nee_interp >= -1 & NEE$FC_nee_interp <= 1), "ustar_interp"]
  # Fitting a nonlinear quadratic curve using nls
  fit <- nls(y ~ a * x^2 + b * x + c, start = list(a = 1, b = 1, c = 1), data = list(y = NEE$FC_nee_interp, x = NEE$ustar_interp))
  #grab model coefficients
  model.coeff <- coefficients(fit)
  a <- model.coeff[["a"]]
  b <- model.coeff[["b"]]
  c <- model.coeff[["c"]]
  #remove NAs before plotting
  NEE <- NEE[complete.cases(NEE[,c("ustar_interp", "FC_nee_interp")]),]
  #plot light response curve
  ggplot(NEE, aes(x = ustar_interp, y = FC_nee_interp)) +
    xlim(0, max(NEE$ustar_interp))+
    ylim(0, max(NEE$FC_nee_interp))+
    geom_point(size = 3) +
    geom_function(fun = function(x) a * x^2 + b * x + c, colour = "blue", size = 2)+
    geom_vline(xintercept = median(ustar.0NEE), colour = "red", size = 2)+
    geom_text(x = 0.2, y = max(NEE$FC_nee_interp), label = paste("Ustar threshold \n", round(median(ustar.0NEE), 3)), size = 8) +
    xlab(expression(paste("Nighttime U star (m/s)")))+
    ylab(expression(paste("Nighttime NEE Flux (umol CO"[2], " m"^-2," s"^-1,")")))+
    theme_minimal()+
    theme(text = element_text(size = 20), axis.title=element_text(size=24))
}