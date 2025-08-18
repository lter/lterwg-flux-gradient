#' Calculate Lin's Concordance Correlation Coefficient
#' Implementation based on the epiR::epi.ccc function:
#' Stevenson M, Sergeant E (2021). epiR: Tools for the Analysis of Epidemiological Data.
#' R package version 2.0.19. https://CRAN.R-project.org/package=epiR
#'
#' This function calculates Lin's Concordance Correlation Coefficient (CCC)
#' which assesses both precision and accuracy of measurements compared to a gold standard.
#' Implementation matches the epiR::epi.ccc function.
#'
#' @param x Numeric vector of measurements
#' @param y Numeric vector of reference values to compare against
#' @param ci Type of confidence interval ("z-transform" or "asymptotic")
#' @param conf.level Confidence level (default: 0.95)
#' @param rep.measure Logical indicating whether measurements are repeated (default: FALSE)
#' @param subjectid Vector of subject identifiers for repeated measures
#'
#' @return A list with components:
#'   \item{rho.c}{A data frame with estimate, lower and upper confidence limits for CCC}
#'   \item{s.shift}{Scale shift (sd1/sd2)}
#'   \item{l.shift}{Location shift}
#'   \item{C.b}{Bias correction factor}
#'   \item{blalt}{Bland-Altman data}
#'   \item{sblalt}{Bland-Altman statistics}
#'   \item{nmissing}{Number of missing observations}
#'
#' @references
#' Lin, L. (1989). A concordance correlation coefficient to evaluate reproducibility.
#' Biometrics, 45, 255-268.
#'
calc.lins.ccc <- function(x, y, ci = "z-transform", conf.level = 0.95, 
                               rep.measure = FALSE, subjectid = NULL) {
  # Calculate the normal quantile for the confidence level
  N. <- 1 - ((1 - conf.level)/2)
  zv <- qnorm(N., mean = 0, sd = 1)
  
  # Create data frame and handle missing values
  dat <- data.frame(x, y)
  id <- complete.cases(dat)
  nmissing <- sum(!complete.cases(dat))
  dat <- dat[id, ]
  
  # Calculate basic statistics
  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1)/k  # Note: biased variance
  sd1 <- sd(dat$y)
  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1)/k  # Note: biased variance
  sd2 <- sd(dat$x)
  
  # Calculate correlation and covariance
  r <- cor(dat$x, dat$y)
  sl <- r * sd1/sd2
  sxy <- r * sqrt(sx2 * sy2)
  
  # Calculate Lin's CCC
  p <- 2 * sxy/(sx2 + sy2 + (yb - xb)^2)
  
  # Calculate differences for Bland-Altman
  delta <- (dat$x - dat$y)
  rmean <- apply(dat, MARGIN = 1, FUN = mean)
  blalt <- data.frame(mean = rmean, delta)
  
  # Calculate scale and location shift
  v <- sd1/sd2
  u <- (yb - xb)/((sx2 * sy2)^0.25)
  C.b <- p/r
  
  # Calculate standard error for CCC
  sep <- sqrt(((1 - ((r)^2)) * (p)^2 * (1 - ((p)^2))/(r)^2 + 
                 (2 * (p)^3 * (1 - p) * (u)^2/r) - 0.5 * (p)^4 * (u)^4/(r)^2)/(k - 2))
  
  # Calculate asymptotic confidence intervals
  ll <- p - (zv * sep)
  ul <- p + (zv * sep)
  
  # Calculate z-transformed confidence intervals
  t <- log((1 + p)/(1 - p))/2
  set <- sep/(1 - ((p)^2))
  llt <- t - (zv * set)
  ult <- t + (zv * set)
  llt <- (exp(2 * llt) - 1)/(exp(2 * llt) + 1)
  ult <- (exp(2 * ult) - 1)/(exp(2 * ult) + 1)
  
  # Handle repeated measures if requested
  if (rep.measure == TRUE && !is.null(subjectid)) {
    dat$sub <- subjectid[id]  # Use only complete cases
    if (!is.factor(dat$sub)) 
      dat$sub <- as.factor(dat$sub)
    nsub <- length(levels(dat$sub))
    model <- aov(delta ~ dat$sub)
    MSB <- anova(model)[[3]][1]
    MSW <- anova(model)[[3]][2]
    pairs <- NULL
    for (i in 1:nsub) {
      pairs[i] <- sum(is.na(delta[dat$sub == levels(dat$sub)[i]]) == FALSE)
    }
    sig.dl <- (MSB - MSW)/((sum(pairs)^2 - sum(pairs^2))/((nsub - 1) * sum(pairs)))
    delta.sd <- sqrt(sig.dl + MSW)
  } else {
    delta.sd <- sqrt(var(delta, na.rm = TRUE))
  }
  
  # Calculate Bland-Altman statistics
  ba.p <- mean(delta)
  ba.l <- ba.p - (zv * delta.sd)
  ba.u <- ba.p + (zv * delta.sd)
  sblalt <- data.frame(est = ba.p, delta.sd = delta.sd, lower = ba.l, upper = ba.u)
  
  # Prepare return value based on CI type
  if (ci == "asymptotic") {
    rho.c <- data.frame(p, ll, ul)
    names(rho.c) <- c("est", "lower", "upper")
  } else {  # Default to z-transform
    rho.c <- data.frame(p, llt, ult)
    names(rho.c) <- c("est", "lower", "upper")
  }
  
  # Create result list
  rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, 
               C.b = C.b, blalt = blalt, sblalt = sblalt, nmissing = nmissing)
  
  return(rval)
}