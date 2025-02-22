#' Compute Monte Carlo Confidence Intervals
#'
#' @description Compute Monte Carlo confidence intervals (MCCIs) for the
#'     difference and equivalence tests.
#' @param d1 The estimated mean(s) (effect(s)) for group 1 (study 1).
#'      If more than one effect is specified, it assumes that the effects are
#'       components of a mediation effect.
#' @param se1 The estimated standard error for d1.
#'      If more than one standard error is specified, it assumes that they are
#'       estimated components of a mediation effect.
#' @param d2 The estimated mean(s) (effect(s)) for group 2 (study 2).
#'      If more than one effect is specified, it assumes that the effects are
#'       components of a mediation effect.
#' @param se2 The estimated standard error for d2.
#'      If more than one standard error is specified, it assumes that they are
#'       estimated components of a mediation effect.
#' @param verbose Logical; print the process if TRUE,
#'    otherwise not; default value is TRUE.
#' @param n.mcci The number of draws for the MCCI method. Default is 10,000.
#' @param sig.level The significance level. Default is .05.
#' @param eq.bd The limit of the equivalence bounds for an equivalence test.
#'     Default is the MCCI for the equivalence test.
#'     It can be specified in the arguments as eq.bd = a positive number or
#'     eq.bd = c(lower bound #, upper bound #).
#' @param dashed.lines Logical of whether dashed lines of equivalence
#'     bounds and zero should be added in the plot. Default is TRUE.
#' @param two.tailed Logical of two tailed test for difference test. Default is TRUE.
#' @param xlim The limits set for the x-axis in the plot.
#'     Default is the MCCI for the difference test. It can be
#'      specified in the arguments as xlim = c(lower #, higher #).
#' @param xlab The label for the x-axis in the plot.
#'     Default is "Differences in Effects".
#' @param ylab The label for the y-axis in the plot.
#'     Default is NULL.
#' @return
#'     The results of moderation analysis and equivalence tests
#'     using the MCCI method. It will also provide a plot for
#'     the MCCIs.
#'
#' @export mcci
#'
#' @examples
#'    library(anomo)
#'    # compute MCCI from two studies
#'     myci <- mcci(d1 = .1, se1 = .1, d2 = .2, se2 = .1)
#'    # compute MCCI from one study
#'    myci <- mcci(d1 = .1, se1 = .1)
#'
#' # See the package vignettes for more examples, including the MCCI for the
#' # test of significance and equivalence for mediation effects in two studies.
#'

mcci <- function(d1 = NULL, se1 = NULL, d2 = NULL, se2 = NULL,
                 n.mcci = 10000, sig.level = .05,
                 two.tailed = TRUE,
                 eq.bd = NULL,
                 xlim = NULL,
                 xlab = NULL,
                 ylab = NULL,
                 dashed.lines = TRUE,
                 verbose = TRUE) {
  funName <- "mcci"
  par <- list(d1 = d1, se1 = se1,
              d2 = d2, se2 = se2,
              n.mcci = n.mcci, sig.level = sig.level,
              two.tailed = two.tailed, eq.bd = eq.bd,
              xlim = xlim, xlab = xlab, ylab = ylab,
              verbose = verbose)
  if(two.tailed) {tside = sig.level/2}
  NumberCheck <- function(x) {!is.null(x) && !is.numeric(x)}
  if (sum(sapply(list(d1, se1),
                   function(x) is.null(x))) >= 1)
      stop("All of 'd1', 'se1' must be specified")
    if (sum(sapply(list(d1, se1), function(x) {
      NumberCheck(x)
    })) >= 1)
      stop("'d1', 'se1' must be numeric")
  if(length(d1)!=length(se1)) stop("'d1', 'se1' must be in the same length")
  if(length(d2)!=length(se2)) stop("'d2', 'se2' must be in the same length")

if (length(d1)==1){
  if(is.null(d2)){
    d.mc <- stats::rnorm(n.mcci, d1, se1)
  }else{
    d1.mc <- stats::rnorm(n.mcci, d1, se1)
    d2.mc <- stats::rnorm(n.mcci, d2, se2)
    d.mc <- d2.mc - d1.mc
  }
} else if (length(d1)==2){
  if(is.null(d2)){
    d1.1.mc <- stats::rnorm(n.mcci, d1[1], se1[1])
    d1.2.mc <- stats::rnorm(n.mcci, d1[2], se1[2])
    d.mc <- d1.1.mc*d1.2.mc
  }else{
    d1.1.mc <- stats::rnorm(n.mcci, d1[1], se1[1])
    d2.1.mc <- stats::rnorm(n.mcci, d2[1], se2[1])
    d1.2.mc <- stats::rnorm(n.mcci, d1[2], se1[2])
    d2.2.mc <- stats::rnorm(n.mcci, d2[2], se2[2])
    d.mc <- d2.1.mc*d2.2.mc - d1.1.mc*d1.2.mc
  }
} else if (length(d1)==3){
  if(is.null(d2)){
    d1.1.mc <- stats::rnorm(n.mcci, d1[1], se1[1])
    d1.2.mc <- stats::rnorm(n.mcci, d1[2], se1[2])
    d1.3.mc <- stats::rnorm(n.mcci, d1[3], se1[3])
    d.mc <- d1.1.mc*d1.2.mc**d1.3.mc
  }else{
    d1.1.mc <- stats::rnorm(n.mcci, d1[1], se1[1])
    d2.1.mc <- stats::rnorm(n.mcci, d2[1], se2[1])
    d1.2.mc <- stats::rnorm(n.mcci, d1[2], se1[2])
    d2.2.mc <- stats::rnorm(n.mcci, d2[2], se2[2])
    d1.3.mc <- stats::rnorm(n.mcci, d1[3], se1[3])
    d2.3.mc <- stats::rnorm(n.mcci, d2[3], se2[3])
    d.mc <- d2.1.mc*d2.2.mc*d2.3.mc - d1.1.mc*d1.2.mc**d1
  }
}
  d2_d1.mean <- mean(d.mc)
  CI.dif <- stats::quantile(d.mc, c(tside, 1 - tside))
  CI.eq <- stats::quantile(d.mc, c(sig.level, 1 - sig.level))

  if(is.null(xlim)){xlim = CI.dif}
  if(is.null(xlab)){xlab = "Difference in Means/Effects"}
  if(is.null(ylab)){ylab = " "}
  graphics::plot(x = d2_d1.mean, y = d2_d1.mean,
                 xlim = xlim, ylim = xlim, xlab = xlab, ylab = ylab,
                 yaxt = "n",
                 pch = 15)
  graphics::segments(x0 = CI.dif[1], x1 = CI.dif[2] ,
                     y0 = d2_d1.mean,
                     y1 = d2_d1.mean,
                     col = "black")
  graphics::segments(x0 = CI.eq[1], x1 = CI.eq[2] ,
                     y0 = d2_d1.mean,
                     y1 = d2_d1.mean,
                     col = "black", lwd = 3)
  if (dashed.lines){
    if(length(eq.bd)==1){
      bound.eq <- c(-eq.bd, eq.bd)
    }else{
      bound.eq <- eq.bd
    }
    if(is.null(bound.eq)){
      graphics::abline(v = CI.eq[1], lty = 3)
      graphics::abline(v = CI.eq[2], lty = 3)
    } else{
      graphics::abline(v = bound.eq[1], lty = 3)
      graphics::abline(v = bound.eq[2], lty = 3)
    }
    graphics::abline(v = 0, lty = 5)
  }
    if (verbose) {
      print(paste("The ", (1-sig.level)*100, "% MCCI for difference test is", " c(",
                  round(CI.dif[1], 3), "," , round(CI.dif[2], 3), ")."
                  ,  sep = ""))
      print(paste("The ", (1-2*sig.level)*100,  "% MCCI for equivalence test is", " c(",
                  round(CI.eq[1], 3), "," , round(CI.eq[2], 3), ")."
                  ,  sep = ""))
    }

    out = list(dif = d2_d1.mean,
               CI.dif = CI.dif,
               CI.eq = CI.eq)
    results <- list(par=par, out=out)
    return(results)
  }




