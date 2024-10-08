#' Compute Monte Carlo Confidence Intervals
#'
#' @description Compute Monte Carlo confidence intervals (MCCI) of the
#'      difference in two effects from summary statistics. The MCCI can be
#'      used to test moderation effects (i.e., whether two effects are
#'      statistically different from each) and the equivalence of effects.
#' @param d1 The estimated treatment effect for group 1.
#' @param se1 The estimated standard error of the treatment effect for group 1.
#' @param d2 The estimated treatment effect for group 2.
#' @param se2 The estimated standard error of the treatment effect for group 2.
#' @param verbose Logical; print the process if TRUE,
#'    otherwise not; default value is TRUE.
#' @param n.mcci The number of draws for the MCCI method. Default is 10,000.
#' @param sig.level The significance level. Default is .05.
#' @param bound.eq The equivalence bounds for equivalence test. Default is the
#'     MCCI for the equivalence test. It can be specified in the arguments as
#'     bound.eq = c(lower bound #, upper bound #).
#' @param dashed.lines Logical of whether dashed lines of equivalence
#'     bounds and zero should be added in the plot. Default is TRUE.
#' @param two.tailed Logical of two tailed test. Default is TRUE.
#' @param xlim The limits set for the x-axis in the plot.
#'     Default is two times the MCCI for moderation. It can be
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
#'     myci <- mcci(d1 = .1, se1 = .1, d2 = .2, se2 = .1)
#'

mcci <- function(d1 = NULL, se1 = NULL, d2 = NULL,
                 se2 = NULL,
                 n.mcci = 10000, sig.level = .05,
                 two.tailed = TRUE,
                 bound.eq = NULL,
                 xlim = NULL,
                 xlab = NULL,
                 ylab = NULL,
                 dashed.lines = TRUE,
                 verbose = TRUE) {
  funName <- "mcci"
  par <- list(d1 = d1, se1 = se1,
              d2 = d2, se2 = se2,
              n.mcci = n.mcci, sig.level = sig.level,
              two.tailed = two.tailed, bound.eq =  bound.eq,
              xlim = xlim, xlab = xlab, ylab = ylab,
              verbose = verbose)
  if(two.tailed) {tside = sig.level/2}
  NumberCheck <- function(x) {!is.null(x) && !is.numeric(x)}
  if (sum(sapply(list(d1, se1, d2, se2),
                   function(x) is.null(x))) >= 1)
      stop("All of 'd1', 'se1', 'd2', 'se2' must be specified")
    if (sum(sapply(list(d1, se1, d2, se2), function(x) {
      NumberCheck(x)
    })) >= 1)
      stop("'d1', 'se1', 'd2', 'se2' must be numeric")

    d1.mc <- stats::rnorm(n.mcci, d1, se1)
    d2.mc <- stats::rnorm(n.mcci, d2, se2)
    d2_d1.mc <- d2.mc - d1.mc
    d2_d1.mean <- mean(d2.mc - d1.mc)
    CI.mo <- stats::quantile(d2_d1.mc,c(tside, 1 - tside))
    CI.eq <- stats::quantile(d2_d1.mc,c(sig.level, 1 - sig.level))

    if(is.null(xlim)){xlim = 2*CI.mo}
    if(is.null(xlab)){xlab = "Difference in Effects"}
    if(is.null(ylab)){ylab = " "}
    graphics::plot(x = d2_d1.mean, y = d2_d1.mean,
         xlim = xlim, xlab = xlab, ylab = ylab,
         yaxt = "n",
         pch = 15)
    graphics::segments(x0 = CI.mo[1], x1 = CI.mo[2] ,
             y0 = d2_d1.mean,
             y1 = d2_d1.mean,
             col = "black")

    graphics::segments(x0 = CI.eq[1], x1 = CI.eq[2] ,
             y0 = d2_d1.mean,
             y1 = d2_d1.mean,
             col = "black", lwd = 3)
    if (dashed.lines){
      if(is.null(bound.eq)){
        graphics::abline(v = CI.eq[1], lty = 3)
        graphics::abline(v = CI.eq[2], lty = 3)
      } else{
        graphics::abline(v = bound.eq[1], lty = 3)
        graphics::abline(v = bound.eq[2], lty = 3)
      }
      graphics::abline(v = 0, lty = 5)
    }


    #https://www.youtube.com/watch?v=x4ekQ1nanQ4&ab_channel=iquit-vids

    if (verbose) {
      print(paste("The ", (1-sig.level)*100, "% MCCI for Moderation is", " c(",
                  round(CI.mo[1], 4), "," , round(CI.mo[2], 4), ")."
                  ,  sep = ""))
      print(paste("The ", (1-2*sig.level)*100,  "% MCCI for Equivalence Test is", " c(",
                  round(CI.eq[1], 4), "," , round(CI.eq[2], 4), ")."
                  ,  sep = ""))
    }

    out = list(CI.mo = CI.mo,
               CI.eq = CI.eq)
    results <- list(par, out)
    return(results)

  }




