#' Compute Monte Carlo Confidence Intervals
#'
#' @description Compute Monte Carlo confidence intervals (MCCIs) for the
#'      difference and equivalence tests.
#' @param d The estimated effect(s), it has a length of one, two, and four.
#'       (1) When the length is one, it is an estimated main or moderation effect,
#'       the MCCI compute the CI for this estimate; (2) When the length is two,
#'       they represent two estimated effects. These two estimated effects are
#'       main or moderation effects when \code{mediation}
#'       is FALSE, the MCCI compute the CI for the difference of the two estimates;
#'       These two estimated effects are the treatment-mediator and mediator-outcome
#'       path estimates for a mediation effect when \code{mediation} is TRUE;
#'       the MCCI compute the CI for the mediation effects. (3) When the length
#'       is four, they represent the mediation effects in two studies in the
#'       following order: the treatment-mediator and mediator-outcome
#'       path estimates in studies (groups) 1 and 2.
#' @param se The corresponding standard error(s) for parameter \code{d}.
#' @param mediation Logical; \code{d} and \code{se} represent
#'      parameters for a mediation effect if TRUE; \code{d} and \code{se}
#'      represent parameters for a main or moderation effect if FALSE;
#'      default value is FALSE.
#' @param verbose Logical; print the process if TRUE,
#'     otherwise not; default value is TRUE.
#' @param n.mcci The number of draws for the MCCI method. Default is 10,000.
#' @param sig.level The significance level. Default is .05.
#' @param sig.adjusted Logical; use Bonferroni correction (i.e., dividing
#'     the original significance level by the number of tests) if TRUE,
#'     otherwise not; default value is TRUE.
#' @param eq.bd The limit of the equivalence bounds for an equivalence test.
#'     Default is the MCCI for the equivalence test.
#'     It can be specified in the arguments as eq.bd = a positive number or
#'     eq.bd = c(lower bound #, upper bound #).
#' @param dashed.lines Logical of whether dashed lines of equivalence
#'     bounds and zero should be added in the plot. Default is TRUE.
#' @param two.tailed Logical of two tailed test for difference test. Default is TRUE.
#' @param seed Random seed for replication, default is 123.
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
#' # 1. Compute MCCIs for main or moderation effects-----
#'    # 1.1. Compute MCCIs for one main or moderation effect from one study
#'    myci <- mcci(d = .1, se = .02); myci$out
#'    # 1.2 Compute MCCIs for differences in two main (or moderation) effects
#'    myci <- mcci(d = c(0.1, 0.15), se = c(.02, 0.01)); myci$out
#'    # 1.3 Compute MCCIs for differences across five main (or moderation) effects
#'    myci <- mcci(d = c(0.10, 0.15, 0.20, 0.25, 0.30),
#'                 se = c(0.01, 0.01, 0.02, 0.02, 0.03))
#'    myci$out
#'
#' # 2. Compute MCCIs for mediation effects
#'    # 2.1. Compute MCCIs for an estimated mediation effect
#'    myci <- mcci(d = c(.1, 0.15), se = c(.02, 0.01), mediation = TRUE)
#'    myci$out
#'    # 2.1. Compute MCCIs for differences in two mediation effects
#'    myci <- mcci(d = c(0.30, 0.50, 0.33, 0.55),
#'                 se = c(0.02, 0.01, 0.02, 0.03), mediation = TRUE)
#'                 myci$out
#' # 3. Explicitly specify other parameters
#'    myci <- mcci(d = .05, se = .02, eq.bd = 0.1) # equivalence bounds
#'    myci <- mcci(d = .05, se = .02, xlim = c(-0.15, 0.15)) # Range of x-axis

mcci <- function(d = NULL, se = NULL,
                 mediation = FALSE,
                 n.mcci = 10000, sig.level = .05, sig.adjusted = TRUE,
                 two.tailed = TRUE, seed = 123,
                 eq.bd = NULL,
                 xlim = NULL,
                 xlab = NULL,
                 ylab = NULL,
                 dashed.lines = TRUE,
                 verbose = TRUE) {
  funName <- "mcci"
  par <- list(d = d, se = se,
              n.mcci = n.mcci, sig.level = sig.level,
              sig.adjusted = sig.adjusted,
              two.tailed = two.tailed, eq.bd = eq.bd,
              xlim = xlim, xlab = xlab, ylab = ylab,
              seed = seed,
              verbose = verbose)
  set.seed(seed)
  NumberCheck <- function(x) {!is.null(x) && !is.numeric(x)}
  if (sum(sapply(list(d, se),
                   function(x) is.null(x))) >= 1)
      stop("All of 'd', 'se' must be specified")
    if (sum(sapply(list(d, se), function(x) {
      NumberCheck(x)
    })) >= 1)
      stop("'d', 'se' must be numeric")
  if(length(d)!=length(se)) stop("'d', 'se' must be in the same length")
  if(((length(d)==1)|(length(d)==5)|(length(d)==5)) & (mediation==TRUE))
  stop("'mediation' must be FALSE when the length of d is an odd number")
if (length(d)==1){
    d.mc <- stats::rnorm(n.mcci, d, se)
    d.mc <- cbind(d.mc)
} else if (length(d)==2){
  if(mediation){
    d1.mc <- stats::rnorm(n.mcci, d[1], se[1])
    d2.mc <- stats::rnorm(n.mcci, d[2], se[2])
    ab.mc <- d1.mc*d2.mc
    d.mc <- cbind(ab.mc)
  }else{
    d1.mc <- stats::rnorm(n.mcci, d[1], se[1])
    d2.mc <- stats::rnorm(n.mcci, d[2], se[2])
    d.12.mc <- d1.mc-d2.mc
    d.mc <- cbind(d.12.mc)
  }
} else if (length(d)==3){
    if(sig.adjusted){sig.level <- round(sig.level/3, 3)}
    d1.mc <- stats::rnorm(n.mcci, d[1], se[1])
    d2.mc <- stats::rnorm(n.mcci, d[2], se[2])
    d3.mc <- stats::rnorm(n.mcci, d[3], se[3])
    d.12.mc <- d1.mc - d2.mc
    d.13.mc <- d1.mc - d3.mc
    d.23.mc <- d2.mc - d3.mc
    d.mc <- cbind(d.12.mc, d.13.mc, d.23.mc)
}else if (length(d)==4){
  if(mediation){
    a1.mc <- stats::rnorm(n.mcci, d[1], se[1])
    b1.mc <- stats::rnorm(n.mcci, d[2], se[2])
    a2.mc <- stats::rnorm(n.mcci, d[3], se[3])
    b2.mc <- stats::rnorm(n.mcci, d[4], se[4])
    ab.12.mc <- a1.mc*b1.mc-a2.mc*b2.mc
    d.mc <- cbind(ab.12.mc)
  }else{
    if(sig.adjusted){sig.level <- round(sig.level/6, 3)}
    d1.mc <- stats::rnorm(n.mcci, d[1], se[1])
    d2.mc <- stats::rnorm(n.mcci, d[2], se[2])
    d3.mc <- stats::rnorm(n.mcci, d[3], se[3])
    d4.mc <- stats::rnorm(n.mcci, d[4], se[4])
    d.12.mc <- d1.mc - d2.mc
    d.13.mc <- d1.mc - d3.mc
    d.14.mc <- d1.mc - d4.mc
    d.23.mc <- d2.mc - d3.mc
    d.24.mc <- d2.mc - d4.mc
    d.34.mc <- d3.mc - d4.mc
    d.mc <- cbind(d.12.mc, d.13.mc, d.14.mc, d.23.mc, d.24.mc, d.34.mc)
  }
} else if (length(d)==5){
  if(sig.adjusted){sig.level <- round(sig.level/10, 3)}
  d1.mc <- stats::rnorm(n.mcci, d[1], se[1])
  d2.mc <- stats::rnorm(n.mcci, d[2], se[2])
  d3.mc <- stats::rnorm(n.mcci, d[3], se[3])
  d4.mc <- stats::rnorm(n.mcci, d[4], se[4])
  d5.mc <- stats::rnorm(n.mcci, d[5], se[5])
  d.12.mc <- d1.mc - d2.mc
  d.13.mc <- d1.mc - d3.mc
  d.14.mc <- d1.mc - d4.mc
  d.15.mc <- d1.mc - d5.mc
  d.23.mc <- d2.mc - d3.mc
  d.24.mc <- d2.mc - d4.mc
  d.25.mc <- d2.mc - d5.mc
  d.34.mc <- d3.mc - d4.mc
  d.35.mc <- d3.mc - d5.mc
  d.45.mc <- d4.mc - d5.mc
  d.mc <- cbind(d.12.mc, d.13.mc, d.14.mc, d.15.mc, d.23.mc, d.24.mc, d.25.mc,
                d.34.mc, d.35.mc, d.45.mc)
}
  if(two.tailed) {tside = sig.level/2}
  if(length(d.mc)/n.mcci==1){
    d.mean <- mean(d.mc)
    CI.dif <- stats::quantile(d.mc, c(tside, 1 - tside))
    CI.eq <- stats::quantile(d.mc, c(sig.level, 1 - sig.level))
    df <- rbind(d.mean, CI.dif[1], CI.dif[2], CI.eq[1], CI.eq[2])
    if(mediation){
      colnames(df) <- ifelse(length(d)==2, "ab.mc", "ab.12.mc")
      rownames(df) <- c("ab.mean",names(CI.dif),names(CI.eq))
    } else {
      colnames(df) <- ifelse(length(d)==1, "d.mc", "d.12.mc")
      rownames(df) <- c("d.mean",names(CI.dif), names(CI.eq))
    }
  } else {
    d.mean <- colMeans(d.mc)
    CI.dif <- apply(d.mc, 2, function(x) stats::quantile(x, c(tside, 1 - tside)))
    CI.eq <- apply(d.mc, 2, function(x) stats::quantile(x, c(sig.level, 1 - sig.level)))
    df <- rbind(d.mean, CI.dif, CI.eq)
  }

  if (is.null(eq.bd)){
    bound.eq <- c(min(CI.eq), max(CI.eq))
  } else {
    if(length(eq.bd)==1){
      bound.eq <- c(-eq.bd, eq.bd)
    } else if (length(eq.bd)==2) {
      bound.eq <- eq.bd
    }}

  if(is.null(xlim)){xlim = c(min(CI.dif, bound.eq), max(CI.dif, bound.eq))}
  if(is.null(xlab)){
    if(length(d)==1 | ((length(d)==2)& isTRUE(mediation))){
      xlab = "Estimated Effect"
    }else{
      xlab = "Difference in Estimates"
    }}
  if(is.null(ylab)){ylab = ""}

  figure <- graphics::par(mfrow = c(1, 1))
  graphics::plot(x = df[1,], y = 1:length(df[1,]),
                 xlim = xlim, ylim = c(0.5, length(df[1,])+ 0.5),
                 xlab = xlab, ylab = ylab,
                 yaxt = "n",
                 pch = 15)
  graphics::segments(df[2,], 1:length(df[1,]),
                     df[3,], 1:length(df[1,]),
                     col = "black", lwd = 1)
  graphics::segments(df[4,], 1:length(df[1,]),
                     df[5,], 1:length(df[1,]),
                     col = "black", lwd = 3)
  if (dashed.lines){
      graphics::abline(v = bound.eq[1], lty = 3)
      graphics::abline(v = bound.eq[2], lty = 3)
      graphics::abline(v = 0, lty = 5)
  }
    if (verbose) {
      if(length(d.mc)/n.mcci==1){
        print(paste("The ", (1-sig.level)*100,
                    "% MCCI for difference test is (",
                    round(df[2,], 3), ", ",
                    round(df[3,], 3), ")", sep = ""))
        print(paste("The ", (1-2*sig.level)*100,
                    "% MCCI for equivalence test is (",
                    round(df[4,], 3), ", ",
                    round(df[5,], 3), ")", sep = ""))
      } else {
        print(paste("See function results for ", (1-sig.level)*100,
            "% MCCIs for difference tests and ",
            (1-2*sig.level)*100, "% MCCIs for equivalence tests",
            sep = ""))
      }
    }
  results <- list(par = par, out = df, plot = figure)
  return(results)
  }
