#' Optimal sample allocation calculation for equivalence test of two-group
#' means
#'
#' @description The optimal design of single-level experiments detecting
#'     equivalence of two-group means is to choose the optimal sample
#'     allocation that minimizes the variance of a treatment effect under
#'     a fixed budget,  which is approximately the optimal
#'     sample allocation that maximizes statistical power under a fixed budget.
#'     The optimal design parameter is
#'     the proportion of individuals to be assigned to treatment (\code{p}).
#' @param r12 The proportion of outcome variance explained by covariates.
#' @param c1 The cost of sampling one unit in the control condition.
#' @param c1t The cost of sampling one unit in the treated condition.
#' @param p The proportion of individuals to be assigned to treatment.
#' @param m Total budget, default value is the total costs of sampling 600
#'     individuals across treatment conditions.
#' @param plim The plot range for p, default value is c(0, 1).
#' @param varlim The plot range for variance, default value is c(0, 0.05).
#' @param plots Logical, provide variance plots if TRUE, otherwise
#'     not; default value is TRUE.
#' @param plab The plot label for \code{p},
#'     default value is "Proportion of Units in Treatment: p".
#' @param varlab The plot label for variance,
#'     default value is "Variance".
#' @param vartitle The title of variance plot, default value is NULL.
#' @param verbose Logical; print the value of \code{p} if TRUE,
#'    otherwise not; default value is TRUE.
#' @param varlab The plot label for variance,
#'     default value is "Variance".
#' @param vartitle The title of variance plot, default value is NULL.
#' @return
#'     Unconstrained or constrained optimal sample allocation (\code{p}).
#'     The function also returns function name, design type,
#'     and parameters used in the calculation.
#'
#' @export od.1.eq
#'
#' @examples
#' # Unconstrained optimal design #---------
#'   myod <- od.1.eq(r12 = 0.5, c1 = 1, c1t = 50)
#'   myod$out # output
#'
od.1.eq <- function(p = NULL, r12 = NULL,
                 c1 = NULL, c1t = NULL, m = NULL,
                 plots = TRUE,
                 plim = NULL, varlim = NULL,
                 plab = NULL, varlab = NULL,
                 vartitle = NULL,verbose = TRUE) {
  funName <- "od.1.eq"
  designType <- "individual RCTs"
  if (sum(sapply(list(r12, c1, c1t),
                 function(x) is.null(x))) >= 1)
    stop("All of 'r12', 'c1', 'c1t' must be specified")
  NumberCheck <- function(x) {!is.null(x) && !is.numeric(x)}
  if (sum(sapply(list(r12), function(x) {
    NumberCheck(x) || any(0 > x | x > 1)
  })) >= 1)
    stop("'r12' must be numeric in [0, 1]")
  if (sum(sapply(list(c1, c1t), function(x) {
    NumberCheck(x)})) >= 1)
    stop("'c1', 'c1t' must be numeric")
  if (c1 == 0 && c1t == 0 && is.null(p))
    stop("when c1 and c1t are both zero, p must be constrained,
         please specify a value for p")
  par <- list(r12 = r12, c1 = c1,
              c1t =c1t, p = p)
  if (is.null(p)) {
    p <- sqrt(c1/ c1t) / (1 + sqrt(c1/ c1t))
  } else {
    if (!is.numeric(p) || any(p <=0 | p >= 1))
      stop("constrained 'p' must be numeric in (0, 1)")
      cat("===============================\n",
        "p are constrained, there is no calculation from other parameters",
        ".\n===============================\n", sep = "")
    }
  if (verbose == TRUE) {
    if (!is.null(par$p)) {
      cat("The constrained proportion of units in treatment (p) is ", p, ".\n", "\n", sep = "")
    } else {
      cat("The optimal proportion of units in treatment (p) is ", p, ".\n", "\n" ,sep = "")
    }
  }
  m <- ifelse(!is.null(m), m, 600 * (p * c1t + (1 - p) * c1))
  var.expr <- quote({
    n <- m / ((1 - p) * c1 + p * c1t);
    (1 - r12) / (p * (1 - p) * n)
  })
  Var <- eval(var.expr)
  par <- c(par, list(m = m))
  out <- list(p = p, var = Var)
  od.out <- list(funName = funName, designType = designType,
                 par = par, out = out)
  limFun <- function(x, y) {
    if (!is.null(x) && length(x) == 2 && is.numeric(x)) {x} else {y}
  }
  plim <- limFun(x = plim, y = c(0, 1))
  varlim <- limFun(x = varlim, y = c(0, 0.05))
  labFun <- function(x, y) {
    if (!is.null(x) && length(x) == 1 && is.character(x)) {x} else {y}
  }
  plab <- labFun(x = plab, y = "Proportion Units in Treatment: p")
  varlab <- labFun(x = varlab, y = "Variance")
  vartitle <- labFun(x = vartitle, y = "")
  prange <- seq(plim[1] + 0.05, plim[2] - 0.05, by = 0.01)
  if (plots) {
      plot.y <- NULL
      for (p in prange)
        plot.y <- c(plot.y, eval(var.expr))
      graphics::plot(prange, plot.y,
           type = "l", lty = 1,
           xlim = plim, ylim = varlim,
           xlab = plab, ylab = varlab,
           main = vartitle, col = "black")
      p <- out$p
      graphics::abline(v = p, lty = 2, col = "black")
  }
  return(od.out)
}
