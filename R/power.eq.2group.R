#' Statistical Power Analysis for Equivalence Tests of Two-Group Means
#'
#' @description Statistical power analysis for equivalence test of
#'     two-group means.
#' @param expr Returned object from function
#'     \code{\link{od.eq.2group}}; default value is NULL;
#'     if \code{expr} is specified, parameter values of \code{r12},
#'     \code{c1}, \code{c1t}, and \code{p}
#'     used or solved in function \code{\link{od.eq.2group}} will
#'     be passed to the current function;
#'     only the value of \code{p} that specified or solved in
#'     function \code{\link{od.eq.2group}} can be overwritten
#'     if \code{constraint} is specified.
#' @param cost.model Logical; power analyses accommodating costs and budget
#'     (e.g., required budget for desired power, power, minimum detectable
#'     eq.dis under a fixed budget)
#'     if TRUE. Otherwise, conventional power analysis is performed
#'     (e.g., required sample size, power, or minimum detectable
#'     eq.dis calculation);
#'     default value is FALSE, and it will be changed to TRUE if
#'     expr is not NULL.
#' @param constraint Specify the constrained value of
#'     \code{p} in list format to overwrite that
#'     from \code{expr}; default value is NULL.
#' @param d The estimated difference in two-group means.
#' @param eq.dis A positive number to specify the distance from equivalence
#'     bounds to \code{d}. The equivalence bounds are
#'     c(-abs(d)-eq.dis, abs(d)+eq.dis).
#' @param n The total sample size across groups.
#' @param p The proportion of individuals in the intervention group or group 1.
#' @param m Total budget.
#' @param verbose Logical; print the process if TRUE,
#'    otherwise not; default value is TRUE.
#' @param c1 The cost of sampling one unit in the control condition.
#' @param c1t The cost of sampling one unit in the treated condition.
#' @param sig.level The significance level. Default is .05.
#' @param r12 The proportion of variance explained by covariates if any.
#' @param q The number of predictors in the combined linear regression model.
#'     Default is 1.
#' @param power Statistical power.
#' @param powerlim The range for solving the root of power (power) numerically,
#'     default value is c(1e-10, 1 - 1e-10).
#' @param nlim The range for searching the root of sample size (n) numerically,
#'     default value is c(4, 10e10).
#' @param mlim The range for searching the root of budget (\code{m}) numerically,
#'     default value is the costs sampling \code{nlim}
#'     units across treatment conditions or
#'     c(4 * ncost, 10e10 * ncost) with ncost = ((1 - p) * c1 + p * c1t).
#' @param eq.dislim The range for solving the root of equivalence difference
#'     with the effect size (d) numerically, default value is c(0, 10).
#' @return
#'     Required budget (and/or required sample size), statistical power, or
#'     minimum detectable eq.dis
#'     depending on the specification of parameters.
#'     The function also returns the function name, design type,
#'     and parameters used in the calculation.
#' @export power.eq.2group
#'
#' @examples
#'    library(anomo)
#' # 1. Conventional Power Analyses from Difference Perspectives
#' # Calculate the required sample size to achieve certain level of power
#' mysample <- power.eq.2group(d = .1, eq.dis = 0.1,  p =.5,
#'                             r12 = .5, q = 1, power = .8)
#' mysample$out
#'
#' # Calculate power provided by a sample size allocation
#' mypower <- power.eq.2group(d = 1, eq.dis = .1, n = 1238, p =.5,
#'                            r12 = .5, q = 1)
#' mypower$out
#'
#' # Calculate the minimum detectable distance a given sample size allocation
#' # can achieve
#' myeq.dis <- power.eq.2group(d = .1, n = 1238, p =.5,
#'                            r12 = .5, q = 1, power = .8)
#' myeq.dis$out
#'
#' # 2. Power Analyses Using Optimal Sample Allocation
#' myod <- od.eq.2group(r12 = 0.5, c1 = 1, c1t = 10)
#' budget <- power.eq.2group(expr = myod, d = .1, eq.dis = 0.1,
#'                           q = 1, power = .8)
#' budget.balanced <- power.eq.2group(expr = myod, d = .1, eq.dis = 0.1,
#'                                    q = 1, power = .8,
#'                                    constraint = list(p = .50))
#' (budget.balanced$out$m-budget$out$m)/budget$out$m *100
#' # 27% more budget required from the balanced design with p = 0.50.
#'
power.eq.2group <- function(cost.model = FALSE, expr = NULL, constraint = NULL,
                            d = NULL, eq.dis = NULL, m = NULL, c1 = NULL,
                            c1t = NULL,
                            n = NULL, p = NULL, q = 1, sig.level = .05,
                            r12 = NULL, power = NULL, powerlim = NULL,
                            nlim = NULL, mlim = NULL, eq.dislim = NULL,
                            verbose = TRUE) {
  #  eq.upper = abs(d) + eq.dis
  #  eq.lower = -eq.upper
  funName <- "power.eq.2group"
  designType = "Equivalence Test for Two-Group Means"
  par <- list(cost.model = cost.model, expr = expr, constraint = constraint,
              d = d, n = n, p = p, q = q, eq.dis = eq.dis,
              sig.level = sig.level, r12 = r12, verbose = verbose)
  NumberCheck <- function(x) {!is.null(x) && !is.numeric(x)}
  if (!is.null(expr)) {
    if (expr$funName != "od.eq.2group") {
      stop("'expr' can only be NULL or
            the return from the function of 'od.eq.2group'")
    } else {
      if (sum(sapply(list(r12, c1, c1t, p),
                     function(x) {!is.null(x)})) >= 1)
        stop("parameters of 'r12', 'c1', 'c1t', 'p'
             have been specified in expr of 'od.eq.2group'")
      r12 <- expr$par$r12
      c1 <- expr$par$c1
      c1t <- expr$par$c1t
      p <- expr$out$p
      cost.model <- TRUE
    }
  } else {
    if (sum(sapply(list(p, r12, q),
                   function(x) is.null(x))) >= 1)
      stop("All of 'p', 'r12', and 'q' must be specified when
           cost.model is FALSE")
    if (sum(sapply(list(d,q, r12), function(x) {
      NumberCheck(x)
    })) >= 1)
      stop("'d', 'q', 'r12' must be numeric")
    if (!is.null(constraint))
      stop("'constraint' must be NULL when 'expr' is NULL")
  }
  if (!is.null(constraint$p)) {
    if(NumberCheck(constraint$p) ||
       any (0 >= constraint$p | constraint$p >= 1))
      stop("constrained 'p' must be numeric in (0, 1)")
    p <- constraint$p
  }
  if(cost.model){
    if (sum(sapply(list(m, eq.dis, power), is.null)) != 1)
      stop("exactly one of 'm', 'eq.dis', and 'power' must be NULL
           when cost.model is TRUE")
    if (!is.null(n))
      stop("'n' must be NULL when cost.model is TRUE")
  } else {
    if (sum(sapply(list(n, eq.dis, power), is.null)) != 1)
      stop("exactly one of 'n', 'eq.dis', and 'power' must be NULL
           when cost.model is FALSE \n")
    if (!is.null(m))
      stop("'m' must be NULL when cost.model is FALSE")
  }

  limFun <- function(x, y) {
    if (!is.null(x) && length(x) == 2 && is.numeric(x)) {
      x
    }
    else {
      y
    }
  }
  nlim <- limFun(x = nlim, y = c(4, 10e10))
  powerlim <- limFun(x = powerlim, y = c(1e-10, 1 - 1e-10))
  eq.dislim <- limFun(x = eq.dislim, y = c(0, 5))

  # Compute theoretical power under TOST method
  if(cost.model){
    pwr.expr <- quote({
      n <- m / ((1 - p) * c1 + p * c1t);
      (1-pt(qt(1-sig.level, df=n-q-2), df = n-q-2, ncp = abs(d-(abs(d) + eq.dis))/
              sqrt((1-r12)/(n*p*(1-p)))))*
        (1-pt(qt(1-sig.level, df=n-q-2), df = n-q-2, ncp = (d+abs(d) + eq.dis)/
                sqrt((1-r12)/(n*p*(1-p)))))
    })
      if (is.null(power)) {
        out <- list(power = eval(pwr.expr))
        if(verbose) {
          cat("The statistical power (power) is ", out$power, ".\n", "\n", sep = "")
        }
      } else if (is.null(m)){
        ncost <- ((1 - p) * c1 + p * c1t)
        mlim <- limFun(x = mlim, y = c(nlim[1] * ncost, nlim[2] * ncost))
        out <- list(m = stats::uniroot(function(m) eval(pwr.expr) -
                                         power, mlim)$root)
        out <- c(out, list(n = out$m / (((1 - p) * c1
                                         + p * c1t))))
        if(verbose) {
          cat("The required budget (m) is ", out$m, ".\n", sep = "")
          cat("The required sample size (n) is ",
              out$n, ".\n", "\n", sep = "")
        }
      } else if (is.null(eq.dis)){
        out <- list(eq.dis = stats::uniroot(function(eq.dis) eval(pwr.expr) -
                                              power, eq.dislim)$root)
        if(verbose) {
          cat("The minimum detectable difference between equivalence bounds ",
          "and mean/effect differnce (eq.dis) is ", out$eq.dis,
          ".\n", "\n", sep = "")
        }}
  }else{
    pwr.expr <- quote({
      (1-pt(qt(1-sig.level, df=n-q-2), df = n-q-2, ncp = abs(d-(abs(d) + eq.dis))/
              sqrt((1-r12)/(n*p*(1-p)))))*
        (1-pt(qt(1-sig.level, df=n-q-2), df = n-q-2, ncp = (d+abs(d) + eq.dis)/
                sqrt((1-r12)/(n*p*(1-p)))))
    })
    if (is.null(power)) {
      out <- list(power = eval(pwr.expr))
      if(verbose) {
        cat("The statistical power (power) is ", out$power, ".\n", "\n", sep = "")
      }
    } else if (is.null(n)) {
      out <- list(n = stats::uniroot(function(n) eval(pwr.expr) -
                                       power, nlim)$root)
      if(verbose) {
        cat("The required sample size (n) is ", out$n, ".\n", "\n", sep = "")
      }
    } else if(is.null(eq.dis)){
      out <- list(eq.dis = stats::uniroot(function(eq.dis) eval(pwr.expr) -
                                            power, eq.dislim)$root)
      if(verbose) {
        cat("The minimum detectable difference between equivalence bounds ",
            "and mean/effect differnce (eq.dis) is ",
            out$eq.dis, ".\n", "\n", sep = "")
      }
    }
  }
  power.out <- list(funName = funName, designType = designType,
                    par = par, out = out)
    return(power.out)
  }




