#' Plot statistical power curves under a fixed budget across optimal design
#' parameters for equivalence testing
#'
#' @description This function plots statistical power curves (for
#'     equivalence testing) under a fixed budget
#'     across optimal design parameters.
#' @inheritParams od.1.eq
#' @inheritParams power.1.eq
#' @param expr Returned objects from an od function (e.g., od.1.eq).
#' @param by Dimensions to plot power curves by the optimal design parameters.
#'     The default value is by all optimal design parameters for a type of design.
#'     For example, default values are by = "p" for single-level designs,
#'     by = c("n", "p") for two-level designs,
#'     and by = c("n", "p", "J") for three-level designs.
#' @param plab Label for the x-axis when the plot is by the optimal design
#'     parameter "p".
#' @param nlab Label for the x-axis when the plot is by the optimal design
#'     parameter "n".
#' @param Jlab Label for the x-axis when the plot is by the optimal design
#'     parameter "J".
#' @param plim The limits of the proportion to the treated (p) for calculating
#'       and plotting power curves.
#' @param nlim The limits of the level-1 sample size (n) for calculating
#'       and plotting power curves.
#' @param Jlim The limits of the level-2 sample size (J) for calculating
#'       and plotting power curves.
#' @param powerlim The power limits for plotting power curves.
#' @param powerlab The label for the statistical power.
#' @param legend Logical; present plot legend if TRUE. The default is TRUE.
#' @param plot.title The title of the plot (e.g., plot.title = "Power Curves").
#'      The default is NULL.
#' @export plot.power.eq
#' @examples
#' # Optimal sample allocation identification
#' od <- od.1.eq(r12 = 0.5, c1 = 1, c1t = 10)
#' # plot the power curve
#' plot.power.eq(expr = od, d = 0.1, eq.dis = 0.1)
#'

plot.power.eq <- function(expr = NULL, nlim = c(2, 300), plim = c(0.01, 0.99),
                       Jlim = c(3, 300),
                       powerlim = c(0, 1), plot.title = NULL,
                       m = NULL, d = NULL, q = 1,
                       power = .80, eq.dis = NULL,
                       by = c("n", "p", "J"), legend = TRUE,
                       nlab = "Level-One Sample Size (n)",
                       plab = "Proportion (p)",
                       Jlab = "Level-Two Sample Size (J)",
                       powerlab = "Statistical Power"){
if (expr$funName == "od.1.eq") {
    p <- expr$out$p
    r12 <- expr$par$r12
    c1 <- expr$par$c1
    c1t <- expr$par$c1t
    if(is.null(m)){m <- power.1.eq(expr = expr, d = d, eq.dis = eq.dis, q = q,
                                   power = power, verbose = F)$out$m}
    prange <- seq(plim[1], plim[2], by = 0.01)
    if (length(by) >= 1) graphics::par(mfrow = c(1, 1))
    if ("p" %in% by) {
      power.eq <- NULL
      for (p in prange){
        power.eq <- c(power.eq, power.1.eq(expr = expr, m = m, q = q,
                                           d = d, eq.dis = eq.dis,
                                            constraint = list(p = p),
                                           verbose = F)$out$power)
      }
      graphics::plot(prange, power.eq,
                     type = "l", lty = 1,
                     xlim = plim, ylim = powerlim,
                     xlab = plab, ylab = powerlab,
                     main = plot.title, col = "black")
      graphics::abline(v = expr$out$p, lty = 2, col = "black")
      figure <- grDevices::recordPlot()
    }
  }
return(plot = figure)
}

