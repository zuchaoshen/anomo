#' Relative efficiency (RE) calculation
#'
#' @description Calculate the relative efficiency (RE) between two designs using
#'    the re function from the R package odr.
#'
#' @param od Returned object of first design (e.g., unconstrained optimal design)
#'     from function \code{\link{od.eq.2group}}.
#' @param subod Returned object of second design (e.g., constrained optimal design)
#'     from function \code{\link{od.eq.2group}}.
#' @param verbose Logical; print the value of relative efficiency if TRUE,
#'    otherwise not; default is TRUE.
#' @param rounded Logical; round the values of \code{p}
#'     to two decimal places if TRUE.
#'     No rounding if FALSE; default is TRUE.
#' @return
#'     Relative efficiency value.
#'
#' @export re
#'
#' @examples
#' # Unconstrained optimal design #----------
#'   myod1 <- od.eq.2group(r12 = 0.5, c1 = 1, c1t = 20)
#' # Constrained optimal design with p = .50
#'   myod2 <- od.eq.2group(r12 = 0.5, c1 = 1, c1t = 20, p = .50)
#' # Relative efficiency (RE)
#'   myre <- re(od = myod1, subod= myod2)
#'   myre$re # RE = 0.71
#'
re <- function(od, subod, rounded = TRUE, verbose = TRUE) {
  out <- odr::re(od = od, subod = subod, rounded = rounded, verbose = verbose)
}
