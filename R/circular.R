#' circular
#'
#' Smooth annual data using a circular smoother. (Shorthand for stats::filter because its syntax is very confusing.)
#'
#' @param x Numeric variable.
#' @param n Number of rows to smooth over.
#'
#' @return Smoothed variable.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'

circular <- function(x, n) {

  stats::filter(x,
                filter = rep(1 / n, n),
                circular = TRUE)

}
