#' fwi_dffmc
#'
#' Quick conversion of the Canadian FWI System's Fine Fuel Moisture Code (FFMC) to dead fine fuel moisture content (%). Because I'm sick of looking this up again and again.
#'
#' @param x Fine Fuel Moisture Content value.
#'
#' @return Dead fine fuel moisture content (%: 0 - 250%).
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   dffmc(c(84, 92))
#'
#' }

dffmc <- function(x) {
  147.27 * ((101 - x) / (59.5 + x))
}
