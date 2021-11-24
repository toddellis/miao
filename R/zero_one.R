#' zero_one
#'
#' Scale a numeric vector to between 0 and 1. N.B. Kind of pointless given `scales::rescale` exists.
#'
#' @param x Vector to transform.
#'
#' @return Numeric values ranging between 0 and 1.
#'
#' @examples
#' \dontrun{
#' tibble::tibble(YEAR = c(2018:2020),
#'                VALUE = c(1,2,3)) %>%
#'  dplyr::mutate(YEAR_SCALED = zero_one(YEAR),
#'                VALUE_SCALED = zero_one(VALUE))
#'
#' }

zero_one <- function(x) {
  
  output <- (x - min(x, na.rm = T)) / 
    (max(x, na.rm = T) - min(x, na.rm=T))
  
  return(output)
  
}