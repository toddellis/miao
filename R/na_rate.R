#' na_rate
#'
#' Extend a timeseries dataset and apply an annual growth rate to it
#'
#' @param x Vector with NAs in future time to fill
#'
#' @return Numeric values with a constant rate applied to lagged values.
#' @export
#'
#' @examples
#' \dontrun{
#' tibble::tibble(START_YEAR = c(2018:2020),
#'                      VALUE = c(1,2,3)) %>%
#'  dplyr::bind_rows(tibble::tibble(START_YEAR = c(2021:2024))) %>%
#'  dplyr::mutate(VALUE = na_rate(VALUE, rate = 0.98))
#'
#' }
#'

na_rate <- function(x,
                    rate = 1.1) {

  while(sum(is.na(x)) > 0) {
    x = ifelse(is.na(x),
               lag(x, 1) * rate,
               x)
  }

  return(x)

}
