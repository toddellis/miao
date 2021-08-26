#' bind_limits
#'
#' Set lower and upper bounds for data. E.g., 0-100% to fit a logistic curve.
#'
#' @param x dbl : numeric timeseries variable
#' @param lwr dbl : lower bounds for transforming x
#' @param upr dbl : upper bounds for transforming x
#' @param inverse lgl : flag for scaling data to fit bounds or transforming it back to the original scale
#'
#' @return A vector of the input transformed to recognize upper and lower bound limits
#' @export
#'
#' @examples
#' tibble::tibble(num = seq(0.1, 0.8, by = 0.05)) %>%
#'   mutate(trans = bind_limits(num, lwr = 0, upr = 0.81)) %>%
#'   mutate(back = bind_limits(trans, lwr = 0, upr = 0.81, inverse = TRUE))
#'

bind_limits <- function(x,
                        lwr = 0,
                        upr = 1,
                        inverse = FALSE) {

  if (inverse) {
    ## Transform bound limits back to original scale
    (upr * exp(x) + lwr) / (exp(x) + 1)
  } else {
    ## Scale x backs on designated upper and lower limits
    log((x - lwr) / (upr - x))
  }
}

#' FUNCTION    : bind_limits
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-08-26 : 2021-07-26
#' DESCRIPTION : Set lower and upper bounds for data. E.g., 0-100% to fit a logistic curve.
#' NOTES       : Based on methods via https://towardsdatascience.com/when-will-the-us-be-vaccinated-1b24890a8c38
#' TODO        :
