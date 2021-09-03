#' modulo
#'
#' Applies a modulo operation with a specified base resolution
#'
#' @param x Numeric vector to scale.
#' @param res Base resolution to scale x to.
#'
#' @return Scaled vector.
#' @export
#'
#' @examples
#' runif(100, 0, 100) %>%
#'  tibble::as_tibble() %>%
#'  dplyr::mutate(mod = modulo(value, 15))
#'

modulo <- function(x,
                   res = 1) {

  x - (x %% res)

}
