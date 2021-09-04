#' modulo
#'
#' Applies a modulo operation with a specified base resolution
#'
#' @param x Numeric vector to scale.
#' @param res Base resolution to scale x to using a modulo operation.
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


#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-09-03
#' NOTES       : I can't believe I never made this before.
#' TODO        :
