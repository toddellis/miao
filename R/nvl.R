#' nvl
#'
#' Replaces all no-value options with a specified replacement.
#'
#' @param x Vector to clean of any class.
#' @param value Replacement value to substitute.
#'
#' @return Cleaned vector.
#' @export
#'
#' @examples
#' nvl(c(NA, NaN, Inf, -Inf, NA_character_, NA_real_, NA_complex_, NA_integer_), 0)
#'

nvl <- function(x,
                value = 0) {

  ifelse(x %in% c(NA, NaN, NULL, Inf, -Inf, NA_character_, NA_complex_, NA_integer_, NA_real_),
         value,
         x)

}


#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-09-03
#' NOTES       : Based on Jered's described SQL function -- unsure if his or Oracle's.
#' TODO        :
