#' group_indices
#'
#' Tweak of `dplyr`'s group_indices allowing for retaining the original order.
#'
#' @param x Vector to create group indices from.
#'
#' @return Numeric indices for input group variable.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#'

group_indices <- function(x) {

  dplyr::group_indices(., factor(x,
                                 levels = unique(x)))

}
