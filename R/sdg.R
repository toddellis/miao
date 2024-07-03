#' sdg
#'
#' Shortcut to remove geometry from sf objects.
#'
#' @param x Vector dataset
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'

sdg <- function(x) {
  sf::st_drop_geometry(x) |>
    tibble::as_tibble()
}
