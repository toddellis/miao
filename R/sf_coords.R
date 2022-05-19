#' sf_coords
#'
#' Extract x-y coordinate values from sf features.
#'
#' @param x sf feature type.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

sf_coords <- function(x) {
  x |>
    dplyr::bind_cols(x |>
                       sf::st_point_on_surface() |>
                       sf::st_coordinates()) |>
    dplyr::rename(x = X, y = Y)
}
