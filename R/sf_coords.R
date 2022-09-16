#' sf_coords
#'
#' Extract x-y coordinate values from sf features.
#'
#' @param x sf feature type.
#'
#' @return Dataframe with added columns representing centroid x-y coordinates.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

sf_coords <- function(x) {
  suppressWarnings(
    output <-
      x |>
      dplyr::bind_cols(x |>
                         sf::st_point_on_surface() |>
                         sf::st_coordinates()) |>
      tibble::as_tibble()
  )

  tryCatch({
    output |>
      dplyr::rename(x = X, y = Y)
  },
  error = function(cond) {
    output
  })
}
