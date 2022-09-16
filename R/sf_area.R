#' sf_area
#'
#' Extract area values from sf features.
#'
#' @param x sf feature type.
#'
#' @return Dataframe with added column representing area
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

sf_coords <- function(x) {
  suppressWarnings(
    x |>
      dplyr::bind_cols(x |>
                         sf::st_area() |>
                         tibble::as_tibble() |>
                         dplyr::rename(area = value) |>
                         dplyr::mutate(area = as.numeric(area)))
  )

}
