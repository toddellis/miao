#' sf_dims
#'
#' Returns area, length, and perimeter for vector features.
#'
#' @param x sf feature type.
#'
#' @return Dataframe with added columns representing area, length, and perimeter values
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

sf_dims <- function(x) {
  suppressWarnings(
    x |>
      dplyr::bind_cols(x |>
                         sf::st_area() |>
                         tibble::as_tibble() |>
                         dplyr::rename(area = value) |>
                         dplyr::mutate(area = as.numeric(area))) |>
      dplyr::bind_cols(x |>
                         sf::st_length() |>
                         tibble::as_tibble() |>
                         dplyr::rename(length = value) |>
                         dplyr::mutate(length = as.numeric(length))) |>
      dplyr::bind_cols(x |>
                         sf::st_perimeter() |>
                         tibble::as_tibble() |>
                         dplyr::rename(perimeter = value) |>
                         dplyr::mutate(perimeter = as.numeric(perimeter)))
  )

}
