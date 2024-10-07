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

sf_dims <- function(x, vars = c("all", "basic", "lines")) {

  .df <-
    x

  if (vars %in% c("basic", "all")) {

    .df$AREA <-
      as.numeric(sf::st_area(.df))

    .df$PERIMETER <-
      as.numeric(sf::st_perimeter(.df))

    if (vars == "all") {

      .df$COMPACTNESS_RATIO <-
        .df$AREA / .df$PERIMETER

      .df$PERIMETER_AREA_RATIO <-
        .df$PERIMETER / .df$AREA

      .df$CIRCULARITY_RATIO <-
        (4 * pi * .df$AREA) / (.df$PERIMETER^2)

      .df$SHAPE_COMPLEXITY_INDEX <-
        1 - .df$PERIMETER / as.numeric(sf::st_area(sf::st_convex_hull(.df$geom)))

    }
  } else {

    ## TODO: Consider adding sinuosity -- see: https://gis.stackexchange.com/questions/334417/calculate-the-sinuoisty-of-a-line
    .df$LENGTH <-
      as.numeric(sf::st_length(.df))

  }

  return(.df)

}
