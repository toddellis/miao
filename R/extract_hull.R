#' extract_hull
#'
#' Extracts the shape of data from dataframes. Originally crafted around multidimensional scaling, but works well with any dataframe.
#'
#' @param df Dataframe to extract shapes from.
#' @param x Numeric variable.
#' @param y Numeric variable.
#' @param group Grouping variable for comparing shape of groups along x-y axes.
#' @param drop_outliers Logical noting to remove outlier percentiles (< 5% | > 95%) on both x and y variables.
#' @param output Character -- either 'df' or 'sf' -- to return regular dataframe for use with `geom_polygon()`, or sf spatial object for use with `geom_sf()`. Note that spatial output applies a smoothing process to the shapes.
#'
#' @return Dataframe of ellipses line shapes along NMDS1 and NMDS2.
#' @export
#'
#' @examples {
#' data(iris)
#' iris |>
#'   extract_hull(x = "Sepal.Width",
#'                y = "Sepal.Length",
#'                group = "Species",
#'                drop_outliers = TRUE,
#'                output = "sf") |>
#'     ggplot() +
#'     geom_sf(aes(col = Species),
#'             fill = NA) +
#'     theme_meow()
#' }
#'

extract_hull <-
  function(df,
           x,
           y,
           group = NULL,
           drop_outliers = FALSE,
           output = c("df", "sf")) {

    .output <-
      rlang::arg_match(output)

    if (!is.null(group) & is.character(group)) {
      .df <-
        df |>
        dplyr::select(!! sym(x), !! sym(y), !! sym(group)) |>
        stats::na.omit() |>
        dplyr::group_by(!! sym(group))
    } else {
      .df <-
        df |>
        dplyr::select(!! sym(x), !! sym(y)) |>
        stats::na.omit()
    }

    if (drop_outliers) {
      .df <-
        .df |>
        dplyr::filter(dplyr::between(!! sym(x),
                                     quantile(!! sym(x),
                                              0.05),
                                     quantile(!! sym(x),
                                              0.95)),
                      dplyr::between(!! sym(y),
                                     quantile(!! sym(y),
                                              0.05),
                                     quantile(!! sym(y),
                                              0.95))) |>
        dplyr::ungroup()
    }

    if (!is.null(group) & is.character(group)) {
      .df <-
        .df |>
        dplyr::group_by(!! sym(group))
    }

    .df <-
      .df |>
      dplyr::slice(grDevices::chull(!! sym(x), !! sym(y))) |>
      dplyr::ungroup()

    if (.output == "sf") {
      .sf <-
        .df |>
        sf::st_as_sf(coords = c(x,
                                y))
      if (!is.null(group) & is.character(group)) {
        .sf <-
          .sf |>
          dplyr::group_by(!! sym(group))
      }
      .sf <-
        .sf|>
        dplyr::summarise() |>
        sf::st_cast("POLYGON") |>
        sf::st_convex_hull()
      if (!is.null(group) & is.character(group)) {
        .sf <-
          .sf |>
          dplyr::group_by(!! sym(group))
      }
      .sf <-
        .sf |>
        smoothr::smooth(method = "ksmooth") |>
        dplyr::ungroup()

      return(.sf)

    } else {

      return(.df)

    }

  }
