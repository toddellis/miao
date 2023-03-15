#' mds_ellipse
#'
#' Pulls ellipsis shapes from multidimensional scaling outputs.
#'
#' @param df Dataframe used in the creation of an MDS analysis.
#' @param mds Multidimensional scaling object created through `vegan::metaMDS()`.
#' @param name Character column name representing the grouping variable we want to highlight.
#'
#' @return Dataframe of ellipses line shapes along NMDS1 and NMDS2.
#' @export
#'
#'
#'
mds_ellipse <- function(df, mds, group) {

  .group <- as.factor(df[[group]])

  .ord <- vegan::ordiellipse(ord = mds,
                             draw = "none",
                             groups = .group,
                             display = "sites")

  .output <-
    .group |>
    levels() |>
    tibble::as_tibble() |>
    dplyr::group_by(value) |>
    dplyr::summarise(vegan:::veganCovEllipse(.ord[[value]]$cov,
                                             .ord[[value]]$center,
                                             .ord[[value]]$scale) |>
                       as_tibble(),
                     .groups = 'drop') |>
    dplyr::distinct() |>
    dplyr::rename(!! sym(group) := value) |>
    suppressWarnings()

  return(.output)

}
