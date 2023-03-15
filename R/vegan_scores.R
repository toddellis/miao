#' vegan_scores
#'
#' Adds scores from {vegan} objects to initial dataset.
#'
#' @param df Dataframe used in {vegan} MDS analyses. N.B. Should add other methods like PCA later.
#' @param mds Multidimensional scaling object created using `vegan::metaMDS()`.
#'
#' @return Dataframe with NMDS1 and NMDS2 added as columns.
#' @export
#'
#'

vegan_scores <-
  function(df, mds) {
    dplyr::bind_cols(df,
                     tibble::as_tibble(vegan::scores(mds)$sites))
  }
