#' mds_arrow
#'
#' Pulls arrow details from multidimensional scaling object via `vegan::metaMDS()`. Note that these represent arrows in a typical multidimensional scaling plot *from* x = 0, y = 0.
#'
#' @param mds Multidimensional scaling object created through `vegan::metaMDS()`.
#' @param name Character value to name column of variable names.
#'
#' @return Dataframe of variable names and NMDS1 and NMDS2 values to describe the 2-dimensional impact of these variables from [0,0].
#' @export
#'
#'
#'

mds_arrow <-
  function(mds,
           name = "var") {
    mds$species |>
      tibble::as_tibble(rownames = name) |>
      dplyr::rename_with(.fn = ~ glue::glue("N{.x}"),
                         .cols = c(MDS1, MDS2))
  }
