#' subsample
#'
#' Combines purrr and dplyr sampling methods to sample x rows from a dataset y times. Useful for means testing.
#'
#' @param x Dataset
#' @param rows_n Number of rows to sample from the dataset
#' @param samples_n Number of times to sample the dataset
#'
#' @return Dataframe
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
subsample <- function(x,
                      rows_n = 100,
                      samples_n = 100,
                      replace = TRUE) {

  purrr::map_dfr(.x = seq_len(samples_n),
                 .f = ~ {
                   .data <-
                     dplyr::slice_sample(x,
                                         n = rows_n,
                                         replace = replace)

                   .data <-
                     dplyr::mutate(.data,
                                   .sample_n = .x)

                   .data <-
                     dplyr::group_by(.data,
                                     .sample_n,
                                     .add = TRUE)

                   return(.data)

                 })

}
