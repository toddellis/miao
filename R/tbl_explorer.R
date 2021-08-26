#' tbl_explorer
#'
#' Counts all unique values in an entire dataset across all columns
#'
#' @param x Dataset
#' @param min Numeric cutoff point for minimum number of observation occurrences. Can be a proportion or an integer value.
#' @param drop_numeric Logical argument to drop numeric columns with a lot of variability
#'
#' @return A dataframe with column and value counts.
#' @export
#'
#' @examples
#' utas_tbl('students.campuses') %>%
#'   tbl_exporer()
#'

tbl_explorer <- function(x,
                         min = 1,
                         drop_numeric = FALSE) {

  if (drop_numeric == TRUE) {
    x <- x %>%
      dplyr::select(!tidyselect:::vars_select_helpers$where(is.numeric))
  }

  output <- x %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                as.character)) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = '.col',
                        values_to = '.val') %>%
    dplyr::mutate(.factor = factor(.col,
                                   levels = unique(.col))) %>%
    dplyr::group_by(.factor) %>%
    dplyr::mutate(.id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::count(.id, .col, .val,
                 name = '.n') %>%
    dplyr::arrange(.id, -.n) %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(.rn = row_number()) %>%
    dplyr::ungroup()

  if (min < 0) {
    min = abs(min)
    warning('A negative minimum was supplied. This has been transformed to an absolute value.')
  }

  if (min %in% c(0, 1)) {
    output
  } else if (min > 1) {
    output %>%
      dplyr::filter(.n >= min)
  } else if (dplyr::between(min, 0, 1)) {
    output %>%
      dplyr::filter(.n >= (min * nrow(x)))
  }

}

#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-08-26
#' NOTES       :
#' TODO        :
