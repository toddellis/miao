#' tbl_summary
#'
#' Gets the number of distinct values in a dataset and total number of observations per column.
#'
#' @param x Dataset.
#' @param .prop Logical determining whether output should transform the count of distinct and total observations for each column into a proportion of the dataset's total size.
#'
#' @return A dataframe with the number of distinct observations and total observations per column.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' tibble::tibble(a = c('A','B', NA),
#'                b = c(1.2, 1.2, 1.2),
#'                c = c(4.5, NA, NA)) %>%
#'  tbl_summary(.prop = TRUE)
#'

tbl_summary <- function(x,
                        .prop = FALSE) {

  .nrow = nrow(x)

  output <- x %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                ~ nvl(.x,
                                      value = NA))) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   .fns = c(dplyr::n_distinct,
                                            ~ sum(!is.na(.))),
                                   .names = '{.col}_{.fn}')) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = '.col',
                        values_to = '.val') %>%
    dplyr::mutate(.fn = ifelse(stringr::str_extract(.col, '(_1|_2)') == '_1',
                               '.distinct',
                               '.obs'),
                  .col = stringr::str_remove_all(.col, '(_1|_2)')) %>%
    tidyr::pivot_wider(id_cols = .col,
                       names_from = .fn,
                       values_from = .val) %>%
    dplyr::mutate(.nrow = .nrow)

  rm(.nrow)

  if (.prop == TRUE) {
    output %>%
      dplyr::mutate(.distinct = .distinct / .nrow,
                    .obs = .obs / .nrow) %>%
      dplyr::mutate(dplyr::across(.cols = c(.distinct, .obs, .nrow),
                                  .fns = ~ nvl(.x,
                                               value = 0)))
  } else {
    output
  }

}


#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-09-04 : 2021-09-03
#' NOTES       : Developed to help with the Survey Monkey Travel Behaviour Survey disaster.
#' TODO        :
