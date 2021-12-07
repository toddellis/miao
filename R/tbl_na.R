#' tbl_na
#'
#' Check the number of NAs for all columns and provide a summary view.
#'
#' @param x Dataframe.
#' @param pivot Logical whether to pivot the output to long data format.
#' @param drop_zeros Logical whether to drop groups or columns with zero missing values.
#'
#' @return Wide or long format dataframe with the count of zeros across all the columns in the original dataset.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' tibble::tibble(a = c(1, 2, NA),
#'                b = c(NA, NA, 'Q'),
#'                c = c(3, 2, 1)) %>%
#'     tbl_na(pivot = TRUE,
#'            drop_zeros = FALSE)

tbl_na <- function(x,
                   pivot = TRUE,
                   drop_zeros = TRUE) {

  .nrow = nrow(x)

  output <- x %>%
    dplyr::summarise(dplyr::across(.cols = tidyselect::everything(),
                                   .fns = ~ sum(is.na(.x))),
                     .groups = 'drop')

  .grp = dplyr::group_vars(x)

  if (.grp %>% tibble::as_tibble() %>% nrow() == 0) {

    output <- output %>%
      tidyr::pivot_longer(cols = tidyselect::everything(),
                          names_to = '.col',
                          values_to = '.sum')

  } else {

    output <- output %>%
      tidyr::pivot_longer(cols = !tidyselect::all_of(.grp),
                          names_to = '.col',
                          values_to = '.sum')

  }

  output <-
    output %>%
    dplyr::mutate(.percent = round((.sum / .nrow) * 100,
                                   2))

  if (drop_zeros) {

    output <- output %>%
      dplyr::filter(.sum > 0)

  }

  output <- if (pivot) {

    output

  } else {

    output %>%
      tidyr::pivot_longer(cols = c(.sum, .percent),
                          names_to = '.metric',
                          values_to = '.value') %>%
      tidyr::pivot_wider(id_cols = c(tidyselect::all_of(.grp), .metric),
                         names_from = .col,
                         values_from = .value)

  }

  return(output)

}

#' FUNCTION    : tbl_na
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-11-24 : 2021-08-02
#' DESCRIPTION : Check the number of NAs for all columns
#' NOTES       : Loosely based on suggestions via https://sebastiansauer.github.io/sum-isna/
#' TODO        :
