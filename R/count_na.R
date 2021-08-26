#' count_na
#'
#' Check the number of NAs for all columns
#'
#' @param x : dataframe
#' @param pivot lgl : whether to pivot the output to long data formate
#' @param drop_zeros lgl : whether to drop groups or columns with zero NAs
#'
#' @return wide or long format dataframe with the count of zeros across all the columns in the original dataset
#' @export
#'
#' @examples
#' tibble::tibble(a = c(1, 2, NA),
#'                b = c(NA, NA, 'Q'),
#'                c = c(3, 2, 1)) %>%
#'     count_na(pivot = TRUE,
#'              drop_zeros = FALSE)

count_na <- function(x,
                     pivot = FALSE,
                     drop_zeros = TRUE) {

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

  if (drop_zeros) {

    output <- output %>%
      dplyr::filter(.sum > 0)

  }

  output <- if (pivot) {

    output

  } else {

    output %>%
      tidyr::pivot_wider(id_cols = tidyselect::all_of(.grp),
                         names_from = .col,
                         values_from = .sum)

  }

  return(output)

}

#' FUNCTION    : count_na
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-08-26 : 2021-08-02
#' DESCRIPTION : Check the number of NAs for all columns
#' NOTES       : Loosely based on suggestions via https://sebastiansauer.github.io/sum-isna/
#' TODO        : Set up pivot_longer to recognize groups
