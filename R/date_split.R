#' date_split
#'
#' Splits a date column into separate year, month, day, and julian day columns.
#'
#' @param x Dataframe
#' @param col Date format column to split
#'
#' @return Dataframe with added year, month, day, and yday columns.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

date_split <- function(x, col) {

  dplyr::mutate(.data = x,
                dplyr::across(.cols = rlang::ensym(col),
                              .fns = list(year = lubridate::year,
                                          month = lubridate::month,
                                          day = lubridate::day,
                                          yday = lubridate::yday),
                              .names = "{.fn}"))

}
