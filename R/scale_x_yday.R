#' scale_x_yday
#'
#' Converts lubridate::yday (Julian day of the year) axis to breaks and labels noting month of the year
#'
#'
#' @return ggplot figure with monthly breaks and labels on x axis.
#' @export
#'
#'
#'

scale_x_yday <- function() {

  scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                     labels = month.abb)

}
