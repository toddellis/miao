#' stats_dump
#'
#' Calculates all the common summary statistics for reporting.
#'
#' @param x Numeric variable to summarise.
#' @param conf_int Confidence level for calculating mean confidence interval.
#'
#' @return Dataframe with useful summary statistics.
#' @export
#'
#' @examples
#' runif(100, 0, 100) %>%
#'  round(0) %>%
#'   tibble::as_tibble() %>%
#'   dplyr::mutate(group = c(rep("GROUP A", 35), rep("GROUP B", 65))) %>%
#'   dplyr::group_by(group) %>%
#'   dplyr::summarise(stats_dump(value))
#'

stats_dump <- function(x,
                       conf_int = 0.95) {
  .data = x

  .n = length(.data)
  .median = ggplot2::median_hilow(.data,
                                  conf.int = conf_int) |>
    dplyr::rename(.median = y,
                  .lwr.quantile = ymin,
                  .upr.quantile = ymax)
  .boot = ggplot2::mean_cl_boot(.data,
                                conf.int = conf_int) |>
    dplyr::rename(.mean = y,
                  .lwr.boot = ymin,
                  .upr.boot = ymax)
  .normal = ggplot2::mean_cl_normal(.data,
                                    conf.int = conf_int) |>
    dplyr::select(.lwr.normal = ymin,
                  .upr.normal = ymax)
  # .sdl = ggplot2::mean_sdl(.data)
  .var = var(.data, na.rm = TRUE)
  .sd = sd(.data, na.rm = TRUE)
  .se = .sd / sqrt(.n)
  .min = min(.data, na.rm = TRUE)
  .max = max(.data, na.rm = TRUE)
  .perc.05 = quantile(.data, probs = 0.05, na.rm = TRUE)
  .perc.25 = quantile(.data, probs = 0.25, na.rm = TRUE)
  .perc.75 = quantile(.data, probs = 0.75, na.rm = TRUE)
  .perc.95 = quantile(.data, probs = 0.95, na.rm = TRUE)
  .distinct = dplyr::n_distinct(.data)
  .na = sum(is.na(.data))


  OUTPUT =
    tibble::tibble(
    .n,
    tidyselect::all_of(.median),
    tidyselect::all_of(.boot),
    tidyselect::all_of(.normal),
    .min,
    .max,
    .var,
    .sd,
    .se,
    .perc.05,
    .perc.25,
    .perc.75,
    .perc.95,
    .distinct,
    .na
  ) |>
    dplyr::select(.n,
                  .distinct,
                  .median,
                  .mean,
                  .min,
                  .max,
                  .var,
                  .sd,
                  .se,
                  .lwr.boot,
                  .upr.boot,
                  .perc.05,
                  .perc.25,
                  .perc.75,
                  .perc.95,
                  .lwr.normal,
                  .upr.normal,
                  .lwr.quantile,
                  .upr.quantile,
                  .na)

  return(OUTPUT)
}
