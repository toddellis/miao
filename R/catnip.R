#' catnip
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
#'  tibble::as_tibble() %>%
#'  dplyr::mutate(group = c(rep("GROUP A", 35), rep("GROUP B", 65))) %>%
#'  dplyr::group_by(group) %>%
#'  catnip(value)
#'

catnip <- function(data, x,
                   conf_int = 0.95) {

  .groups =
    dplyr::group_vars(data)

  data %>%
    dplyr::summarise(.count = dplyr::n(),
                     .distinct = dplyr::n_distinct({{ x }}),
                     .na = sum(is.na({{ x }})),
                     .boot = ggplot2::mean_cl_boot({{ x }},
                                                   conf.int = conf_int) |>
                       dplyr::rename(.mean = y,
                                     .ci.lwr.boot = ymin,
                                     .ci.upr.boot = ymax),
                     .normal = ggplot2::mean_cl_normal({{ x }},
                                                       conf.int = conf_int) |>
                       dplyr::select(.ci.lwr.normal = ymin,
                                     .ci.upr.normal = ymax),
                     .min = min({{ x }}, na.rm = TRUE),
                     .p025 = quantile({{ x }}, probs = 0.025, na.rm = TRUE),
                     .p05 = quantile({{ x }}, probs = 0.05, na.rm = TRUE),
                     .p10 = quantile({{ x }}, probs = 0.10, na.rm = TRUE),
                     .q1 = quantile({{ x }}, probs = 0.25, na.rm = TRUE),
                     .median = median({{ x }}, na.rm = TRUE),
                     .q3 = quantile({{ x }}, probs = 0.75, na.rm = TRUE),
                     .p90 = quantile({{ x }}, probs = 0.90, na.rm = TRUE),
                     .p95 = quantile({{ x }}, probs = 0.95, na.rm = TRUE),
                     .p975 = quantile({{ x }}, probs = 0.975, na.rm = TRUE),
                     .max = max({{ x }}, na.rm = TRUE),
                     .var = var({{ x }}, na.rm = TRUE),
                     .sd = sd({{ x }}, na.rm = TRUE),
                     .se = sd({{ x }}) / sqrt(n()),
                     .groups = 'drop') |>
    tidyr::unnest(cols = c(.median, .boot, .normal))

  }
