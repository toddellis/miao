#' tidy_gam
#'
#' Wrapped for 2x broom::tidy as applied to a GAM object, pulling both parametric and smooth terms.
#'
#' @param x GAM model object
#' @param conf.level Confidence level for pulling confidence intervals from parametric terms.
#'
#' @return Dataframe with model/term metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
#'


tidy_gam <- function(x,
                     conf.level = 0.95) {
  .model <-
    x
  .conf.level <-
    conf.level

  .parametric <-
    .model |>
    broom::tidy(parametric = TRUE,
                conf.int = TRUE,
                conf.level = .conf.level) |>
    dplyr::mutate(term.type = "Parametric")

  .nonparametric <-
    .model |>
    broom::tidy(parametric = FALSE) |>
    dplyr::mutate(term.type = "Smooth")

  output <-
    .parametric |>
    dplyr::bind_rows(.nonparametric) |>
    dplyr::select(term,
                  term.type,
                  estimate,
                  std.error,
                  edf,
                  ref.df,
                  statistic,
                  p.value,
                  conf.low,
                  conf.high)

  return(output)

}
