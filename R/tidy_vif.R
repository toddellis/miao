#' tidy_vif
#'
#' Quick and tidy-friendly adaptation of {car}'s `vif()`. Returns a dataframe with variance inflation, tolerance, and a flag identifying if either is beyond common cut-offs reflecting problematic input variables.
#'
#' @param mod Input model to assess for variance inflation among predictors.
#'
#' @return Dataframe with model/term metrics.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   mtcars |>
#'     lm(mpg ~ .,
#'        data = _) |>
#'     tidy_vif()
#'
#' }

tidy_vif <- function(mod) {

  .df <-
    mod |>
    car::vif() |>
    as.data.frame() |>
    tibble::rownames_to_column("predictor") |>
    dplyr::rename(vif = 2) |>
    dplyr::mutate(tolerance = 1 / vif,
                  multicollinearity = vif >= 5 | tolerance <= 0.2)

  return(.df)

}

## Goal: Bootstrapped VIF
#
# tidy_vif <- function(x, y,
#                      n_boot = 500,
#                      n_sample = 500) {
#
#   if (n_boot >= 1) {
#     df <- list()
#
#     for (i in 1:n_boot) {
#       df[[i]] <-
#         x |>
#         dplyr::slice_sample(n = n_sample,
#                             replace = TRUE) |>
#         dplyr::mutate(.boot = i)
#     }
#
#   }
#
#   return(dplyr::bind_rows(df))
# }
