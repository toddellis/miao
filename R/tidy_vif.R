#' tidy_vif
#'
#' Quick and tidy-friendly adaptation of {car}'s `vif()`. Returns a dataframe with variance inflation, tolerance, and a flag identifying if either is beyond common cut-offs reflecting problematic input variables. Note that VIF is not particularly useful when looking at categorical predictors, and will often report high VIF if any categories with a small sample size exist.
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
                  multicollinearity_code = dplyr::case_when(
                    vif <= 1 ~ 0,
                    dplyr::between(vif, 1, 2) ~ 1,
                    dplyr::between(vif, 2, 3) ~ 2,
                    dplyr::between(vif, 3, 4) ~ 3,
                    dplyr::between(vif, 4, 5) ~ 4,
                    dplyr::between(vif, 5, 10) ~ 5,
                    vif >= 10 ~ 6
                  ),
                  multicollinearity_desc = dplyr::recode(multicollinearity_code,
                                                         "0" = "None",
                                                         "1" = "Low",
                                                         "2" = "Moderate: Tolerable",
                                                         "3" = "Moderate: Likely Tolerable",
                                                         "4" = "Moderate: Potentially Tolerable",
                                                         "5" = "High",
                                                         "6" = "Extreme") |>
                    forcats::fct_reorder(multicollinearity_code,
                                         mean)) |>
    tibble::as_tibble() |>
    dplyr::arrange(vif)

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
