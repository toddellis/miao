#' dominance
#'
#' Runs a bootstrapped dominance analysis (aka relative weighting analysis or Shapley regression) via the {relaimpo} package and returns a dataframe of all relative importance values and the associated ranks.
#'
#' @param mod Regression model input.
#' @param n_boot Number of bootstrapped model samples to assess.
#' @param method Method for assessing variable dominance, passed to {relaimpo}.
#' @param seed Random seed.
#'
#' @return Dataframe with model method, bootstrapped sample number, predictor variable name, dominance values, and variable ranks.
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars |>
#'   lm(mpg ~ cyl + wt + hp,
#'      data = _) |>
#'     dominance(n_boot = 500,
#'               method = "car")
#' }
#'


dominance <- function(mod,
                      n_boot = 1e3,
                      method = c("lmg", "pmvd", "last", "first", "betasq", "pratt", "genizi", "car"),
                      seed = 8675309,
                      ...) {

  .method <-
    rlang::arg_match(method)

  set.seed(seed)

  .dom <-
    mod |>
    relaimpo::boot.relimp(nboot = n_boot,
                          type = .method,
                          ...) |>
    relaimpo::booteval.relimp()

  clean_dom <- function(x, dom, values_name) {

    .df <-
      x |>
      tibble::as_tibble()

    names(.df) <-
      dom@namen[-1]

    .df <-
      .df |>
      dplyr::mutate(.boot = dplyr::row_number()) |>
      tidyr::pivot_longer(cols = -.boot,
                          names_to = "predictor",
                          values_to = values_name)

    return(.df)

  }

  .output <-
    slot(.dom,
         glue::glue("{.method}.boot")) |>
    clean_dom(dom = .dom,
              values_name = "dominance") |>
    dplyr::left_join(slot(.dom,
                          glue::glue("{.method}.rank.boot")) |>
                       clean_dom(dom = .dom,
                                 values_name = "rank"),
                     by = dplyr::join_by(.boot, predictor)) |>
    dplyr::mutate(.method = .method) |>
    dplyr::relocate(.method,
                    .before = .boot)

  return(.output)


}
