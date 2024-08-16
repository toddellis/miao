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

  if (inherits(mod, "gam")) {

    warning("Generalised additive models ignore `n_boot` (for now) and `method` input options.")

    ## TODO: Pick apart `gam.hp::gam.hp()` function to ingest its methods into {miao} *and* add bootstrapped sampling.
    ##     : The rationale here is...this package just seems incredibly poorly designed (like {relaimpo}, tbqh), and the scripts are full of possible bugs and beta-testing code fragments or created objects that go nowhere. I'm honestly a bit sceptical of it doing what it sets out to do correctly. There's no reason that `type = ...` needs to be binary as an input -- processing both 'adjR2' and 'dev' would take the same processing time. The authors of the package also designed it and a myriad of nearly-identical packages from a perspective of inflating their own h-index, which is really danged obnoxious. I also would simply like to understand how the method works to possibly improve on it (e.g., bootstrapping).
    .dom <-
      gam.hp::gam.hp(mod,
                     ...,
                     commonality = FALSE)

    .output <-
      .dom$hierarchical.partitioning |>
      as.data.frame() |>
      tibble::rownames_to_column("predictor") |>
      janitor::clean_names() |>
      dplyr::transmute(predictor = stringr::str_trim(predictor),
                       unique,
                       shared = average_share,
                       dominance = individual,
                       dominance_prop = prop.table(dominance),
                       rank = as.numeric(forcats::fct_rev(as.factor(dominance))))

  } else {

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
      dplyr::mutate(.method = .method,
                    dominance_prop = prop.table(dominance),
                    .by = c(.boot)) |>
      dplyr::relocate(.method,
                      .before = .boot) |>
      dplyr::relocate(rank,
                      .after = dominance_prop)

  }

  return(.output)

}
