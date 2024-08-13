#' model_covariates
#'
#' Uses both tidy_vif and dominance to assessment model covariates. Shorthand for plotting dominance values and variable ranks, while also printing variance inflation factor scores.
#'
#' @param mod Input model to assess.
#'
#' @return Output from tidy_vif and figures of dominance values.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   mtcars |>
#'     lm(mpg ~ cyl + disp + hp + wt,
#'        data = _) |>
#'     model_covariates(plot_rows = 2,
#'                      plot_vif = TRUE,
#'                      n_boot = 500,
#'                      method = "car")
#'
#' }

model_covariates <- function(mod,
                             plot_rows = 1,
                             plot_vif = FALSE,
                             ...) {

  .vif <-
    mod |>
    tidy_vif()

  .da <-
    mod |>
    dominance(...)

  p1 <-
    .da |>
    ggplot2::ggplot(ggplot2::aes(x = dominance,
                                 y = forcats::fct_reorder(predictor,
                                                          rank,
                                                          .desc = TRUE))) +
    ggdist::stat_slabinterval(point_interval = "mean_hdci",
                              density = "histogram",
                              .width = c(0.5, 0.89)) +
    theme_meow() +
    ggplot2::labs(x = "Relative importance",
                  y = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1))

  p2 <-
    .da |>
    ggplot2::ggplot(ggplot2::aes(x = rank,
                                 y = forcats::fct_reorder(predictor,
                                                          rank,
                                                          .desc = TRUE))) +
    ggdist::stat_slabinterval(point_interval = "mean_hdci",
                              density = "histogram",
                              breaks = length(unique(.da$predictor)),
                              .width = c(0.5, 0.89)) +
    theme_meow() +
    ggplot2::labs(x = "Rank",
                  y = NULL) +
    ggplot2::coord_cartesian(xlim = c(1, NA_real_)) +
    ggplot2::scale_x_continuous(n.breaks = round(max(.da$rank)))

  if (plot_vif) {

    p3 <-
      .vif |>
      dplyr::mutate(fill = dplyr::recode(multicollinearity_code,
                                         "0" = "#64BF30",
                                         "1" = "#64BF30",
                                         "2" = "#64BF30",
                                         "3" = "#FEDD3A",
                                         "4" = "#F78100",
                                         "5" = "#AD0909",
                                         "6" = "#AD0909")) |>
      ggplot2::ggplot(ggplot2::aes(x = vif,
                                   y = forcats::fct_reorder(predictor,
                                                            vif,
                                                            mean,
                                                            .desc = TRUE),
                                   fill = fill)) +
      ggplot2::geom_col() +
      theme_meow() +
      ggplot2::scale_fill_identity() +
      ggplot2::geom_vline(xintercept = c(2, 3, 4, 5),
                          linetype = "dashed",
                          alpha = 0.3,
                          size = 1.25) +
      ggplot2::coord_cartesian(xlim = c(1, NA_real_)) +
      ggplot2::scale_x_continuous(n.breaks = round(max(.vif$vif))) +
      ggplot2::labs(x = "Variance inflation score",
                    y = NULL)

    print(patchwork::wrap_plots(p1 + p2 + p3 + patchwork::plot_layout(nrow = plot_rows))) |>
      suppressWarnings()

  } else {

    print(.vif)

    print(patchwork::wrap_plots(p1 + p2 + patchwork::plot_layout(nrow = plot_rows))) |>
      suppressWarnings()

  }


}
