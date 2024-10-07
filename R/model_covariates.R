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

    p0 <-
      .da |>
      ggplot2::ggplot(ggplot2::aes(x = dominance,
                                   y = forcats::fct_reorder(predictor,
                                                            rank,
                                                            mean,
                                                            .desc = TRUE))) +
      theme_meow() +
      ggplot2::labs(y = NULL) +
      ggplot2::scale_x_continuous(limits = c(0, NA_real_))

    if (inherits(mod, "gam")) {

      .r2 <-
        round(sum(.da$dominance, na.rm = TRUE), digits = 3)

      ## TODO: Investigate what this statistically means.
      if (any(.da$unique < 0)) {
        warning(glue::glue("Some *unique* variable R-squared contribution reported as negative (min: {min(.da$unique)})."))
      }
      if (any(.da$shared < 0)) {
        warning(glue::glue("Some *shared* variable R-squared contribution reported as negative (min: {min(.da$shared)})."))
      }

      p1 <-
        p0 +
        ggplot2::geom_col(fill = "#BFBFBF") +
        ggplot2::geom_col(ggplot2::aes(x = unique),
                          fill = "#808080") +
        ggplot2::labs(x = glue::glue("Relative importance up to {.r2}"))

      p2 <-
        .da |>
        dplyr::select(predictor, rank, unique, shared) |>
        tidyr::pivot_longer(cols = c(unique, shared),
                            names_to = "wt",
                            values_to = "dominance") |>
        ggplot2::ggplot(ggplot2::aes(x = dominance,
                                     y = forcats::fct_reorder(predictor,
                                                              rank,
                                                              mean,
                                                              .desc = TRUE))) +
        theme_meow() +
        ggplot2::labs(x = glue::glue("Unique ({round(sum(.da$unique, na.rm = TRUE), digits = 3)}) vs. shared ({ round(sum(.da$shared, na.rm = TRUE), digits = 3)}) importance"),
                      y = NULL) +
        ggplot2::scale_x_continuous(labels = scales::percent,
                                    limits = c(0, NA_real_)) +
        ggplot2::geom_col(aes(fill = wt),
                          show.legend = FALSE,
                          position = "fill") +
        ggplot2::scale_fill_manual(values = c("#BFBFBF", "#808080"))

    } else {

      p1 <-
        p0 +
        ggdist::stat_slabinterval(point_interval = "mean_hdci",
                                  density = "histogram",
                                  fill = "#808080",
                                  .width = c(0.5, 0.89)) +
        ggplot2::labs(x = "Relative importance")

      p2 <-
        .da |>
        ggplot2::ggplot(ggplot2::aes(x = rank,
                                     y = forcats::fct_reorder(predictor,
                                                              rank,
                                                              mean,
                                                              .desc = TRUE))) +
        theme_meow() +
        ggplot2::labs(x = "Rank",
                      y = NULL) +
        ggplot2::coord_cartesian(xlim = c(1, NA_real_)) +
        ggplot2::scale_x_continuous(n.breaks = round(max(.da$rank))) +
        ggdist::stat_slabinterval(point_interval = "mean_hdci",
                                  density = "histogram",
                                  fill = "#808080",
                                  breaks = length(unique(.da$predictor)),
                                  .width = c(0.5, 0.89))

    }

    if ((plot_rows == 2 & plot_vif) |
        (plot_rows == 1)) {
      p2 <-
        p2 +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }

    if (plot_vif) {

      .max <-
        round(max(.vif$vif))

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
        ggplot2::geom_vline(xintercept = c(2, 3, 4, 5, ifelse(.max > 5, 10, NA)),
                            linetype = "dashed",
                            alpha = 0.3,
                            linewidth = 1.25) +
        ggplot2::geom_col() +
        theme_meow() +
        ggplot2::scale_fill_identity() +
        ggplot2::coord_cartesian(xlim = c(1, NA_real_)) +
        ggplot2::scale_x_continuous(breaks = seq(1, ifelse(.max > 5, ifelse(.max > 10, .max, 10), 5), by = 1)) +
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

