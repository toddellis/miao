#' theme_meow
#'
#' Use miao-preferred ggplot2 theme, ideal for publishing from Quarto.
#'
#' @param axis.title Text size for axis title.
#' @param axis.text Text size for axis text.
#' @param strip.text Text size for facet strips.
#' @param legend.text Text size for legend.
#' @param line_col Color for axis lines and ticks; defaults to dark grey.
#'
#' @return Theme ready for adding to ggplot2 object.
#' @export
#'
#' @examples
#' tibble::tibble(x = 1:10,
#'                y = 10:1) |>
#'  ggplot(aes(x = x, y = y)) +
#'  geom_point() +
#'  theme_meow()
#'

theme_meow <- function(axis.title = 14,
                       axis.text = 10,
                       strip.text = 10.5,
                       legend.text = 10.25,
                       strip.bg = "white",
                       plot.bg = "white",
                       line.col = "#838383") {
  cowplot::theme_cowplot() +
    ggplot2::theme(
      legend.position = 'bottom',
      axis.ticks = ggplot2::element_line(colour = line.col),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = line.col),
      axis.text = ggplot2::element_text(size = axis.text),
      axis.title = ggplot2::element_text(size = axis.title),
      strip.background = ggplot2::element_rect(fill = strip.bg),
      strip.text = ggplot2::element_text(size = strip.text),
      legend.text = ggplot2::element_text(size = legend.text),
      panel.background = ggplot2::element_rect(fill = plot.bg),
      plot.background = ggplot2::element_rect(fill = plot.bg)
    )
}
