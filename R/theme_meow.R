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

theme_meow <- function(axis.title = 10,
                       axis.text = 7.5,
                       strip.text = 7,
                       legend.text = 9.25,
                       line_col = "#838383") {
  cowplot::theme_cowplot() +
    theme(
      legend.position = 'bottom',
      axis.ticks = element_line(colour = line_col),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_line(colour = line_col),
      axis.text = element_text(size = axis.text),
      axis.title = element_text(size = axis.title),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = strip.text),
      legend.text = element_text(size = legend.text)
    )
}
