#' pprint
#'
#' Shortcut to print out .png and .pdf figures at a high resolution to project subfolder.
#'
#' @param p Figure to print.
#' @param file Output figure name.
#' @param dpi Dots per inch.
#' @param overwrite Logical to replace existing file or not if it exists.
#' @param folder Output figure subfolder (assuming standardised project folder structure).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'

pprint <- function(p,
                   file = NULL,
                   dpi = 540,
                   overwrite = FALSE,
                   folder = "02 Results/Figures",
                   ...) {

  .folder <-
    here::here(folder)

  if (is.null(p)) {
    p <- ggplot2::last_plot()
  }

  if (is.null(file)) {
    .file <- here::here(glue::glue("{.folder}/Scratch/Temp"))
    .pdf <- glue::glue("{.file}.pdf")
    .png <- glue::glue("{.file}.png")
  } else {
    .file <- glue::glue("{.folder}/{file}")
    .pdf <- glue::glue("{.file}.pdf")
    .png <- glue::glue("{.file}.png")
  }

  if (overwrite) {
    cowplot::ggsave2(.pdf,
                     p,
                     dpi = dpi,
                     ...)

    cowplot::ggsave2(.png,
                     p,
                     dpi = dpi,
                     ...)

    ## TODO: Add .svg?
    # .svg <- glue::glue("{file}.svg")
  }

  if (!overwrite &
      !file.exists(.pdf)) {

    cowplot::ggsave2(.pdf,
                     p,
                     dpi = dpi,
                     ...)

  }

  if (!overwrite &
      !file.exists(.png)) {

    cowplot::ggsave2(.png,
                     p,
                     dpi = dpi,
                     ...)

  }

}
