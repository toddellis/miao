#' read_arrow
#'
#' Shorthand for {arrow}'s confusing syntax to read a directory.
#'
#' @param files Vector of file paths to be read by
#'
#' @return Bound dataset comprised of multiple files.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

read_arrow <- function(files) {

  .data <-
    arrow::open_dataset(files)

  .output <-
    arrow::Scanner$create(.data)$ToTable() |>
    tibble::as_tibble()

  rm(.data)

  return(.output)

}
