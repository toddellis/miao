#' collectable
#'
#' Wraps a timer around functions that pulls a lazy database query into memory.
#'
#' @param x Dataset or lazy database query.
#' @param qid Optional string query identifier.
#' @param format Output format type, either via `data.table`, `dtplyr`, or `tibble`.
#'
#' @return A queried database connection stored in local memory and the time it took to pull into memory. Alternatively provides the time to process a piped workflow or single function if the input dataset is not a lazy database connection.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' dplyr::tbl(connection,
#'            dbplyr::in_schema("SCHEMA",
#'                              "TABLE_NAME")) %>%
#'   collectable(qid = "an impossible and certainly fake query",
#'               format = 'tibble')
#' }
#'

collectable <- function(x,
                        qid = 'query',
                        format = 'tibble') {

  if (is.null(qid) || is.na(qid)) {
    qid = 'query'
  }

  tictoc::tic(paste0('Time to process ', qid))

  output <- if (format %in% c('data.table', 'dt')) {
    x %>%
      data.table::as.data.table()
  } else if (format == 'lazy') {
    x %>%
      dtplyr::lazy_dt()
  } else if (format %in% c('tbl', 'tibble', 'tidy')) {
    x %>%
      dplyr::collect() %>%
      tibble::as_tibble()
  } else {
    stop('Please choose either a `data.table` (default), `lazy`, or `tibble` format for the output.')
  }

  tictoc::toc()

  return(output)
}


#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-09-04 : 2021-08-06
#' NOTES       :
#' TODO        : Reconsider how format is chosen.
