#' wordcount
#'
#' Count the number of distinct words in a sentence.
#'
#' @param x String variable or vector.
#'
#' @return Wordcount variable or vector.
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

wordcount <- function(x) {

  stringr::str_count(x,
                     '[A-z]\\W+') + 1L

}
