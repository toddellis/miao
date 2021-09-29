#' right
#'
#' Simplified variant of `substr` imitating the Excel & Alteryx `RIGHT` function with an option to pad.
#'
#' @param x Vector to extract from.
#' @param n Number of characters to extract starting from the right.
#' @param pad Character to pad string values with (from the right) if the input vectors are less than n.
#'
#' @return Transformed vector of `n` length.
#' @export
#'
#' @examples
#' right(c('Example', 'E.g.', 'Eg'),
#'       n = 4,
#'       pad = ' ')
#'

right <- function(x,
                  n = 4,
                  pad = NULL) {

  output <- substr(x,
                   start = nchar(x) - n + 1,
                   stop = nchar(x))

  if (!is.null(pad)) {

    if (nchar(pad) >= 2) {

      warning('Pad should only be a single character value. Defaulting to only the first given character.')

      pad = substr(pad, start = 1, stop = 1)

    }

    output <- gsub(' ',
                   pad,
                   sprintf(paste0('%-', n, 's'), output))
  }

  return(output)

}
