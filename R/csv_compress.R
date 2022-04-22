#' csv_compress
#'
#' Read a folder of .csv files and compress them individually to .gz
#'
#' @param folder Source / destination folder for compressing .csv files.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

csv_compress <- function(folder) {

  files <- fs::dir_ls(folder,
                      glob = '*.csv')

  for(i in 1:length(files)) {

    foo <- data.table::fread(files[i])

    data.table::fwrite(foo,
                       paste0(substr(files[i], 1, nchar(files[i]) - 4), '.gz'))

    rm(foo)
  }

}
