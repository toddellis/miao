#' create_project
#'
#' Creates common folder structure for new projects for consistency.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_project()
#'}

create_project <- function() {

  .dir <- getwd()

  suppressWarnings(dir.create(paste0(.dir, '/data/')))
  suppressWarnings(dir.create(paste0(.dir, '/data/input/')))
  suppressWarnings(dir.create(paste0(.dir, '/data/output')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/literature/')))
  suppressWarnings(dir.create(paste0(.dir, '/figures/')))
  suppressWarnings(dir.create(paste0(.dir, '/tables/')))

  print(paste0('Project directories created in \'', .dir, '/\'.'))

}
