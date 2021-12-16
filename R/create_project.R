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

create_project <- function(quiet = FALSE) {

  .dir <- getwd()

  suppressWarnings(dir.create(paste0(.dir, '/data/')))
  suppressWarnings(dir.create(paste0(.dir, '/data/input/')))
  suppressWarnings(dir.create(paste0(.dir, '/data/output/')))
  suppressWarnings(dir.create(paste0(.dir, '/data/scratch/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/literature/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/pptx/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/reports/')))
  suppressWarnings(dir.create(paste0(.dir, '/documents/scratch/')))
  suppressWarnings(dir.create(paste0(.dir, '/figures/')))
  suppressWarnings(dir.create(paste0(.dir, '/figures/scratch/')))
  suppressWarnings(dir.create(paste0(.dir, '/scripts/')))
  suppressWarnings(dir.create(paste0(.dir, '/scripts/scratch/')))
  suppressWarnings(dir.create(paste0(.dir, '/tables/')))
  suppressWarnings(dir.create(paste0(.dir, '/tables/scratch/')))

  if (!quiet) {
    print(paste0('Project directories created in \'', .dir, '/\'.'))
  }

}
