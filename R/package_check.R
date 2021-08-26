#' package_check
#'
#' Install and/or load any necessary libraries, or skip if they are already loaded.
#'
#' @param packages chr : Single character or character vector with package/library names
#' @param quiet lgl : Boolean whether to show which packages were installed, loaded, or skipped, as well as their version numbers.
#' @param repos chr : Specified repo web address to attempt to pull uninstalled libraries from.
#'
#' @return Installs and/or loads libraries on the backend.
#' @export
#'
#' @examples
#' \dontrun{
#' packacge_check(c('tidyverse', 'utastoolkit'))
#' }

package_check <- function(packages,
                          quiet = FALSE,
                          repos = c("https://cloud.r-project.org")) {

  for (package in packages) {
    ## SKIP CHECK ####
    ## Check if package is both installed and loaded
    if (package %in% .packages() & quiet == FALSE) {
      print(paste0("..............Skipping library: ", package, " (", packageVersion(package), ")"))
      ## LOAD CHECK ####
      ## Check if package is installed but not loaded
    } else if (package %in% rownames(installed.packages())) {
      suppressWarnings(suppressPackageStartupMessages(library(package,
                                                              quietly = TRUE,
                                                              character.only = TRUE,
                                                              warn.conflicts = FALSE)))
      if (quiet == FALSE) {
        print(paste0("...............Loading library: ", package, " (", packageVersion(package), ")"))
      }
    } else {
      ## INSTALL CHECK ####
      ## Install and load package if it is not installed
      if (is.na(repos) || is.null(repos)) {
        install.packages(package)
      } else {
        install.packages(package, repos = repos,
                         dependencies = NA, type = getOption("pkgType"))
      }
      suppressWarnings(suppressPackageStartupMessages(library(package,
                                                              quietly = TRUE,
                                                              character.only = TRUE,
                                                              warn.conflicts = FALSE)))
      if (quiet == FALSE) {
        print(paste0("Installing and loading library: ", package, " (", packageVersion(package), ")"))
      }
    }
  }
}

#' FUNCTION    : package_check
#' AUTHOR      : todd.ellis@utas.edu.au
#' DATE        : 2021-08-26 : 2021-03-29 (est.)
#' DESCRIPTION : Installs (if necessary) and loads (if necessary) vector of libraries
#' NOTES       : Set repos to NA if using the `renv` library for package control
#' TODO        :
