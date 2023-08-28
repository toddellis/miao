#' mod_env
#'
#' Modify internal paths for `HOME` and `R_USER` options, removing potential issues with OneDrive defaults on Windows 11. Stands for "Microsoft OneDrive Environments."
#'
#' @param quiet Logical. Whether to print the changed environment paths or not.
#'
#' @return Modified internal `HOME` and `R_USER` paths.
#' @export
#'
#' @examples
#' \dontrun{
#'
#'    mod_env()
#'
#' }

mod_env <- function(quiet = TRUE) {

  .env_home <-
    Sys.getenv("HOME")

  if (stringr::str_detect(.env_home,
                          "OneDrive") |
      stringr::str_detect(Sys.getenv("R_USER"),
                          "OneDrive")) {

    .env_local <-
      glue::glue("C:/Users/{keyring::key_list()[1,2]}/Documents")

    Sys.setenv(HOME = .env_local,
               R_USER = .env_local)

    if (!quiet) {
      warning(glue::glue("Current Windows 'HOME' and 'R_USER' environments changed from {.env_home} to {.env_local}."))
    }

    rm(.env_local)

  }

  rm(.env_home)

}
