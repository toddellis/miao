#' cmdstan_set
#'
#' Sets cmdstan parameters to use cmdstan easily from {brms}. TODO: I'm not actually sure how necessary these fixes still are, but they were required mid-2024 to run cmdstan....
#'
#' @param user OS username used for finding the cmdstan installation directory. Optional: By default, `Sys.getenv("USERPROFILE")` is called.
#'
#' @return No object -- fixes pathing issues for running cmdstan (if cmdstan is installed).
#' @export
#'
#' @examples
#' \dontrun{
#' cmdstan_set()
#' }
#'

cmdstan_set <- function(user = NA) {
  if (!is.null(user)) {
    .path <- Sys.getenv("USERPROFILE")
  } else {
    .path <- paste0("C:\\Users\\", user)
  }

  ## TODO: Are these steps still necessary?
  Sys.setenv(HOME = paste0(.path, "\\Documents"))
  Sys.setenv(R_USER = paste0(.path, "\\Documents"))

  if (!"cmdstanr" %in% installed.packages()) {
    install.packages("cmdstanr")
  }

  if (length(cmdstanr::cmdstan_path()) == 0) {
    cmdstanr::install_cmdstan()
  }

  .cmdstan <-
    fs::dir_info(fs::dir_ls(.path, regexp = ".cmdstan", all = TRUE, type = "directory", recurse = FALSE)) |>
    dplyr::bind_rows(fs::dir_info(fs::dir_ls(paste0(.path, "\\Documents"), regexp = ".cmdstan", all = TRUE, type = "directory", recurse = FALSE))) |>
    dplyr::filter(birth_time == max(birth_time, na.rm = TRUE)) |>
    dplyr::slice(1) |>
    dplyr::pull(path)

  cmdstanr::check_cmdstan_toolchain(fix = TRUE)

  cmdstanr::set_cmdstan_path(path = .cmdstan)

}
