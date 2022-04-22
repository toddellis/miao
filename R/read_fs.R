#' read_folder
#'
#' Loop through objects in an fs::dir_ls filelist and apply data.table::fread across them.
#'
#' @param files Filelist generated using fs::dir_ls.
#' @param colClasses Column classes if desired.
#' @param method Option to use for loop or purrr::map.
#'
#' @return Either bound dataframe (via loop) or nested dataset (via purrr).
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'

read_fs <- function(files,
                        colClasses = c(NULL),
                        method = c("purrr", "base")) {

  .method = match.arg(method)

  if (.method == 'base') {
    ### Create empty list to fill
    df <- list()

    .class <- class(files)[1]

    if (.class != 'fs_path') {
      ## Most likely "tbl_df" but potentially any number of others
      files <-
        files |>
        tibble::as_tibble() |>
        dplyr::rename(value = 1) |>
        dplyr::pull(value)
    }

    ### Loop through each file in the input and add it to the list
    for (i in 1:length(files)) {
      df[[i]] <- data.table::fread(files[i],
                                   colClasses = colClasses)
    }

    ### Bind the list into a single data table
    dplyr::bind_rows(df)

  } else if (.method == 'purrr') {

    files |>
      tibble::as_tibble() |>
      dplyr::rename(value = 1) |>
      dplyr::mutate(data = purrr::map(.x = value,
                                      .f = ~ data.table::fread(.x,
                                                               colClasses = colClasses))) |>
      dplyr::select(-value)

  }

}

#' @rdname read_fs
#' @export
read_folder <- read_fs
