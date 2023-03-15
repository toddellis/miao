

smooth_data <- function(x,
                        var_x,
                        var_y,
                        res_x = 0.1,
                        res_y = 10) {
  
  .min <- min(dplyr::pull(x, rlang::ensym(var_x)), na.rm = TRUE)
  .max <- max(dplyr::pull(x, rlang::ensym(var_x)), na.rm = TRUE)
  .res_x <- res_x
  .res_y <- res_y
  # .var <- 
  # .grps <- c(dplyr::group_vars(x), rlang::as_string(.var))
  # print(.grps)
  
  x |>
    dplyr::summarise(dplyr::across(.cols = rlang::ensym(var_x),
                                   .fns = ~ seq(.min, .max, by = .res_x))) |>
    dplyr::left_join(x) |>
    suppressMessages() |>
    dplyr::mutate(dplyr::across(.cols = rlang::ensym(var_y),
                                .fns = ~ zoo::na.approx(.x) |>
                                  zoo::rollapply(res_y,
                                                 mean,
                                                 align = "center",
                                                 fill = NA))) |>
    dplyr::ungroup()
  
}
