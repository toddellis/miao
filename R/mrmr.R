
# df <-
#   CL_MODEL_BAKE_CS |>
#   dplyr::select(TBP_CS, CIRCULARITY_RATIO, TRI_MEAN, ELEV_RANGE, FFDI, KBDI, TMAXDAILY, PRECIPDAILY, REBURN_INTERVAL_MIN)
#
# .mrmr <-
#   mRMRe::mRMR.ensemble(data = mRMRe::mRMR.data(data = data.frame(target = df |>
#                                                                    dplyr::pull(TBP_CS),
#                                                                  df |>
#                                                                    dplyr::select(-TBP_CS))),
#                        target_indices = 1,
#                        feature_count = 6,
#                        solution_count = 1)

mrmr <- function(x) {

  if (!class(x) %in% c("mRMRe.Filter")) {
    stop("Supply a feature output by mRMRe::mRMR.ensemble(...).")
  }

  .length <-
    length(x@feature_names)

  output <-
    tibble::tibble(
      filter = 1:.length,
      .var = x@feature_names,
      causality = x@causality_list$`1`
    ) |>
    ## TODO: .run is *not* correct here if we have multiple runs. Sometimes a secondary run is the only one to feature a variable, meaning row_number will classify that as from run #1.
    ## Scratch fix....
    # tibble::as_tibble(x@filters$`1`) |>
    # tidyr::pivot_longer(cols = tidyselect::everything(),
    #                     names_to = ".run",
    #                     values_to = ".id",
    #                     names_transform = ~ readr::parse_number(.x))
    dplyr::left_join(tibble::as_tibble(x@filters$`1`) |>
                       tidyr::pivot_longer(cols = tidyselect::everything(),
                                           names_to = ".run",
                                           values_to = ".id"))
  dplyr::left_join(tibble::tibble(filter = as.vector(x@filters$`1`),
                                  scores = as.vector(x@scores$`1`)) |>
                     dplyr::mutate(.run = dplyr::row_number(),
                                   .by = c(filter)) |>
                     dplyr::mutate(.rank = dplyr::row_number(),
                                   .by = c(.run))) |>
    dplyr::arrange(.run, .rank) |>
    dplyr::filter(.var != "target") |>
    ## Post-hoc, non-ideal fix to ranks that include the "target" variable in the count.
    dplyr::mutate(.rank = dplyr::row_number(),
                  .by = c(.run)) |>
    dplyr::relocate(.run, .rank,
                    .before = .var) |>
    dplyr::relocate(filter,
                    .after = .var)

  return(output)

}
