#' summarise_sentiment
#'
#' Summarises sentiment for all sentiment dictionaries.
#'
#' @param data dataframe object
#' @param x text column to pull sentiment from
#'
#' @return summary sentiment scores for all available sentiment lexicon dictionaries.
#' @export
#'
#' @examples
#' \dontrun{
#' janeaustenr::austen_books() |>
#'   dplyr::group_by(book) |>
#'   summarise_sentiment(text)
#' }
#'

summarise_sentiment <- function(data,
                                x) {

  .base <-
    data |>
    tidytext::unnest_tokens(word, {{ x }})

  .groups <-
    dplyr::group_vars(data)

  ## Wordcount
  .wordcount <-
    .base |>
    dplyr::summarise(wordcount = n(),
                     .groups = 'drop')

  ## Bing library
  .bing <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('bing'),
                      by = 'word')

  .wordcount_bing <-
    .bing |>
    dplyr::group_by(word, sentiment,
                    .add = TRUE) |>
    dplyr::summarise(.n = max(dplyr::row_number()),
                     .groups = 'keep') |>
    dplyr::group_by(dplyr::across(dplyr::setdiff(purrr::map_chr(dplyr::groups(.base),
                                                                rlang::quo_text),
                                                 purrr::map_chr(dplyr::quos("word", "sentiment"),
                                                                rlang::quo_text)))) |>
    dplyr::distinct(word, .n) |>
    dplyr::summarise(wordcount_bing = sum(.n),
                     .groups = 'drop')

  .bing <-
    .bing |>
    dplyr::count(sentiment) |>
    tidyr::pivot_wider(names_from = 'sentiment',
                       values_from = n) |>
    tidyr::replace_na(list(positive = 0, negative = 0)) |>
    dplyr::rename(sent_bing_positive = positive,
                  sent_bing_negative = negative) |>
    dplyr::mutate(sent_bing_total = sent_bing_positive - sent_bing_negative) |>
    dplyr::ungroup()

  ## Loughran library
  .loughran <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('loughran'),
                      by = 'word')

  .wordcount_loughran <-
    .loughran |>
    dplyr::group_by(word, sentiment,
                    .add = TRUE) |>
    dplyr::summarise(.n = max(dplyr::row_number()),
                     .groups = 'keep') |>
    dplyr::group_by(dplyr::across(dplyr::setdiff(purrr::map_chr(dplyr::groups(.base),
                                                                rlang::quo_text),
                                                 purrr::map_chr(dplyr::quos("word", "sentiment"),
                                                                rlang::quo_text)))) |>
    dplyr::distinct(word, .n) |>
    dplyr::summarise(wordcount_loughran = sum(.n),
                     .groups = 'drop')

  .loughran <-
    .loughran|>
    dplyr::count(sentiment) |>
    tidyr::pivot_wider(names_from = 'sentiment',
                       values_from = n) |>
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                dplyr::coalesce, 0)) |>
    dplyr::rename_with(~ paste0('sent_loughran_', .),
                       !.groups) |>
    dplyr::mutate(sent_loughran_total = sent_loughran_positive - sent_loughran_negative) |>
    dplyr::ungroup()

  ## AFINN library
  .afinn <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('afinn'),
                      by = 'word') |>
    ## Don't think this is possible, but just in case....
    dplyr::filter(value != 0) |>
    dplyr::mutate(sentiment = ifelse(value < 0,
                                     "sent_afinn_negative",
                                     "sent_afinn_positive"),
                  value = ifelse(value < 0,
                                 -value,
                                 value))

  .wordcount_afinn <-
    .afinn |>
    dplyr::group_by(word, sentiment,
                    .add = TRUE) |>
    dplyr::summarise(.n = max(dplyr::row_number()),
                     .groups = 'keep') |>
    dplyr::group_by(dplyr::across(dplyr::setdiff(purrr::map_chr(dplyr::groups(.base),
                                                                rlang::quo_text),
                                                 purrr::map_chr(dplyr::quos("word", "sentiment"),
                                                                rlang::quo_text)))) |>
    dplyr::distinct(word, .n) |>
    dplyr::summarise(wordcount_afinn = sum(.n),
                     .groups = 'drop')

  .afinn <-
    .afinn |>
    dplyr::count(sentiment,
                 wt = value) |>
    tidyr::pivot_wider(names_from = sentiment,
                       values_from = n) |>
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                dplyr::coalesce, 0)) |>
    dplyr::mutate(sent_afinn_total = sent_afinn_positive - sent_afinn_negative)

  ## NRC library
  .nrc <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('nrc'),
                      by = 'word')
  .wordcount_nrc <-
    .nrc |>
    dplyr::group_by(word, sentiment,
                    .add = TRUE) |>
    dplyr::summarise(.n = max(dplyr::row_number()),
                     .groups = 'keep') |>
    dplyr::group_by(dplyr::across(dplyr::setdiff(purrr::map_chr(dplyr::groups(.base),
                                                                rlang::quo_text),
                                                 purrr::map_chr(dplyr::quos("word", "sentiment"),
                                                                rlang::quo_text)))) |>
    dplyr::distinct(word, .n) |>
    dplyr::summarise(wordcount_nrc = sum(.n),
                     .groups = 'drop')

  .nrc <-
    .nrc |>
    dplyr::count(sentiment) |>
    dplyr::mutate(sentiment = paste0("sent_nrc_", sentiment)) |>
    tidyr::pivot_wider(names_from = sentiment,
                       values_from = n) |>
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                dplyr::coalesce, 0)) |>
    dplyr::mutate(sent_nrc_total = sent_nrc_positive - sent_nrc_negative)


  if (length(.groups) > 0) {
    .output <-
      .wordcount |>
      dplyr::full_join(.bing |>
                         dplyr::left_join(.wordcount_bing,
                                          by = .groups),
                       by = .groups) |>
      dplyr::full_join(.loughran |>
                         dplyr::left_join(.wordcount_loughran,
                                          by = .groups),
                       by = .groups) |>
      dplyr::full_join(.afinn |>
                         dplyr::left_join(.wordcount_afinn,
                                          by = .groups),
                       by = .groups) |>
      dplyr::full_join(.nrc |>
                         dplyr::left_join(.wordcount_nrc,
                                          by = .groups),
                       by = .groups) |>
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                  dplyr::coalesce, 0))
  } else {
    .output <-
      .wordcount  |>
      dplyr::bind_cols(.bing |>
                         dplyr::bind_cols(.wordcount_bing)) |>
      dplyr::bind_cols(.loughran |>
                         dplyr::bind_cols(.wordcount_loughran)) |>
      dplyr::bind_cols(.afinn |>
                         dplyr::bind_cols(.wordcount_afinn)) |>
      dplyr::bind_cols(.nrc |>
                         dplyr::bind_cols(.wordcount_nrc)) |>
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                  dplyr::coalesce, 0))
  }

  dplyr::select(.output,
                tidyselect::all_of(.groups),
                tidyselect::ends_with('_total'),
                tidyselect::starts_with('wordcount_'),
                sent_bing_positive, sent_bing_negative,
                sent_loughran_positive, sent_loughran_negative,
                tidyselect::starts_with('sent_loughran'),
                sent_afinn_positive, sent_afinn_negative,
                sent_nrc_positive, sent_nrc_negative,
                tidyselect::starts_with('sent_nrc'))

}


#' @rdname summarise_sentiment
#' @export
summarize_sentiment <- summarise_sentiment
