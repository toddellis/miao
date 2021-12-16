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
                      by = 'word') |>
    dplyr::count(sentiment) |>
    dplyr::mutate(n = ifelse(sentiment == 'negative',
                             -n,
                             n)) |>
    dplyr::summarise(sent_bing = sum(n),
                     .groups = 'drop')

  ## Loughran library
  .loughran <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('loughran'),
                      by = 'word') |>
    dplyr::count(sentiment) |>
    dplyr::mutate(n = ifelse(sentiment == 'negative',
                             -n,
                             n)) |>
    dplyr::summarise(sent_loughran = sum(n),
                     .groups = 'drop')

  ## AFINN library
  .afinn <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('afinn'),
                      by = 'word') |>
    dplyr::summarise(sent_afinn = sum(value),
                     .groups = 'drop')

  ## NCR library
  .nrc <-
    .base |>
    dplyr::inner_join(tidytext::get_sentiments('nrc'),
                      by = 'word') |>
    dplyr::count(sentiment) |>
    dplyr::mutate(n = ifelse(sentiment == 'negative',
                             -n,
                             n)) |>
    dplyr::summarise(sent_nrc = sum(n[sentiment %in% c('positive', 'negative')]),
                     sent_nrc_anger = sum(n[sentiment == 'anger']),
                     sent_nrc_anticipation = sum(n[sentiment == 'anticipation']),
                     sent_nrc_disgust = sum(n[sentiment == 'disgust']),
                     sent_nrc_fear = sum(n[sentiment == 'fear']),
                     sent_nrc_joy = sum(n[sentiment == 'joy']),
                     sent_nrc_sadness = sum(n[sentiment == 'sadness']),
                     sent_nrc_surprise = sum(n[sentiment == 'surprise']),
                     sent_nrc_trust = sum(n[sentiment == 'trust']),
                     .groups = 'drop')


  if (length(.groups) > 0) {
    .wordcount |>
      dplyr::full_join(.bing,
                       by = .groups) |>
      dplyr::full_join(.loughran,
                       by = .groups) |>
      dplyr::full_join(.afinn,
                       by = .groups) |>
      dplyr::full_join(.nrc,
                       by = .groups) |>
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                  dplyr::coalesce, 0))
  } else {
    .wordcount  |>
      dplyr::bind_cols(.bing) |>
      dplyr::bind_cols(.loughran) |>
      dplyr::bind_cols(.afinn) |>
      dplyr::bind_cols(.nrc)
  }


}


#' @rdname summarise_sentiment
#' @export
summarize_sentiment <- summarise_sentiment
