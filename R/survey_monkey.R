#' survey_monkey
#'
#' Clean (most) Survey Monkey format data, barring overly-complex and poorly-constructed surveys (insert tears).
#'
#' @param x Dataframe of raw Survey Monkey dataset.
#' @param pivot Logical determinant of whether to pivot the dataset from long format (default) to wide format data.
#'
#' @return Dataframe of cleaned (or at least more manageable) Survey Monkey data.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#'

survey_monkey = function(survey,
                         pivot = FALSE) {

  id_cols = c('Respondent ID',
              'Collector ID',
              'Start Date',
              'End Date',
              'IP Address',
              'Email Address',
              'First Name',
              'Last Name',
              'Custom Data 1')

  ## Rename Respondent ID to RID
  # survey_cols = colnames(survey)
  ## N.B. `Respondent ID` is a default Survey Monkey column.
  # if ('Respondent ID' %in% survey_cols) {
  #   survey <- dplyr::rename(survey,
  #                           RID = `Respondent ID`)
  # } else if ('RID' %in% survey_cols) {
  #
  # } else {
  #   ## Backup plan: Assume column 1 is always the Respondent ID as per the default Survey Monkey output.
  #   survey <- dplyr::rename(survey,
  #                           RID = 1)
  # }

  ## Create a question-answer key.
  survey_key <- survey %>%
    ## Isolate top row (default answer key row)
    dplyr::slice(1) %>%
    dplyr::select(-tidyselect::any_of(id_cols)) %>%
    ## Change all columns to character to avoid collapsing datatype issues
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                as.character)) %>%
    ## Pivot to long format data to collapse the too-many columns.
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = '.colname',
                        values_to = '.key') %>%
    dplyr::mutate(.key = ifelse(is.na(.key),
                                .colname,
                                .key))

  ## Transform and clean (and transform (and clean)) survey data
  output <- survey %>%
    # Remove answer key row from survey data
    dplyr::slice(-1) %>%
    ## Change all columns to character to avoid collapsing datatype issues
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                as.character)) %>%
    ## Pivot to long format data to collapse the too-many columns but retain the Respondent ID
    tidyr::pivot_longer(cols = !tidyselect::any_of(id_cols),
                        names_to = '.colname',
                        values_to = '.response')

  cols_intact = output %>%
    dplyr::select(-c(.colname, .response)) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   dplyr::n_distinct)) %>%
    tidyr::pivot_longer(cols = tidyselect::everything())

  cols_to_remove = cols_intact %>%
    dplyr::filter(value == 1) %>%
    dplyr::pull(name)

  cols_to_keep = cols_intact %>%
    dplyr::filter(value >= 2) %>%
    dplyr::pull(name)

  cols_to_id = cols_intact %>%
    dplyr::filter(value == max(value,
                               na.rm = TRUE)) %>%
    dplyr::pull(name)

  output <- output %>%
    dplyr::select(!tidyselect::any_of(cols_to_remove)) %>%
    ## Join the responses to the answer key
    dplyr::left_join(survey_key,
                     by = c('.colname')) %>%
    ## Remove the default ellipses and numbers reflecting column numbers via Excel columns without any title text
    dplyr::mutate(.colname = stringr::str_replace_all(.colname,
                                                      '\\.{3}[:alnum:]+',
                                                      NA_character_)) %>%
    ## Fill down using the question text
    tidyr::fill(.colname,
                .direction = 'down') %>%
    ## Create a factored question ID to keep the original column order
    dplyr::mutate(.qid = as.numeric(factor(.colname,
                                           levels = unique(.$.colname)))) %>%
    ## Remove empty cells where respondents did not provide an answer
    dplyr::filter(!is.na(.response)) %>%
    ## Separate open answer questions
    ## N.B. This is likely custom to a specific survey and will likely have no effect on most datasets...
    dplyr::mutate(.colname = ifelse(.key == "Any comments?",
                                    stringr::str_c(.colname, ' Any comments?'),
                                    .colname)) %>%
    ## Remove the key column
    dplyr::select(-.key)

  ## Check for multi-part questions or open-answer text.
  if (max(output %>%
          dplyr::group_by(!! dplyr::sym(dplyr::first(cols_to_id)),
                          .colname) %>%
          dplyr::summarise(.n = dplyr::n_distinct(.response),
                           .groups = 'drop') %>%
          dplyr::pull(.n)) >= 2) {

    warning('Multiple responses were found for single respondent-question answers, suggesting multi-part questions or open-text responses. Check unique combinations of questions and responses to find these issues.')

    flag_multi = TRUE

    }

  if (pivot) {

    if (flag_multi == TRUE) {

      warning('Pivoting to wide format with multiple question-response combinations automatically pulls the first answer from each respondent. This typically excludes free-form text responses.')

    }

    output <- output %>%
      dplyr::group_by(!! dplyr::sym(dplyr::first(cols_to_id)),
                      .qid) %>%
      dplyr::slice(1) %>%
      tidyr::pivot_wider(id_cols = tidyselect::any_of(cols_to_keep),
                         names_from = .colname,
                         values_from = .response)

  } else {
    output <- output %>%
      dplyr::select(tidyselect::any_of(cols_to_keep),
                    .qid, .colname, .response)
  }

  output <- output %>%
    dplyr::rename_with(.cols = tidyselect::any_of(cols_to_keep),
                       .fn = ~ stringr::str_replace_all(toupper(.),
                                                        ' ',
                                                        '_'))

  return(output)

}
