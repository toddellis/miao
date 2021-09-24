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

  ## Rename Respondent ID to RID
  ## N.B. `Respondent ID` is a default Survey Monkey column.
  if (stringr::str_detect(colnames(survey), 'Respondent ID')) {
    survey <- dplyr::rename(survey,
                            RID = `Respondent ID`)
  } else if (stringr::str_detect(colnames(survey), 'RID')) {
    survey <- survey
  } else {
    ## Backup plan: Assume column 1 is always the Respondent ID as per the default Survey Monkey output.
    survey <- dplyr::rename(survey,
                            RID = 1)
  }

  ## Create a question-answer key.
  survey_key <- survey %>%
    ## Isolate top row (default answer key row)
    dplyr::slice(1) %>%
    ## Change all columns to character to avoid collapsing datatype issues
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                as.character)) %>%
    ## Pivot to long format data to collapse the too-many columns.
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = '.colname',
                        values_to = '.key')

  ## Transform and clean (and transform (and clean)) survey data
  output <- survey %>%
    # Remove answer key row from survey data
    dplyr::slice(-1) %>%
    ## Change all columns to character to avoid collapsing datatype issues
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                as.character)) %>%
    ## Pivot to long format data to collapse the too-many columns but retain the Respondent ID
    tidyr::pivot_longer(cols = -c(RID),
                        names_to = '.colname',
                        values_to = '.response') %>%
    ## Join the responses to the answer key
    dplyr::left_join(survey_key,
                     by = c('.colname')) %>%
    ## Remove the default ellipses and numbers reflecting column numbers via Excel columns without any title text
    dplyr::mutate(.colname =
                    stringr::str_replace_all(.colname,
                                             '\\.{3}[:alnum:]+',
                                             NA_character_)) %>%
    ## Fill down using the question text
    tidyr::fill(.colname,
                .direction = 'down') %>%
    ## Remove empty cells where respondents did not provide an answer
    dplyr::filter(!is.na(.response)) %>%
    ## Separate open answer questions
    ## N.B. This is likely custom to a specific survey and will likely have no effect on most datasets...
    dplyr::mutate(.colname = ifelse(.key == "Any comments?",
                                    str_c(.colname, ' Any comments?'),
                                    .colname))

  ## Check for multi-part questions or open-answer text.
  if (max(output %>%
          group_by(RID, .colname) %>%
          summarise(N = n_distinct(.response)) %>%
          pull(N)) >= 2) {

    warning('Multiple responses were found for single respondent-question answers, suggesting multi-part questions or open-text responses. Check unique combinations of questions and responses to find these issues.')

  }

  if (pivot) {
    output <- output %>%
      tidyr::pivot_wider(id_cols = c(RID),
                         names_from = .colname,
                         values_from = .response)

  }

  return(output)

}
