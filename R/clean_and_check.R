#' Check for issues in the survey data
#'
#' @description
#' Functions to check the inital survey data for issues such as unexpected answers to questions or questions with multiple answers.
#'
#' @details
#' - check_survey() checks survey data for issues with barcodes, unexpected values and questions with multiple answers (i.e "99"). Uses check_question_columns() to genereate unexpected values and location "99"s.
#'
#' - check_question_columns() checks given columns for unexpected values where the expected values are auto-detected by comparison to `survey_values`.
#'
#' See `Preparation and initial survey analysis` vignette for example.
#'
#'
#' @inheritParams to_sheet_single_survey_question
#' @param ID_column the name of the ID column. Default is "Barcode_ID".
#' @param question_columns The index of the question columns.
#' @param survey_values A list of possible survey question answers by default it takes the values from `OMESurvey::survey_values`.
#' @param format Either 'list' where the result for each question is another element in a list or 'table' which concatenates all the results into a single table.
#' @param exclude_99 Logical (TRUE/FALSE) for whether to exclude '99' from the non-expected values.
#' @param first_name_column the name of the "First Name' column. Default is "First_Name".
#' @param surname_column the name of the "Surname' column. Default is "Surname".
#' @param true_df a data frame including the columns `ID_column`, `first_name_column`, `surname_column` which contain the correct ID, first name and surname combinations.
#' @param ... parameters for downstream functions.
#' @export
#'
check_question_columns <- function(data,
                                   question_columns,
                                   survey_values = OMESurvey::survey_values,
                                   format = 'table',
                                   exclude_99 = TRUE){
  data = data |> as.data.frame()
  out = lapply(question_columns, function(column_index){
    values = data[,column_index] ;  # values = values[!is.na(values)]
    if(exclude_99){
      values[values == '99'] = NA
    }
    unique_values = values |> unique() ; unique_values = unique_values[!is.na(unique_values)]

    # Generate the expected values if not given.
    best_index = lapply(survey_values, function(x){
      return(sum(x %in% unique_values))
    }) |> unlist() |> which.max()
    expected_values = survey_values[[best_index[1]]]


    non_expected = unique_values[!unique_values %in% expected_values]

    if(length(non_expected) == 0){return(NULL)}

    # Find count and row numbers of offending values
    issue = lapply(non_expected, function(name){
      count = sum(values == name,na.rm = T)
      index = which(values == name) |> paste0(collapse = ', ')
      return(c(count, index))
    }) |> data.frame() |> t() |> data.frame()

    issue = data.frame(non_expected, issue)
    names(issue) = c('Unexpected value', 'Count', 'Indices') ; rownames(issue) = 1:nrow(issue)
    return(list(expected = expected_values, unexpected = issue))
  })
  names(out) = names(data)[question_columns]

  if(format == 'full'){
    return(out)
  }

  out_new = lapply(names(out), function(q){
    if(is.null(out[[q]]$unexpected)){
      return(NULL)
    }
    table_cur = data.frame(question = q, out[[q]]$unexpected, expected = paste0(out[[q]]$expected, collapse = ', ') )
  })
  out_new =  do.call(rbind, out_new)

  return(out_new)
}


#' @rdname check_question_columns
#' @export
#'
check_survey <- function(data,
                         ID_column = 'Barcode_ID',
                         ...){
  # 1) Check for any issues with the barcode.
  IDs = data[[ID_column]]
  wrong_length = which(IDs |> stringr::str_length() != 10)

  if(length(wrong_length) == 0){
    bar_iss = list(isIssue = FALSE, issues = NULL)
  }else{
    issues = data.frame(index = wrong_length, ID = IDs[wrong_length], rep('ID length != 10', length(wrong_length)))
    bar_iss = list(isIssue = TRUE, issues = issues)
  }

  # 2) Check for any non-expected values in the question answers.
  if(!exists('question_columns')){
    question_columns = stringr::str_detect(names(data),pattern = '__') |> which()
  }
  question_issues = check_question_columns(data = data,
                         question_columns = question_columns,
                         exclude_99 = FALSE
                         )

  v99_locations = question_issues[question_issues$Unexpected.value == '99',]
  question_issues = question_issues[question_issues$Unexpected.value != '99',]

  if(nrow(question_issues) > 0){
    quest_iss = list(isIssue = TRUE, issues = question_issues)
  }else{
    quest_iss = list(isIssue = FALSE, issues = NULL)
  }

  if(nrow(v99_locations) > 0){
    barcodes_index = v99_locations$Indices |> stringr::str_split(', ') |> unlist() |> unique() |> as.numeric()
    barcodes = data[[ID_column]][barcodes_index]
    v99_iss = list(isIssue = TRUE, issues = v99_locations, barcodes = barcodes)
  }else{
    v99_iss = list(isIssue = FALSE, issues = NULL)
  }

  # 3) Return all issues
  return(list(barcode = bar_iss,
              question_text = quest_iss,
              multiple_answers = v99_iss))
}

#' @rdname check_question_columns
#' @export
#'
check_IDs <- function(data,
                      ID_column = 'Barcode_ID',
                      first_name_column = "First_Name",
                      surname_column = "Surname",
                      true_df = NA
){
  IDs = data[[ID_column]]
  wrong_length = which(IDs |> stringr::str_length() != 10)
  IDs_length_issue = IDs[wrong_length]
  if(length(IDs_length_issue) == 0){
    IDs_length_issue = NA
  }
  if(!is.na(true_df[1])[1]){
    true_combo = paste0(true_df[[ID_column]], '---',
                        true_df[[first_name_column]], '---',
                        true_df[[surname_column]]
    )
    data_combo = paste0(data[[ID_column]], '---',
                        data[[first_name_column]], '---',
                        data[[surname_column]]
    )

    issue_index = which(!data_combo %in% true_combo)

    issue_df = lapply(issue_index, function(index){
      combo = data_combo[index]
      split = data_combo[index] |> stringr::str_split('---') |> unlist()
      ID = split[1] |> as.numeric() ; FN = split[2] ; SN = split[3]

      matched_ID_index = match(ID, true_df[[ID_column]])
      found_FN = true_df[[first_name_column]][matched_ID_index]
      found_SN = true_df[[surname_column]][matched_ID_index]

      matched_name_index = match(paste0(FN,'---',SN), paste0(true_df[[first_name_column]], '---',
                                                       true_df[[surname_column]]))
      found_IDs = paste0(true_df[[ID_column]][matched_name_index],collapse = ', ')
      if(found_IDs == 'NA'){found_IDs = NA}

      return(c(ID, FN, SN, found_IDs, found_FN, found_SN))
      }) |> data.frame() |> t() |> data.frame()
    rownames(issue_df) = 1:nrow(issue_df) ; names(issue_df) = c('Data: ID', "Data: First Name", 'Data: Surname',
                                                                'Potential:  ID', "Potential: First Name", 'Potential: Surname')
  }
  else{
    issue_df = 'Not checked'
  }

  return(list(length_issue = IDs_length_issue,
             inconsistent_issue = issue_df))


}


#' Factor survey questions in data
#'
#' @param data survey data
#' @param survey_values a list of allowed grouped survey values. Default is OMESurvey::survey_values.
#' @param verbose Flag (TRUE/FALSE) for whether to print console messages.
#'
#' @return data with factored survey columns
#' @export
#'
#' @examplesIf FALSE
#' data = OMESurvey::survey_example
# for(i in 1:ncol(data)){
#   data[[i]][which(data[[i]] == 'N-A')] = NA
# }
# clean = data |> factor_survey_data()

#'
factor_survey_data <- function(data, survey_values = OMESurvey::survey_values, verbose = T){
  data = data |> as.data.frame() # Tibbles can be annoying.
  for(i in 1:ncol(data)){
    values = data[[i]]
    unique_values = values |> unique() ; unique_values = unique_values[!is.na(unique_values)]

    best_index = lapply(survey_values, function(x){
      return(sum(x %in% unique_values))
    }) |> unlist() |> which.max()
    expected_values = survey_values[[best_index[1]]]
    non_expected = unique_values[!unique_values %in% expected_values]


    if(length(non_expected) == 0){
      if(verbose) cli::cli_alert_info(paste0('Variable: `',
                                             names(data)[i],
                                             '` is found within question group `',
                                             names(survey_values)[best_index[1]],
                                             '` Set to factor.'))
      data[[i]] = factor(data[[i]], expected_values)
      next
    }

    if(verbose) cli::cli_alert_info(paste0('Variable: `',
                                           names(data)[i],
                                           '` is not found entirely within a question group. '))


  }
  return(data)
}

