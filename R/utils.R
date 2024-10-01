#' Retrieve theme columns
#'
#' @inheritParams plot_theme
#'
#' @return the column indices of the questions relating to the theme
#'
get_theme_columns <- function(data, theme){
  theme_columns = NA
  if(!is.na(theme)){
    theme_columns = grep(theme |> stringr::str_replace_all(' ','_'), names(data))
  }
  if(is.na(theme_columns[1])){
    stop('Theme is not correctly specified.')
  }
  return(theme_columns)
}


#' Extract theme and questions from column names
#'
#' @param  theme_and_questions column names of a single theme and it's respective questions.
#'
#' @return a list of length 2, where
#' - $theme string of the theme name
#' - $questions vector of strings with the questions
#'
get_theme_and_questions <- function(theme_and_questions){
  theme_questions = theme_and_questions |> stringr::str_split('__')
  theme = lapply(theme_questions, function(x){x[[1]]}) |>
    unlist()  |>
    stringr::str_replace_all('_', ' ') |>
    unique()
  no_theme = theme |> length()

  if(no_theme > 1){
    stop(paste0('"theme_columns" implies multiple themes. These are: ', paste0(theme, collapse = ', ')) )
  }

  questions = lapply(theme_questions, function(x){x[[2]]}) |>
    unlist() |>
    stringr::str_replace_all('_', ' ')
  return(list(theme = theme, questions = questions))
}
