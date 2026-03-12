#' Retrieve theme columns
#'
#' @inheritParams plot_theme
#'
#' @return the column indices of the questions relating to the theme
#'
get_theme_columns <- function(data, theme){
  theme_columns = NA
  if(!is.na(theme)){
    theme_columns = grep(paste0(theme,collapse = '|') |> stringr::str_replace_all(' ','_'), names(data))
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


#' Convert "N-A" to NA across a data frame
#'
#' @param data a data frame
#'
#' @export
#'
#' @details
#' Loops over all columns and changes "N-A" to NA.
#'
#'
#' @examplesIf FALSE
#' OMESurvey::survey_example |> convert_NA()
#'
#'
convert_NA <- function(data){
  for(i in 1:ncol(data)){
    data[[i]][data[[i]] == 'N-A'] = NA
  }
  data
}

#' Explain how to set the OME SharePoint root folder
#'
#' Prints instructions for setting the `OME_SHAREPOINT_ROOT` environment
#' variable in `.Renviron`. This should point to the user's local synced
#' SharePoint / OneDrive root folder, for example
#' `"C:/Users/yourID/OneDrive - Company"`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' set_OME_sharepoint_root()
#' }
#'
#' @export
set_OME_sharepoint_root <- function() {
  cli::cli_h2("To set your OME SharePoint root folder, follow these steps:")
  cli::cli_ol()
  cli::cli_li("Open and edit your .Renviron file (for example, run {.run usethis::edit_r_environ()})")
  cli::cli_li("Add a line like the following, replacing with your own synced root path:")
  cli::cli_code('OME_SHAREPOINT_ROOT="C:/Users/yourID/OneDrive - Company" (Template Windows root)')
  cli::cli_li("Restart R")
  cli::cli_end()

  invisible(NULL)
}


#' Check SharePoint root
#'
#' Internal helper to retrieve and validate the SharePoint root path.
#'
#' @param x Optional character string giving the SharePoint root directly.
#'   If `NULL`, the value is taken from `Sys.getenv("OME_SHAREPOINT_ROOT")`.
#'
#' @return A length-1 character string giving the SharePoint root path.
#'
#' @noRd
check_sharepoint_root <- function(x = NULL) {
  tmp <- x

  if (is.null(tmp)) {
    tmp <- Sys.getenv("OME_SHAREPOINT_ROOT", unset = "")
  }

  if (!is.character(tmp) || length(tmp) != 1 || !nzchar(tmp)) {
    stop(
      "OME_SHAREPOINT_ROOT is not set correctly. ",
      "Please add it to your .Renviron file.",
      call. = FALSE
    )
  }

  tmp
}


#' Build a path within the OME SharePoint root
#'
#' Constructs a file path relative to the user's synced SharePoint / OneDrive
#' root folder stored in the `OME_SHAREPOINT_ROOT` environment variable.
#'
#' This allows scripts to use shared relative paths rather than hard-coded
#' user-specific absolute paths.
#'
#' @param ... Path components to append to the SharePoint root, passed to
#'   [base::file.path()].
#' @param root Optional character string giving the SharePoint root directly.
#'   If `NULL`, the value is taken from the `OME_SHAREPOINT_ROOT` environment
#'   variable.
#' @param must_exist Logical. If `TRUE`, an error is thrown if the resulting
#'   path does not exist. Default is `FALSE`.
#'
#' @return A character string giving the full file path.
#'
#' @examples
#' \dontrun{
#' sp_path("Folder", "file.csv")
#' sp_path("ProjectA", "Data", "raw_data.csv", must_exist = TRUE)
#' }
#'
#' @export
sp_path <- function(..., root = NULL, must_exist = FALSE) {
  root <- check_sharepoint_root(root)
  out <- base::file.path(root, ...)

  if (isTRUE(must_exist) && !base::file.exists(out)) {
    stop("Path does not exist: ", out, call. = FALSE)
  }

  out
}
