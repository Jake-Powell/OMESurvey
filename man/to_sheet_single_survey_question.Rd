% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_xlsx.R
\name{to_sheet_single_survey_question}
\alias{to_sheet_single_survey_question}
\title{Add response to single survey question}
\usage{
to_sheet_single_survey_question(
  data,
  question_column,
  demographic_columns,
  append_to = NULL,
  wb = NULL,
  sheet = "Data"
)
}
\arguments{
\item{data}{survey data, formatted where each row is an individual and column names describe demographics or survey questions (of the format YY__ZZ, where XX is the theme and YY the question, where ' ' are replaced with '_' and '?' with 'XX'.)}

\item{question_column}{the column index of the question within data}

\item{demographic_columns}{column indices of demographics used to filter the data (e.g. sex, etc)}

\item{append_to}{Flag (TRUE/FALSE) for whether the summary sheet is appended to a previous workbook}

\item{wb}{the workbook to append the sheet to (requires append_to = TRUE)}

\item{sheet}{the sheet name}
}
\value{
returns openxlsx workbook with new sheet added
}
\description{
Add response to single survey question
}
