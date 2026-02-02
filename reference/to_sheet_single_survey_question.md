# Add response to single survey question

Add response to single survey question

## Usage

``` r
to_sheet_single_survey_question(
  data,
  question_column,
  demographic_columns,
  append_to = NULL,
  wb = NULL,
  sheet = "Data"
)
```

## Arguments

- data:

  survey data, formatted where each row is an individual and column
  names describe demographics or survey questions (of the format
  YY\_\_ZZ, where XX is the theme and YY the question, where ' ' are
  replaced with '\_' and '?' with 'XX'.)

- question_column:

  the column index of the question within data

- demographic_columns:

  column indices of demographics used to filter the data (e.g. sex, etc)

- append_to:

  Flag (TRUE/FALSE) for whether the summary sheet is appended to a
  previous workbook

- wb:

  the workbook to append the sheet to (requires append_to = TRUE)

- sheet:

  the sheet name

## Value

returns openxlsx workbook with new sheet added
