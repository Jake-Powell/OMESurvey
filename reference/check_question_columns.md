# Check for issues in the survey data

Functions to check the inital survey data for issues such as unexpected
answers to questions or questions with multiple answers.

## Usage

``` r
check_question_columns(
  data,
  question_columns,
  survey_values = OMESurvey::survey_values,
  format = "table",
  exclude_99 = TRUE
)

check_survey(data, ID_column = "Barcode_ID", ...)

check_IDs(
  data,
  ID_column = "Barcode_ID",
  first_name_column = "First_Name",
  surname_column = "Surname",
  true_df = NA
)
```

## Arguments

- data:

  survey data, formatted where each row is an individual and column
  names describe demographics or survey questions (of the format
  YY\_\_ZZ, where XX is the theme and YY the question, where ' ' are
  replaced with '\_' and '?' with 'XX'.)

- question_columns:

  The index of the question columns.

- survey_values:

  A list of possible survey question answers by default it takes the
  values from
  [`OMESurvey::survey_values`](https://jake-powell.github.io/OMESurvey/reference/survey_values.md).

- format:

  Either 'list' where the result for each question is another element in
  a list or 'table' which concatenates all the results into a single
  table.

- exclude_99:

  Logical (TRUE/FALSE) for whether to exclude '99' from the non-expected
  values.

- ID_column:

  the name of the ID column. Default is "Barcode_ID".

- ...:

  parameters for downstream functions.

- first_name_column:

  the name of the "First Name' column. Default is "First_Name".

- surname_column:

  the name of the "Surname' column. Default is "Surname".

- true_df:

  a data frame including the columns `ID_column`, `first_name_column`,
  `surname_column` which contain the correct ID, first name and surname
  combinations.

## Details

- check_survey() checks survey data for issues with barcodes, unexpected
  values and questions with multiple answers (i.e "99"). Uses
  check_question_columns() to genereate unexpected values and location
  "99"s.

- check_question_columns() checks given columns for unexpected values
  where the expected values are auto-detected by comparison to
  `survey_values`.

See `Preparation and initial survey analysis` vignette for example.
