# Read, merge, validate, and prepare survey data in one step

Convenience wrapper that performs the full survey data preparation
pipeline by combining
[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md)
and
[`survey_data_prepare`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md).
This includes reading input files, merging establishment characteristics
(if supplied), validating and coercing variables according to the data
dictionary, and returning both processed data and diagnostic outputs.

## Usage

``` r
survey_prepare_data(
  data_path,
  dict_path,
  dict_sheet,
  est_chars_path = NULL,
  est_chars_sheet = NULL,
  est_char_vars = NULL,
  est_char_types = NULL,
  est_char_values = NULL,
  est_char_statements = NULL
)
```

## Arguments

- data_path:

  Character string. Path to the survey data file (csv, xls, or xlsx).

- dict_path:

  Character string. Path to the data dictionary file (xls or xlsx).

- dict_sheet:

  Character string. Name of the sheet within the data dictionary file to
  use.

- est_chars_path:

  Optional character string. Path to the establishment characteristics
  file (xls or xlsx). If `NULL` (default) no additional data are merged.

- est_chars_sheet:

  Optional character string. Name of the sheet within the establishment
  characteristics file to use. Required if `est_chars_path` is supplied.

- est_char_vars:

  Optional character vector. Names of establishment characteristics
  variables to include.

- est_char_types:

  Optional character vector. Data types for establishment
  characteristics variables (same format as dictionary).

- est_char_values:

  Optional character vector. Allowed values for establishment
  characteristics variables (as in the dictionary).

- est_char_statements:

  Optional character vector. Descriptive statements/labels for
  establishment characteristics variables.

## Value

A list with components:

- data:

  A tibble containing the fully prepared survey data, including merged
  establishment characteristics (if supplied) and variables coerced to
  their specified types.

- validation_log:

  A list summarising validation checks applied to each variable.

- validation_df:

  A tibble made as `dplyr::bind_rows(validation_log)` (easier to access
  for some purposes).

- read_messages:

  A list of message objects generated during input reading and merging.

- prep_messages:

  A list of message objects generated during validation and coercion.

## See also

[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md),
[`survey_data_prepare`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md)
