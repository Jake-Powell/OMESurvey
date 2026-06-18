# Read, merge, validate, and prepare survey data

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
  est_char_statements = NULL,
  extra_vars = c("suffix_asis", "keep", "drop")
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

- extra_vars:

  Character string controlling how extra variables present in the source
  data but absent from the dictionary are handled in the returned data.
  One of `"suffix_asis"` (default), `"keep"` or `"drop"`.

  - `"suffix_asis"` appends a suffix \_asis to all such variable names

  - `"keep"` keeps such variable names as they are, unchanged

  - `"drop"` drops all such variables from the returned data

## Value

A list with components:

- data:

  A tibble containing the fully prepared survey data, including merged
  establishment characteristics (if supplied) and variables coerced to
  their specified types. Variable naming conventions follow
  [`survey_data_prepare()`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md),
  including the use of `*_raw`, `*_numeric`, and optionally `*_asis`
  suffixes.

- validation_log:

  A list summarising validation checks applied to each variable.

- validation_df:

  A tibble made as `dplyr::bind_rows(validation_log)` (easier to access
  for some purposes).

- read_messages:

  A list of message objects generated during input reading and merging.

- prep_messages:

  A list of message objects generated during validation and coercion.

- extra_vars:

  Character vector of source-data variables not present in the
  dictionary.

- extra_vars_mode:

  The value of `extra_vars` used when preparing the returned data.

- extra_var_map:

  Named character vector mapping original extra-variable names to their
  returned names; dropped variables have `NA` values.

## See also

[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md),
[`survey_data_prepare`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md)
