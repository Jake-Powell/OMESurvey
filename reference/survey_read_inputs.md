# Read and assemble survey data and dictionary inputs

Reads the survey data, data dictionary, and optionally establishment
characteristics files, and prepares them for downstream processing. This
includes merging establishment characteristics onto the main data (if
provided), augmenting the dictionary with corresponding additional
variables, simple dictionary validation.

## Usage

``` r
survey_read_inputs(
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

  A tibble containing the survey data, merged with establishment
  characteristics if supplied.

- dict:

  A tibble containing the data dictionary, augmented with any
  establishment characteristics entries supplied.

- messages:

  A list of message objects describing any issues encountered during
  reading or merging (e.g. missing files, duplicate keys, unmatched
  joins).

## Details

This function performs input and structural preparation of the data and
preparation & validation of the dictionary.
[`survey_data_prepare`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md)
is designed to take the output of this function and validate &
coerce-to-type the data.

The establishment characteristics to be merged into the data are
specified through the variables `est_char_vars`, `est_char_types`,
`est_char_values` and `est_char_statements`. They are vectors with
elements corresponding to the variable name, data type, allowed values,
item statement entries of the data dictionary. See the preprocessing SOP
for details/examples.

## See also

[`survey_data_prepare`](https://jake-powell.github.io/OMESurvey/reference/survey_data_prepare.md)

## Author

Dave Sirl
