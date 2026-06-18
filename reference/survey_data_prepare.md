# Validate and coerce survey data according to a data dictionary

Applies validation and coercion rules to survey data using a supplied
data dictionary. This includes enforcing data types, applying allowed
values, handling conditional logic, and producing a structured
validation log.

## Usage

``` r
survey_data_prepare(data, dict, extra_vars = c("keep", "suffix_asis", "drop"))
```

## Arguments

- data:

  A tibble containing the survey data, typically the `data` component
  returned by
  [`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md).
  This may already include merged establishment characteristics.

- dict:

  A tibble containing the data dictionary, typically the `dict`
  component returned by
  [`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md).

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

  A tibble containing the processed survey data, with variables coerced
  to their specified types (e.g. factors, numeric) and with allowed
  values enforced. See Details.

- validation:

  A tibble summarising validation checks applied to each variable,
  including counts of values meeting or failing specified conditions.

- messages:

  A list of message objects describing any issues encountered during
  validation or coercion (e.g. missing variables, invalid values, failed
  conditions).

- extra_vars:

  Character vector of source-data variables not present in the
  dictionary.

- extra_vars_mode:

  The value of `extra_vars` used when preparing the returned data.

- extra_var_map:

  Named character vector mapping original extra-variable names to their
  returned names; dropped variables have `NA` values.

## Details

This function operates on in-memory data and a dictionary, usually the
output from
[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md).
That function reads files and validates the dictionary, this one uses
the dictionary to transform & validate the data.

In the returned `data`:

- Dictionary-backed variables are returned under their dictionary names
  as prepared/coerced variables.

- For each dictionary-backed variable `x`, a companion variable `x_raw`
  is created containing the raw/original values before
  preparation/coercion.

- For establishment-characteristic variables that are numeric before
  being binned to factors based on x-iles of national data, the
  pre-binning numeric values are retained as `x_numeric`.

- Variables present in the source data but absent from the dictionary
  are handled according to `extra_vars`: renamed to `x_asis`, kept
  unchanged, or dropped.

## See also

[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md)
