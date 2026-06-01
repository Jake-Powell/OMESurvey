# Validate and coerce survey data according to a data dictionary

Applies validation and coercion rules to survey data using a supplied
data dictionary. This includes enforcing data types, applying allowed
values, handling conditional logic, and producing a structured
validation log.

## Usage

``` r
survey_data_prepare(data, dict)
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

## Value

A list with components:

- data:

  A tibble containing the processed survey data, with variables coerced
  to their specified types (e.g. factors, numeric) and with allowed
  values enforced.

- validation:

  A tibble summarising validation checks applied to each variable,
  including counts of values meeting or failing specified conditions.

- messages:

  A list of message objects describing any issues encountered during
  validation or coercion (e.g. missing variables, invalid values, failed
  conditions).

## Details

This function operates on in-memory data and a dictionary, usually the
output from
[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md).
That function reads files and validates the dictionary, this one uses
the dictionary to transform & validate the data.

## See also

[`survey_read_inputs`](https://jake-powell.github.io/OMESurvey/reference/survey_read_inputs.md)
