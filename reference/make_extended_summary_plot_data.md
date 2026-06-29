# Prepare data for extended summary plots

Constructs the extended data structure required by
[`summary_plot_stacked_bar()`](https://jake-powell.github.io/OMESurvey/reference/summary_plot_stacked_bar.md)
and
[`summary_plot_boxplot()`](https://jake-powell.github.io/OMESurvey/reference/summary_plot_boxplot.md)
from the `data` and `validation_df` outputs of
[`survey_prepare_data()`](https://jake-powell.github.io/OMESurvey/reference/survey_prepare_data.md).

## Usage

``` r
make_extended_summary_plot_data(data, validation_df, vars)
```

## Arguments

- data:

  A data frame containing validated variables and their associated raw
  variables (for example, `q1` and `q1_raw`).

- validation_df:

  Validation metadata data frame containing at least the columns
  `variable`, `condition`, and `allowed_technical`.

- vars:

  Character vector of variable names to process.

## Value

A tibble with one row per row of `data`. For each variable `v` in
`vars`, the output contains three variables:

- `v_value`:

  The variable values.

- `v_include`:

  Logical indicator for inclusion in the denominator.

- `v_plot`:

  Logical indicator for inclusion in plotted counts.

## Details

The returned data frame contains the variable values together with
logical indicators defining which records should be included in
denominators and plotted counts for each variable.

See the package vignette `dave_plotting_functions` for a complete
example showing how this function is used to prepare data for extended
stacked bar plots.

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare survey data
data_prep <- survey_prepare_data(
  "<path_to_data>",
  "<path_to_dictionary>",
  "<sheet_in_dictionary>"
)

data <- data_prep$data
validation_df <- data_prep$validation_df

# Construct the extended plotting data
plot_data <- make_extended_summary_plot_data(
  data,
  validation_df,
  vars = c("q_satisfaction", "q_confidence")
)
} # }
```
