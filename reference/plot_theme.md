# plot survey theme result

plot survey theme result

## Usage

``` r
plot_theme(
  data,
  theme = NA,
  theme_columns = NA,
  kind = "ggplot",
  rm99 = TRUE,
  survey_values = OMESurvey::survey_values,
  do_percent = FALSE
)
```

## Arguments

- data:

  survey data

- theme:

  A string containing the theme name (with or without "\_" as spacing).

- theme_columns:

  column index relating to theme. This input is overwritten by the
  columns found by parameter 'theme' if inputte.

- kind:

  what kind of object we want to be returned. Allowable values are
  "ggplot", "plotly" or "data.frame".

- rm99:

  Flag (TRUE/FALSE) for whether we want to set '99' values to NA to
  suppress multiple values as an option. Default is TRUE.

- survey_values:

  A list of survey answer types and their allowble values. Default is
  OMESurvey::survey_values.

- do_percent:

  Flag (TRUE/FALSE) for whether we want to show the number of
  responses (F) or the percentage of responses (T).

## Value

either a plotly object, ggplot object or a data frame depending on
`kind`.

## Details

See `vignette("Create-survey-figures", package = "OMESurvey")` article
for further examples.

## Examples

``` r
if (FALSE) {
# Load in some example survey data.
data = OMESurvey::survey_example

# Create the theme plot.
p = OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')

}
```
