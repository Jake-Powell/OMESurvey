# Plot survey theme result by demographic

Plot survey theme result by demographic

## Usage

``` r
plot_theme_by_demographic(
  data,
  theme = NA,
  theme_columns = NA,
  demographic_column,
  agree_values = c("Agree a little", "Agree a lot"),
  kind = "ggplot",
  rm99 = TRUE
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

- demographic_column:

  the index of the demographic column.

- agree_values:

  The answers that correspond to agree.

- kind:

  what kind of object we want to be returned. Allowable values are
  "ggplot", "plotly" or "data.frame".

- rm99:

  Flag (TRUE/FALSE) for whether we want to set '99' values to NA to
  suppress multiple values as an option. Default is TRUE.

## Value

either a plotly object, ggplot object or a data frame depending on
`kind`.

## Details

See `Create survey figures` article for examples.

## Examples

``` r
if (FALSE) {
# Load in some example survey data.
data = OMESurvey::survey_example

# Add a school column using the barcode.
school = data$Barcode_ID |> as.character() |>  substr(3, 5)
data$school = school
school_column = ncol(data)

# Create the theme by demographic plot.
OMESurvey::plot_theme_by_demographic(data,
theme = 'Theme_1',
demographic_column = school_column,
kind = 'ggplot')

}
```
