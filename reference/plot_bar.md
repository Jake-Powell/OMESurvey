# Plot OME bar plot

Plot OME bar plot

## Usage

``` r
plot_bar(
  data,
  column,
  type = "horizontal",
  kind = "ggplot",
  rm99 = TRUE,
  showValue = FALSE,
  ...
)
```

## Arguments

- data:

  survey data

- column:

  column index or name to select the data.

- type:

  Either 'vertical' or 'horizontal'. Default is 'horizontal'.

- kind:

  what kind of object we want to be returned. Allowable values are
  "ggplot", "plotly" or "data.frame".

- rm99:

  Flag (TRUE/FALSE) for whether we want to set '99' values to NA to
  suppress multiple values as an option. Default is TRUE.

- showValue:

  Flag (TRUE/FALSE) for whether to display the values on the plot.

- ...:

  parameters for table(). I.e whether you want to include NA or not by
  using useNA = 'always' or 'ifany'.

## Value

either a data.frame, ggplot object or plotly object.

## Examples

``` r
if (FALSE) {
# Load in some example survey data and create a plot.
data = OMESurvey::survey_example

# Create bar plot for column 5.
OMESurvey::plot_bar(data, column = 5, kind = 'ggplot')

}
```
