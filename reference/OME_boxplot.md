# Boxplot (horizontal) with optional right-side counts showing valid & total \#datapoints for boxes, in an OME style.

Boxplot (horizontal) with optional right-side counts showing valid &
total \#datapoints for boxes, in an OME style.

## Usage

``` r
OME_boxplot(
  data,
  value_var,
  group_var,
  show_counts = TRUE,
  title = NULL,
  colour = OMESurvey::get_OME_colours(1),
  dashed_at = NULL
)
```

## Arguments

- data:

  A data frame.

- value_var:

  Numeric variable to plot.

- group_var:

  Grouping variable (factor).

- show_counts:

  Logical; whether to display (valid/total) counts on the right-hand
  axis. Defaults to TRUE.

- title:

  Optional plot title.

- colour:

  Boxplot outline colour (any valid R colour). Defaults to
  [get_OME_colours](https://jake-powell.github.io/OMESurvey/reference/1).

- dashed_at:

  Optional horizontal reference line.

## Value

A ggplot object

## Note

Future/possible extensions include support for dodging/colour (and maybe
vertical-ness and/or faceting).

## Author

Dave Sirl

## Examples

``` r
df <- tibble::tibble(
  group = factor(c("A", "A", "B", "B")),
  value = c(3.2, 4.1, 5.0, NA)
)

OME_boxplot(
  data = df,
  value_var = value,
  group_var = group,
  title = "Example boxplot"
)
```
