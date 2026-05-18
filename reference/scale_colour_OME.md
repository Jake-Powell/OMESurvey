# OME colour scale

Convenience wrapper around
[`ggplot2::scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
using the standard OME colour palette.

## Usage

``` r
scale_colour_OME(type = "distinct", ...)
```

## Arguments

- type:

  Character string specifying which palette to use. Passed to
  [`get_OME_colours`](https://jake-powell.github.io/OMESurvey/reference/get_OME_colours.md);
  see that function for available options. Default is `"distinct"`.

- ...:

  Additional arguments passed to `scale_colour_manual()`.

## Value

A ggplot2 scale for colours.

## Examples

``` r
# Scatterplot coloured by number of cylinders using OME palette
ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_OME() +
  theme_OME() +
  labs(
    x = "Weight",
    y = "Miles per gallon",
    colour = "Cylinders",
    title = "Fuel efficiency vs weight, coloured by cylinders"
  )
#> Error in ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))): could not find function "ggplot"
```
