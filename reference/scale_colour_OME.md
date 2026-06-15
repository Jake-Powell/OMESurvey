# OME colour scales for ggplot2

Provides OME palettes for use with colour and fill aesthetics.
(Convenience wrapper around
[`ggplot2::scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
using the standard OME colour palette.)

## Usage

``` r
scale_colour_OME(type = "distinct", ...)

scale_fill_OME(type = "distinct", ...)
```

## Arguments

- type:

  Character string specifying which palette to use. Passed to
  [`get_OME_colours`](https://jake-powell.github.io/OMESurvey/reference/get_OME_colours.md);
  see that function for available options. Default is `"distinct"`.

- ...:

  Additional arguments passed to scale_colour_manual()\`.

## Value

A ggplot2 scale for colours.

## Examples

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Examples here use scale_colour_OME(); scale_fill_OME() is exactly analagous.

# Prepare example dataset with labelled factors
mtcars_small <- mtcars |>
  mutate(
    am = factor(am, levels = c(0, 1), labels = c("Auto", "Man")),
    cyl = factor(cyl, levels = c(4, 6, 8))
  )

# Scatterplot using OME palette & theme for colour aesthetic
# (Not OME-like data and illustration would be better if the factor had more than two levels,
#  but hopefully illustrates use of scale_colour_OME() when colouring by a factor)
mtcars_small |>
  ggplot(aes(x = wt, y = mpg, colour = am)) +
  geom_point() +
  scale_colour_OME() +
  theme_OME() +
  labs(
    x = "Weight (/1,000 lb)",
    y = "Miles per gallon",
    colour = "Transmission",
    title = "Fuel efficiency vs weight,\ncoloured by transmission type"
  )


# Use a sequential palette for an ordered factor ("divergent" is available too)
mtcars_small |>
  ggplot(aes(x = wt, y = mpg, colour = cyl)) +
  geom_point() +
  scale_colour_OME(type = "sequential") +
  theme_OME() +
  labs(
    x = "Weight (/1,000 lb)",
    y = "Miles per gallon",
    colour = "Cylinders",
    title = "Fuel efficiency vs weight,\ncoloured by cylinders"
  )
```
