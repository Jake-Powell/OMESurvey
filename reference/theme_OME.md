# OME ggplot2 theme

OME house style theme for `ggplot2`, based on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Usage

``` r
theme_OME(base_size = 16, legend_placement = c("right", "bottom"))
```

## Arguments

- base_size:

  Numeric. Base font size for the theme. Defaults to 16.

- legend_placement:

  Character. Controls position/orientation/etc of legends. One of
  `"right"` or `"bottom"`. Defaults to `"right"`.

## Value

A `ggplot2` theme object that can be added to a ggplot with `+`.

## Author

Dave Sirl

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = "A plot with non-OME-relevant data,",
      subtitle = "but which demos using theme_OME"
      )

  p + theme_OME()

  p + theme_OME(legend_placement = "bottom")

  # Set as the global default for the current session:
  ggplot2::theme_set(theme_OME())
}
```
