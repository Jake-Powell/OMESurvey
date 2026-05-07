# ROME ggplot2 theme

An OME ggplot2 theme (based on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)).

## Usage

``` r
theme_OME(base_size = 16)
```

## Arguments

- base_size:

  Numeric. Base font size for the theme. Defaults to 16.

## Value

A `ggplot2` theme object that can be added to a ggplot with `+`.

## Details

!!! CURRENTLY COPIED FROM JAKE'S ROME_ggtheme(), with some-but-very-few
tweaks !!!

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::labs(title = "ROME theme demo")

  # Add the theme to a single plot:
  p + theme_OME()

  # Set as the global default for the current session:
  ggplot2::theme_set(theme_OME())
}
```
