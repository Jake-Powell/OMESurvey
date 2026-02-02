# ROME ggplot2 theme

A clean, minimal theme based on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with custom title, subtitle, legend, strip, and axis styling.

## Usage

``` r
ROME_ggtheme(base_size = 16)
```

## Arguments

- base_size:

  Numeric. Base font size for the theme. Defaults to 16.

## Value

A `ggplot2` theme object that can be added to a ggplot with `+`.

## See also

[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html),
[`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html),
[`ggplot2::theme_update()`](https://ggplot2.tidyverse.org/reference/get_theme.html)

## Examples

``` r
if (FALSE) {

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl))) +
    ggplot2::geom_bar() +
    ggplot2::labs(title = "ROME theme demo")

  # Add the theme to a single plot:
  p + ROME_ggtheme()

  # Set as the global default for this session:
  ggplot2::theme_set(ROME_ggtheme())
}
}
```
