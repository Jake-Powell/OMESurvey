# OME fill colour scale

Convenience wrapper around
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
using the standard OME colour palette.

Convenience wrapper around
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
using the standard OME colour palette.

## Usage

``` r
scale_fill_OME(type = "distinct", ...)

scale_fill_OME(type = "distinct", ...)
```

## Arguments

- type:

  Character string specifying which palette to use. Passed to
  [`get_OME_colours`](https://jake-powell.github.io/OMESurvey/reference/get_OME_colours.md);
  see that function for available options. Default is `"distinct"`.

- ...:

  Additional arguments passed to `scale_fill_manual()`.

## Value

A ggplot2 scale for fill colours.

A ggplot2 scale for fill colours.
