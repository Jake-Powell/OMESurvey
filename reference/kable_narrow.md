# Narrow kable table with standard styling

Convenience wrapper around
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) that
applies
[`kableExtra::kable_styling()`](https://rdrr.io/pkg/kableExtra/man/kable_styling.html)
with sensible defaults for compact, centred tables in reports.

## Usage

``` r
kable_narrow(x, ..., full_width = FALSE, position = "center")
```

## Arguments

- x:

  An object to be converted to a table by
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

- ...:

  Additional arguments passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

- full_width:

  Logical; whether the table should span the full page width. Defaults
  to `FALSE`.

- position:

  Character string controlling table positioning (e.g. `"center"`,
  `"left"`, \`"right").

## Value

A styled `kableExtra` table object.

## Author

Dave Sirl
