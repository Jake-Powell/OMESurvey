# Convert shorthand colour name to a colour vector

`convert_colo` accepts `NULL`, a character vector of colour hex codes,
or a single keyword and returns a character vector of hex colours (or
`NULL`). This helper is intended for normalising user-supplied colour
arguments into a vector usable by
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
and similar functions. Tidies a situation where a variable (usually
called `colo`) is passed around as either a string of hex code colours
or a shortcut name/keyword. Mainly kept for backwards-compatibility - in
most contexts this function SHOULD NOT BE USED, as
[`OMESurvey::get_OME_colours()`](https://jake-powell.github.io/OMESurvey/reference/get_OME_colours.md)
is more direct & transparent and less hard-coded.

## Usage

``` r
convert_colo(colo)
```

## Arguments

- colo:

  `NULL`, a character vector of colour hex codes, or a single character
  keyword. If it is a character of length 1 it is expanded into a
  character vector of colours (see Details). Otherwise it is returned
  unchanged.

## Value

A character vector of hex colour codes, or `NULL` if `colo` is `NULL`.

## Details

The following single-string keywords are recognised and expanded:

- `"likert5"`:

  5-colour palette:
  `c("#009BC1","#08607E","#10263B","#732C53","#D7336C")`

- `"scale4"`:

  4-colour palette: `c("#009BC1","#08607E","#732C53","#D7336C")`

- `"ynsn3"`:

  3-colour palette: `c("#009BC1","#10263B","#D7336C")`

- `"scale2"`:

  2-colour palette: `c("#009BC1","#D7336C")`

Any other single string is returned as-is, i.e. it DOESN'T throw an
error/warning!

## Examples

``` r
convert_colo(NULL)            # NULL -> NULL
#> NULL
convert_colo(c("#000000","#FFFFFF"))  # vector -> returned unchanged
#> [1] "#000000" "#FFFFFF"
convert_colo("likert5")       # returns the 5-colour likert palette
#> [1] "#009BC1" "#08607E" "#10263B" "#732C53" "#D7336C"
convert_colo("an_odd_name")       # Unintended use: unknown and so returned unchanged
#> [1] "an_odd_name"
convert_colo(c("thing","amijig"))  # Unintended use: vector -> returned unchanged
#> [1] "thing"  "amijig"
```
