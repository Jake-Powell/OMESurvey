# get colours for the OME

get colours for the OME

## Usage

``` r
get_OME_colours(n, type = "contrast")
```

## Arguments

- n:

  the number of colours.

- type:

  either "contrast/divergent", "complementary/sequential" or
  "distinct/qualitative".

## Value

a vector of "n" colours.

## Details

Base colours for palettes:

- OME complementary colours = "#10263B" (Dark Navy/Nottingham Blue),
  "#009BC1" (Blue/Malaysia Sky Blue) and "#37B4B0"(Turquoise/Trent
  Turquoise).

- OME contrast colours = "#009BC1" (Blue/Malaysia Sky Blue), "#10263B"
  (Dark Navy/Nottingham Blue) and '#D7336C' (Pink/Pioneering Pink).

- OME distinct colours = "#009BC1"(Blue/Malaysia Sky Blue),'#DEB406'
  (Gold/Rebel's Gold), '#D7336C' (Pink/Pioneering Pink) and '#10263B'
  (Dark Navy/Nottingham Blue).

For 'contrast' and 'distinct' specific colour orderings are chosen for n
\<= 9, whereas for 'complementary' specific colours selected for n \<=
3. If n is larger than we interpolate the palette using
grDevices::colorRampPalette().

## Examples

``` r
if (FALSE) {
get_OME_colours(3) |> scales::show_col()
get_OME_colours(3, type = 'complementary') |> scales::show_col()
get_OME_colours(9, type = 'distinct') |> scales::show_col()
}
```
