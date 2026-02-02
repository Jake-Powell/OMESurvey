# Add OME logo to a ggplot object

Add OME logo to a ggplot object

## Usage

``` r
add_logo(p, type = "", position = "bottom right", logo_sizing = c(0.11, 0.2))
```

## Arguments

- p:

  ggplot object.

- type:

  Specifies the type on logo. Either 'bw' or 'standard'.

- position:

  Specifies the logo position. Either 'top left', 'top right', 'bottom
  left' or 'bottom right'.

- logo_sizing:

  A numeric vector of length 2. The first element defines the amount of
  vertical spacing the logo uses (default is 11% of the figure height).
  The second element defines the logo width (min 0, max 1 default is
  0.2)

## Value

original plot with the logo added.

## Details

This function currently only works for ggplot objects and always adds
the logo outside the original plot (In an extended margin).

Note that you cannot use '+' to add this to the ggplot object, as the
function returns a non-ggplot object (as we use
gridExtra::grid.arrange() to add the logo). This should always be the
last step of creating a plot.

Play around with the logo sizing parameter until you find your desired
logo size.

## Examples

``` r
if (FALSE) {
# Load in some example survey data and create a plot.
data = OMESurvey::survey_example
p = OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')

# Add logo to plot either top right or bottom right.
p |> OMESurvey::add_logo(position = 'top right')
p |> OMESurvey::add_logo(position = 'bottom right', logo_sizing = c(0.21, 0.3))

}
```
