# Make stacked bar chart (possibly split into separate bars and facetted) in the OME style. (Dave's version.)

`initial_bar` builds a (stacked, filled) proportional bar chart from a
tidy data frame of survey responses. The first column is treated as the
response factor (fill), the optional second column as the
grouping/subdivision, and the optional third column is used for
faceting.

## Usage

``` r
initial_bar(
  dat,
  percCut = NULL,
  colo = NULL,
  na.rm = FALSE,
  horiz = FALSE,
  text_scale = 1,
  fillLabText = NULL,
  xLabText = NULL,
  yLabText = "Proportion of responses",
  titleText = NULL,
  facet_labels = NULL,
  facet_layout = NULL
)
```

## Arguments

- dat:

  A data frame with 1–3 variables:

  - **1 variable** → a single proportional bar.

  - **2 variables** → separate bars for each level of the second
    variable.

  - **3 variables** → facetted bars by the third variable. All variables
    should be factors (characters may work but are not guaranteed).

- percCut:

  numeric scalar (0-100). Cutoff below which percentages are not shown
  in bar segments. Default `5`.

- colo:

  (optional but recommended) vector of colours to use for the fill scale
  (character vector of colours, length the same as the number of levels
  of the factor that is the first variable of `dat`.) If `NULL`
  (default) the pallete from
  `OMESurvey::get_OME_colours(type='distinct')` is used. When
  `na.rm = FALSE` a grey colour is prepended for the "No response"
  level. Note that percentage labels are in white, so the pallete needs
  to work with that. (... or this function needs upgrading!)

- na.rm:

  logical. If `TRUE` then remove `NA` responses from the data; if
  `FALSE` (default) then they are converted to "(No response)" and
  treated as an additional allowed response.

- horiz:

  logical (default FALSE) determining whether to coord_flip() so that
  bars are horizontal and place legend below the plot.

- text_scale:

  positive number (default 1) for scaling the size of the percentage and
  \#response labels.

- fillLabText:

  Optional character. Label for the fill/legend. If `NULL` (the default)
  the legend title is removed (no label). If `""` the name of the first
  variable in `dat` is used.

- xLabText:

  optional text for labelling the second-variable-in-`dat` axis. If
  `NULL` (the default) the label is removed (no label). If `""` the name
  of the first variable in `dat` is used.

- yLabText:

  optional text for labelling the 'proportion' axis. Default
  `"Proportion of responses"`.

- titleText:

  optional text for plot title. With default `NULL` the title is
  removed.

- facet_labels:

  Optional named character vector used as a labeller for facets (names
  are original facet values, values are labels to display). Default
  `NULL` uses the values of that variable.

- facet_layout:

  Optional character. If `"1row"` the facets are arranged in a single
  row; otherwise default facet layout is used. (`"1col"` option planned
  but not yet supported.)

## Value

A `ggplot` object (or a `cowplot` object if `cowplot` is installed)

## Details

The plot displays:

- relative frequencies of the first variable,

- optional subdivision into separate bars,

- optional faceting,

- percentage labels inside bar segments (above a cutoff),

- `(n)` labels showing the number of responses per bar.

The function returns a `ggplot` (or `cowplot` if it is installed) object
ready for further customization or direct printing.

Percentages are displayed in segments that comprise at least `percCut`
of their bar; the percentage displayed is this rounded to a whole
number.

## Examples

``` r
# Minimal example with two questions and three response levels
dat <- tibble::tibble(
  Response = factor(c("Yes", "No", "Yes", "Maybe")),
  Group    = factor(c("A", "A", "B", "B"))
)

initial_bar(dat)


# Horizontal version with a title
initial_bar(dat, horiz = TRUE, titleText = "Example bar chart")

```
