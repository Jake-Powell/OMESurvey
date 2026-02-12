# Make stacked bar chart (possibly split into separate bars and facetted) in the OME style. (Dave's version.)

`initial_bar` builds a proportional (stacked, filled) bar chart from a
tidy data frame of survey responses. The first column is treated as the
response factor (fill), the optional second column as the
grouping/subdivision, and the optional third column is used for
faceting. Percentages of bars are overlaid on the plot and number of
observations/responses are shown at the end of each bar. The function
returns a `ggplot` object ready for further customization or direct
printing.

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

  data frame of 1, 2 or 3 variables. Bar charts show relative
  frequencies of the first variable, split into separate bars according
  to the second variable (if present), and facetted by the third
  variable (if present). All variables should be factors (but characters
  might work?).

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

ggplot object - a stacked proportional bar chart.

## Details

Percentages are displayed in segments that comprise at least `percCut`
of their bar; the percentage displayed is this rounded to a whole
number.

## Examples

``` r
# NEED A SHORT EXAMPLE
```
