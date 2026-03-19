# Stacked proportional bar chart in OME style (Dave's version.)

`OME_stacked_bar()` builds a stacked (filled) proportional bar chart
from survey-style data using **bare column names**. `OME_stacked_bar_()`
is the programmatic interface and underlying engine, designed for use
when column names are supplied as **strings** or **symbols**.

## Usage

``` r
OME_stacked_bar_(
  dat,
  response_var,
  group_var = NULL,
  facet_var = NULL,
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
  facet_layout = NULL,
  fill_label_width = 20
)

OME_stacked_bar(dat, response_var, group_var = NULL, facet_var = NULL, ...)
```

## Arguments

- dat:

  A data frame.

- response_var:

  Bare column name giving the response categories (used for fill).

- group_var:

  Optional bare column name defining separate bars.

- facet_var:

  Optional bare column name used for faceting. All variables should be
  factors (characters may work but are not guaranteed).

- percCut:

  Numeric scalar (0–100). Cutoff below which percentages are not shown
  in bar segments. Default `5`; use a number \>100 to suppress all
  percentages.

- colo:

  Optional character vector of fill colours (one per response level). If
  `NULL` (default), colours are taken from
  `OMESurvey::get_OME_colours(type = "distinct")`. When `na.rm = FALSE`,
  a grey colour is prepended for the `"Missing"` level.

- na.rm:

  Logical. If `TRUE`, remove `NA` responses; if `FALSE` (default)
  convert them to `"Missing"` and treat as an additional response level.

- horiz:

  Logical (default `FALSE`). If `TRUE`, flip coordinates so bars are
  horizontal and place the legend below the plot.

- text_scale:

  Positive number (default `1`) scaling the size of percentage and `(n)`
  labels.

- fillLabText:

  Optional legend title for the fill variable. If `NULL` (default) the
  title is removed; if `""` the name of `response_var` is used.

- xLabText:

  Optional label for the bar‑group axis. If `NULL` the label is removed;
  if `""` the name of `group_var` is used.

- yLabText:

  Label for the proportion axis. Default `"Proportion of responses"`.

- titleText:

  Optional plot title. Default `NULL` removes the title.

- facet_labels:

  Optional named character vector used as a labeller for facets (names
  are original facet values, values are labels to display). Default
  `NULL` uses the raw facet values.

- facet_layout:

  Optional character. If `"1row"` the facets are arranged in a single
  row; otherwise the default facet layout is used.

- fill_label_width:

  Optional integer. Width (in characters) used when wrapping fill labels
  with
  [`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html).
  Default is 20.

- ...:

  Additional arguments passed to the underlying engine.

## Value

A `ggplot` object.

## Details

The plot displays:

- relative frequencies of the response variable,

- optional subdivision into separate bars,

- optional faceting,

- percentage labels inside bar segments (above a cutoff),

- `(n)` labels showing the number of responses per bar.

`OME_stacked_bar()` uses tidy‑evaluation; `OME_stacked_bar_()` uses
standard evaluation and is safe for loops and programmatic workflows.

## Note

Future/possible extensions include adding an optional dashed line (like
[`OME_boxplot()`](https://jake-powell.github.io/OMESurvey/reference/OME_boxplot.md)),
making the `(n)` labels optional, using a secondary axis for `(n)`
labels, improving percentage‑label contrast for light fill colours, and
a 1‑column option for facet layout.

## Author

Dave Sirl

## Examples

``` r
dat <- tibble::tibble(
  Response = factor(c("Yes", "No", "No", NA, "Yes", "Maybe")),
  Group    = factor(c("A", "A", "A", "A", "B", "B"))
)

# Basic example (NA is made explicit)
OME_stacked_bar(dat, Response, Group)


# NA's ignored
OME_stacked_bar(dat, Response, Group, na.rm=TRUE)


# Horizontal version with a title
OME_stacked_bar(
   dat,
   response_var = Response,
   group_var = Group,
   horiz = TRUE,
   titleText = "Example bar chart"
)


# Example programmatic use
if (FALSE) { # \dontrun{
  vars <- c("Gender", "Ethnicity", "FSM")
  for (v in vars) {
    p <- OME_stacked_bar_(dat = survey_df, response_var = v)
    print(p)
  }
} # }
```
