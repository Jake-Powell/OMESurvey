# Make a stacked bar chart summarising many survey questions

Make a horizontal stacked bar chart to summarise several survey
questions, with questions ordered according to the proportion of
responses that fit a particular pattern.

## Usage

``` r
plot_many_questions(
  dat,
  labels_vec = NULL,
  na.rm = FALSE,
  percCut = 5,
  colo = NULL,
  order_values = NULL,
  titleText = NULL,
  fill_label_width = 20,
  question_label_width = 30
)
```

## Arguments

- dat:

  a tibble/data.frame, each variable corresponding to a question. All
  variables should be factors and all with the same possible values.

- labels_vec:

  a named vector of labels to use for the questions on the plot. Names
  are variable names and values are corresponding labels.

- na.rm:

  Logical. If `TRUE`, remove `NA` responses; if `FALSE` (default)
  convert them to `"Missing"` and treat as an additional response level.

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
  to work with that. (... or this function/package needs upgrading!)

- order_values:

  Optional. Controls how questions are ordered in the plot. May be a
  character vector of response levels, or the special value
  `"mean(as.numeric())"`. See Details.

- titleText:

  Optional text to use as plot title.

- fill_label_width:

  Optional integer. Width (in characters) used when wrapping fill labels
  with
  [`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html).
  Passed to
  [`OME_stacked_bar()`](https://jake-powell.github.io/OMESurvey/reference/OME_stacked_bar_.md).
  Default is 20.

- question_label_width:

  Optional integer. Width (in characters) used when wrapping axis labels
  with
  [`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html).
  Default is 30.

## Value

A `ggplot` object, a horizontal stacked bar chart summarising the survey
questions.

## Details

The `order_values` argument controls how questions are ordered in the
horizontal bar chart.

- If `order_values` is a character vector of response levels (e.g.
  `c("Strongly agree", "Agree")`), questions are ordered by the
  proportion of respondents whose answers fall into those levels.
  (Intended for factors with positive-negative / divergent scales.)

- If `order_values` is exactly `"mean(as.numeric())"`, the function
  instead orders questions by the mean of the numeric codes of the
  response factor. (Intended for factors with low-high / sequential
  scales.)

Axis labels are wrapped using
[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
with a width controlled by `question_label_width`. Legend/fill labels
are treated the same using `fill_label_width`.

## Note

All variables in `dat` should have identical factor levels. The names of
`labels_vec` must match the column names of `dat`. Values in
`order_values` must be valid levels of the response factor.

## Legend placement

By default, the legend is placed horizontally according to the standard
ggplot2 layout. If you want the legend to be centred across the full
width of the final figure, the helper function
[`centre_legend_below()`](https://jake-powell.github.io/OMESurvey/reference/centre_legend_below.md)
can be used after plotting. (This must be done after any tweaks to the
ggplot theme, annotations, etc)

## Examples

``` r
# Minimal example with three questions and three response levels
dat <- tibble::tibble(
  Q1 = factor(c("Yes", "No", "Yes", NA), levels=c("No", "Maybe", "Yes")),
  Q2 = factor(c("No", "No", "Yes", "Maybe"), levels=c("No", "Maybe", "Yes")),
  Q3 = factor(c("Maybe", "Yes", "Maybe", "Yes"), levels=c("No", "Maybe", "Yes"))
)

# Simplest use
plot_many_questions(dat, labels_vec = labels)
#> Error in stri_split_lines(str): argument `str` should be a character vector (or an object coercible to)

# Add question labels
labels <- c(Q1 = "Question 1", Q2 = "Question 2", Q3 = "Question 3")
plot_many_questions(dat, labels_vec = labels)


# With custom wrapping if they are long
labels_long <- c(Q1 = "Question 1", Q2 = "Question 2", Q3 = "The third question asked as part of this series of three questions")
plot_many_questions(dat, labels_vec = labels_long)

plot_many_questions(dat, labels_vec = labels_long, question_label_width = 20)


# Remove missing values
plot_many_questions(dat, na.rm=FALSE)

```
