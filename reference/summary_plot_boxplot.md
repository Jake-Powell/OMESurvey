# Section-level summary plot for numeric questions

Make a horizontal boxplot to summarise several numeric survey questions,
with questions ordered according to a summary statistic (by default the
median of observed responses).

## Usage

``` r
summary_plot_boxplot(
  dat,
  labels_vec = NULL,
  na.rm = FALSE,
  order_fun = median,
  titleText = NULL,
  group_label_width = 30,
  ...
)
```

## Arguments

- dat:

  A tibble/data.frame, each variable corresponding to a survey question.
  All variables should be numeric.

- labels_vec:

  Optional named character vector of labels to use for the questions on
  the plot. Names must match the column names of `dat`, and values are
  the labels to display on the axis.

- na.rm:

  Logical. Whether missing values should be removed/ignored for the
  purposes of plotting. Passed through to
  [`OME_boxplot_()`](https://jake-powell.github.io/OMESurvey/reference/OME_boxplot_.md).
  Defaults to `FALSE`. Note that missing values are always removed when
  computing the ordering statistic.

- order_fun:

  Function used to order questions in the plot. This function should
  accept arguments `(x, na.rm = TRUE)` and return a single numeric value
  (e.g. `median`, `mean`). Defaults to `median`.

- titleText:

  Optional text to use as the plot title.

- group_label_width:

  Optional integer. Width (in characters) used when wrapping question
  labels on the axis. Passed to
  [`OME_boxplot_()`](https://jake-powell.github.io/OMESurvey/reference/OME_boxplot_.md).
  Default is 30.

- ...:

  Additional arguments passed to
  [`OME_boxplot_()`](https://jake-powell.github.io/OMESurvey/reference/OME_boxplot_.md).

## Value

A `ggplot` object, a horizontal boxplot summarising the survey
questions.

## Details

The variables in `dat` are pivoted to long format, then ordered
according to the value returned by `order_fun` applied to each
question’s observed responses (with `na.rm = TRUE`).

The original column order of `dat` is preserved as a stable tie-breaker
when multiple questions have identical ordering statistics.

Missing-value handling for ordering and for plotting are intentionally
separated: missing responses are ignored for ordering purposes, but
their treatment in the plot itself is controlled by `na.rm`.

## Examples

``` r
# Minimal example with three numeric questions
dat <- tibble::tibble(
  Q1 = c(1, 2, 3, NA),
  Q2 = c(5, 4, 3, 1),
  Q3 = c(2, 2, 2, 3)
)

# Simplest use
dat |> summary_plot_boxplot()



# Add question labels
labels <- c(
  Q1 = "High-valued question",
  Q2 = "Low-valued question",
  Q3 = "Mid-valued question"
)
dat |> summary_plot_boxplot(labels_vec = labels)


# With longer labels
labels_long <- c(
  Q1 = "Question where responses tend to be at the higher end of the scale",
  Q2 = "Question where responses tend to be at the lower end of the scale",
  Q3 = "Question where responses tend to be somewhere in the middle"
)
dat |> summary_plot_boxplot(labels_vec = labels_long)


# Control label wrapping
summary_plot_boxplot(
  dat,
  labels_vec = labels_long,
  group_label_width = 20
)


# Remove missing values for plotting
dat |> summary_plot_boxplot(na.rm = TRUE)
```
