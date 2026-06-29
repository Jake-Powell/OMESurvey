# Section-level summary plot for numeric questions

Make a horizontal boxplot to summarise several numeric survey questions,
with questions ordered according to a summary statistic (by default the
median of observed responses).

## Usage

``` r
summary_plot_boxplot(
  dat,
  dat_format = "auto",
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

  A tibble/data.frame containing survey responses.

  The function supports two input formats:

  - **Simple format**: one numeric column per question. All questions
    are assumed to be included and plotted.

  - **Extended format**: columns named using the pattern
    `question_value`, with optional `question_include` and
    `question_plot` columns.

    - `*_value` columns contain numeric responses.

    - `*_include` (logical) indicates whether a response is included in
      the analysis at all.

    - `*_plot` (logical) indicates whether a response is shown in the
      plot (values set to `FALSE` are treated as missing).

    Any missing `*_plot` and `*_include` columns are assumed to be
    `TRUE`.

    In all cases,

    - all `*_value` variables should be numeric.

    - only variables to be used in the plot should be included. I.e.
      calls may need to be of the form
      `data |> dplyr::select(<variables needed for plotting>) |> summary_plot_boxplot()`.

- dat_format:

  One of `"auto"`, `"simple"`, or `"extended"`. Defaults to `"auto"`,
  which detects extended format if any `*_value` columns are present,
  otherwise assumes simple format.

- labels_vec:

  Optional named character vector of labels to use for the questions on
  the plot. Names should correspond to variable names (without `_value`
  appended in extended format), and values are the labels to display on
  the axis.

- na.rm:

  Logical. Controls how missing values are handled in the plot. Missing
  values (including those created via `*_plot = FALSE`) are:

  - removed if `TRUE`

  - retained (and therefore reflected in the displayed count) if `FALSE`
    (default).

- order_fun:

  Function used to order questions in the plot, or `NULL`. This function
  should accept arguments `(x, na.rm = TRUE)` and return a single
  numeric value (e.g. `median`, `mean`). If `NULL` ordering is the same
  as that of the variables in the data. Defaults to `median`.

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
according to the value returned by
order_fun`applied to each question's observed responses (with`na.rm =
TRUE\`).

The original column order of `dat` is preserved as a stable tie-breaker
when multiple questions have identical ordering statistics.

Missing-value handling for ordering and for plotting are intentionally
separated: missing responses are ignored for ordering purposes, but
their treatment in the plot itself is controlled by `na.rm`.

If `dat` is supplied in simple format, it is internally converted to the
extended format with all `*_plot` and `*_include` values set to `TRUE`.

## Author

Dave Sirl

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


# Extended format example
dat_ext <- tibble::tibble(
  Q1_value = dat$Q1,
  Q1_plot = c(TRUE, TRUE, TRUE, FALSE),
  Q2_value = dat$Q2,
  Q2_include = c(TRUE, TRUE, FALSE, TRUE)
)
summary_plot_boxplot(dat_ext)
```
