# Add theme summary sheet

Add theme summary sheet

## Usage

``` r
to_sheet_theme_summary(
  data,
  theme = NULL,
  theme_columns = NULL,
  append_to = NULL,
  wb = NULL,
  sheet = "Data",
  order = FALSE
)
```

## Arguments

- data:

  survey data, formatted where each row is an individual and column
  names describe demographics or survey questions (of the format
  YY\_\_ZZ, where XX is the theme and YY the question, where ' ' are
  replaced with '\_' and '?' with 'XX'.)

- theme:

  string containing the theme name.

- theme_columns:

  Indices of the columns in the data for the theme

- append_to:

  Flag (TRUE/FALSE) for whether the summary sheet is appended to a
  previous workbook

- wb:

  the workbook to append the sheet to (requires append_to = TRUE)

- sheet:

  the sheet name

- order:

  not used

## Value

a workbook with the theme summary added
