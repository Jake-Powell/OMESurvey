# Relabel NA values in the first column of a data frame

Converts the first column of a data frame to character and replaces any
`NA` values in that column with a specified label (default: "Missing").
Intended for use after `tabyl()` and related janitor formatting steps,
once the object has been converted to a plain data frame.

## Usage

``` r
rename_NA_first_col(df, missing_label = "Missing")
```

## Arguments

- df:

  A data frame whose first column may contain `NA` values.

- missing_label:

  A character string used to replace `NA` values in the first column.
  Defaults to `"Missing"`.

## Value

A data frame with the first column converted to character and any `NA`
values replaced by `missing_label`.

## Examples

``` r
c(rep("A", 3), rep("B", 2), rep(NA, 2)) |>
  janitor::tabyl() |>
  janitor::adorn_totals() |>
  janitor::adorn_pct_formatting() |>
  as.data.frame() |>
  rename_NA_first_col()
#>   c(rep("A", 3), rep("B", 2), rep(NA, 2)) n percent valid_percent
#> 1                                       A 3   42.9%         60.0%
#> 2                                       B 2   28.6%         40.0%
#> 3                                 Missing 2   28.6%             -
#> 4                                   Total 7  100.0%        100.0%
```
