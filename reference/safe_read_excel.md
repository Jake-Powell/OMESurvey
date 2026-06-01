# Safely read Excel files, including when open or locked

A wrapper around
[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
that attempts to read an Excel file directly, and if that fails (e.g.
because the file is open or locked in Excel on Windows), falls back to
copying the file to a temporary location and reading from the copy.

## Usage

``` r
safe_read_excel(path, sheet = NULL, ...)
```

## Arguments

- path:

  A character string giving the path to the Excel file.

- sheet:

  Sheet to read from. Passed to
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).

- ...:

  Additional arguments passed to
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).

## Value

A tibble containing the data read from the Excel file.

## Details

This improves robustness in workflows where Excel files may be open
during execution, such as interactive data preparation or reporting
pipelines.

On some systems (notably Windows), Excel may lock files in a way that
prevents them being read by other processes. This function attempts a
direct read first, and only falls back to copying the file if that
fails.

If both the direct read and the copy fail, an error is thrown.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read normally
df <- safe_read_excel("data.xlsx")
df <- safe_read_excel("data.xlsx", sheet = 2)
} # }
```
