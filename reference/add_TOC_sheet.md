# Add table of contents to an workbook

Add table of contents to an workbook

## Usage

``` r
add_TOC_sheet(wb, link_text = NULL, return_pos = c(1, 1))
```

## Arguments

- wb:

  the workbook to append the sheet to (requires append_to = TRUE)

- link_text:

  character vector of the text displayed for the linked text in the TOC
  sheet. Must have the same length as the number of sheets in wb.

- return_pos:

  integer pair, of the row and column index to place the return to
  contents link on the sheets in the wb.

## Value

openxlsx workbook with TOC added.
