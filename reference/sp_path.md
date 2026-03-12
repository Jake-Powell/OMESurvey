# Build a path within the OME SharePoint root

Constructs a file path relative to the user's synced SharePoint /
OneDrive root folder stored in the `OME_SHAREPOINT_ROOT` environment
variable.

## Usage

``` r
sp_path(..., root = NULL, must_exist = FALSE)
```

## Arguments

- ...:

  Path components to append to the SharePoint root, passed to
  [`base::file.path()`](https://rdrr.io/r/base/file.path.html).

- root:

  Optional character string giving the SharePoint root directly. If
  `NULL`, the value is taken from the `OME_SHAREPOINT_ROOT` environment
  variable.

- must_exist:

  Logical. If `TRUE`, an error is thrown if the resulting path does not
  exist. Default is `FALSE`.

## Value

A character string giving the full file path.

## Details

This allows scripts to use shared relative paths rather than hard-coded
user-specific absolute paths.

## Examples

``` r
if (FALSE) { # \dontrun{
sp_path("Folder", "file.csv")
sp_path("ProjectA", "Data", "raw_data.csv", must_exist = TRUE)
} # }
```
