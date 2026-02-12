# Produce html document summarising a single survey

Produce a summary html document of all responses in an OME survey, based
on preprocessed data and the data dictionary. Structure of the summary
document is encoded in specified columns of the data dictionary.

## Usage

``` r
render_survey_summary(
  data_path,
  dict_path,
  dict_sheet,
  output_dir = getwd(),
  output_file = NULL,
  output_title = NULL,
  output_author = NULL,
  output_date = NULL,
  overwrite = FALSE,
  est_chars_path = NULL,
  est_chars_sheet = NULL,
  est_char_vars = NULL,
  est_char_types = NULL,
  est_char_values = NULL,
  est_char_statements = NULL,
  quiet = TRUE,
  show = FALSE,
  rmd = NULL
)
```

## Arguments

- data_path:

  path to the survey data (assumed to be xls/xlsx).

- dict_path, dict_sheet:

  path to the data dictionary (assumed to be xls/xlsx) and name of sheet
  in that file to use.

- output_dir:

  (optional) directory to save the output file to. Defaults to current
  working directory. Ignored if `output_file` is not specified.

- output_file:

  name of output html file (needs to end with ".html", defaults to
  "\_summary.html")

- output_title:

  (optional) text string to use as title for html report output

- output_author:

  (optional) text string to use as author for html report output

- output_date:

  (optional) text string to use as date for html report output

- overwrite:

  (optional) whether to overwrite `output_file` if it already exists.
  Defaults to FALSE for safety but in use probably TRUE will be more
  typical.

- est_chars_path:

  path to establishment characteristics data (xls/xlsx).

- est_chars_sheet:

  name of sheet in est't chars to use.

- est_char_vars, est_char_types, est_char_values, est_char_statements:

  specification of establishment characteristics to use, see Details
  (write).

- quiet:

  (optional) whether to quieten the Rmd rendering information. Defaults
  to TRUE; FALSE useful for testing.

- show:

  (optional) whether to show intermediate results in the RStudio Viewer.
  Defaults to FALSE; unclear when TRUE might be useful.

- rmd:

  (optional) path to .Rmd document to use for report-making. Using
  anything other than the default should be rare.

## Value

a string giving the path of the saved .html document

## Details

Here is some text for the details. Needs to say something about the
arguments
est_char_vars,est_char_types,est_char_values,est_char_statements. And
possibly quiet, show, rmd too.

## Examples

``` r
#No examples yet - probably difficult and low priority,
# given the need to have data and a dictionary and that we produce a file.
```
