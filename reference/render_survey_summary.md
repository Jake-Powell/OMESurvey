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
  verbose = FALSE,
  show = FALSE,
  rmd = NULL
)
```

## Arguments

- data_path:

  path to the survey data (assumed to be xls/xlsx/csv).

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

  (optional) path to establishment characteristics data (xls/xlsx).

- est_chars_sheet:

  (optional, needed if `est_chars_path` is used) name of sheet in est't
  chars to use.

- est_char_vars, est_char_types, est_char_values, est_char_statements:

  (optional, needed if `est_chars_path` is used) specification of
  establishment characteristics to use, see Details.

- quiet:

  (optional) whether to quieten the Rmd rendering information. Defaults
  to TRUE; FALSE useful for testing.

- verbose:

  (optional) whether to include extra/verbose detail in the report.
  Defaults to FALSE; TRUE useful for testing.

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

Additional grouping variables using Establishment Characteristics can be
added using `est_chars_path` and `est_chars_sheet` to point to the
Establishment Characteristics spreadsheet and the relevant sheet
thereof. If used then `est_char_vars`, `est_char_types`,
`est_char_values` and `est_char_statements` must also be used, to
specify the establishment characteristics variables to be used and their
details as in the data dictionary. See example below.

## Examples

``` r
# Simplest usage
render_survey_summary(
  data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",
  dict_path = "data_dict_copy.xlsx",
  dict_sheet = "pupil_survey_Y12",
  output_file = "y12_summary.html",
  output_title = "Y12 student survey summary",
  output_author = "D Sirl",
)
#> Error in render_survey_summary(data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",     dict_path = "data_dict_copy.xlsx", dict_sheet = "pupil_survey_Y12",     output_file = "y12_summary.html", output_title = "Y12 student survey summary",     output_author = "D Sirl", ): File not found: C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx

# Using establishment characteristics too
render_survey_summary(
  data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",
  dict_path = "data_dict_copy.xlsx",
  dict_sheet = "pupil_survey_Y12",
  output_file = "y12_summary.html",
  output_title = "Y12 student survey summary",
  output_author = "D Sirl",
  est_chars_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME Research Data - Partner Establishment Characteristics/Partner Establishment Characteristics Pseudonymised Data/2026-01-29-partner_characteristics_june_2025_data.xlsx",
  est_chars_sheet = "partner_characteristics_sec",
  est_char_vars =
    c("TrustCategory",
      "UrbanRural"),
  est_char_types =
    c("factor-neg-pos",
      "factor-unordered"),
  est_char_values =
    c("No trust; Trust of 1-9; Trust of 10-19; Trust of 20+",
      "Conurbation; City or town; Rural"),
  est_char_statements =
    c("Trust size",
      "Urban/Rural classification")
)
#> Error in render_survey_summary(data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",     dict_path = "data_dict_copy.xlsx", dict_sheet = "pupil_survey_Y12",     output_file = "y12_summary.html", output_title = "Y12 student survey summary",     output_author = "D Sirl", est_chars_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME Research Data - Partner Establishment Characteristics/Partner Establishment Characteristics Pseudonymised Data/2026-01-29-partner_characteristics_june_2025_data.xlsx",     est_chars_sheet = "partner_characteristics_sec", est_char_vars = c("TrustCategory",         "UrbanRural"), est_char_types = c("factor-neg-pos", "factor-unordered"),     est_char_values = c("No trust; Trust of 1-9; Trust of 10-19; Trust of 20+",         "Conurbation; City or town; Rural"), est_char_statements = c("Trust size",         "Urban/Rural classification")): File not found: C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx
```
