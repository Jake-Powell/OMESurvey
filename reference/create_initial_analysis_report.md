# Initial analysis report

Create a report on the initial finding of a survey.

## Usage

``` r
create_initial_analysis_report(
  survey_data,
  survey_name,
  participant_ID = "Barcode_ID",
  report_kind = "interactive",
  demographic_columns = NULL,
  output_dir = NULL,
  output_file = NULL,
  reference_docx = NULL
)
```

## Arguments

- survey_data:

  The cleaned survey data

- survey_name:

  The name of the survey

- participant_ID:

  column name of the column in `survey_data` corresponding to the
  participant ID.

- report_kind:

  Either 'interactive'(.html) or 'static'(.docx) for the report type

- demographic_columns:

  column names (or indices) of the columns in `survey_data`
  corresponding to demographic columns. These will be used to compare
  missingness across the demographics.

- output_dir:

  The path of the output directory

- output_file:

  the filename of the output. If `NULL` we use paste0(survey_name,
  '\_initial_analysis_report', "extension") as the filename.

- reference_docx:

  Path to a .docx file whose style (design) the report copies.

## Value

html/word report
