# Missingness report

Create a report on the missingness of entries in a survey.

## Usage

``` r
create_missingness_report(
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

  Either 'interactive'(.html) or 'static'(.docx)

- demographic_columns:

  column names (or indices) of the columns in `survey_data`
  corresponding to demographic columns. These will be used to compare
  missingness across the demographics.

- output_dir:

  The path of the output directory

- output_file:

  the filename of the output. If `NULL` we use paste0(survey_name,
  '\_missingness_report', "extension") as the filename.

- reference_docx:

  Path to a .docx file whose style (design) the report copies

## Value

html/word report

## Details

Note that currently if report_kind = 'static', then the tables and plots
are screenshots of the interactive plots (using the webshot package).
Moreover, chromote version 0.4.0 is needed to avoid rendering issues.

## Examples

``` r
if (FALSE) {
data = OMESurvey::survey_example

# Run with no missingness.
OMESurvey::create_missingness_report(survey_data = data,
                                     survey_name = 'example data',
                                     participant_ID = "Barcode_ID",
                                     report_kind = "interactive",
                                     demographic_columns = NULL,
                                     output_dir = NULL,
                                     output_file = NULL,
                                     reference_docx = NULL)

# Convert 'N-A' to NA, to imply missing
for(i in 1:ncol(data)){
  data[,i][data[,i] == 'N-A'] = NA
}

# Run with missingness.
OMESurvey::create_missingness_report(survey_data = data,
                                     survey_name = 'example data',
                                     report_kind = "interactive")
# Knit to word.
OMESurvey::create_missingness_report(survey_data = data,
                                     survey_name = 'example data',
                                     report_kind = "static",
                                     output_file = 'example_word.docx')

# Knit to word with styling. (using example template from within the OMESurvey package)
OMESurvey::create_missingness_report(survey_data = data,
                                     survey_name = 'example data',
                                     participant_ID = "Barcode_ID",
                                     report_kind = "static",
                                     reference_docx = paste0(
                                     system.file(package = "OMESurvey"), "/template.docx"
                                     ),
                                     output_file = 'example_word_styled.docx')
}
```
