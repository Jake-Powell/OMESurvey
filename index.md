# OMESurvey

📦 The `OMESurvey` package contains tools for pre-processing and
analysing survey data for the OME. in particular:

- converting raw survey data into publishable excel workbooks,
- pre-processing helpers,
- plotting functions.

## Installation

You can install the development version of OMESurvey from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Jake-Powell/OMESurvey")
```

## Examples

### Plotting survey result

``` r
library(OMESurvey)
data = OMESurvey::survey_example

OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')
#> Warning in OMESurvey::plot_theme(data, theme = "Theme_1", kind = "ggplot"): The
#> expected answers are: "Agree a lot", "Agree a little", "Neither", "Disagree a
#> little", "Disagree a lot" which does not include the following found in the
#> data: "N-A"
```

![](reference/figures/README-example-1.png)

### Checking for issues in a survey

Use
[`check_survey()`](https://jake-powell.github.io/OMESurvey/reference/check_question_columns.md)
to search for unexpected responses (for non free text), issues with the
barcode and locations of questions with multiple answers (“99”)

``` r
check = OMESurvey::survey_example |>
  OMESurvey::convert_NA() |>
  OMESurvey::check_survey()
check
#> $barcode
#> $barcode$isIssue
#> [1] FALSE
#> 
#> $barcode$issues
#> NULL
#> 
#> 
#> $question_text
#> $question_text$isIssue
#> [1] TRUE
#> 
#> $question_text$issues
#>                question Unexpected.value Count  Indices
#> 5  Theme_3__Question_21          Neither     2 187, 352
#> 7  Theme_3__Question_22     Almost Never     1        3
#> 11 Theme_5__Question_41   DIsagree a lot     1      252
#>                                                                          expected
#> 5  Almost always, Most of the time, Half the time, Some of the time, Almost never
#> 7  Almost always, Most of the time, Half the time, Some of the time, Almost never
#> 11        Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 
#> 
#> $multiple_answers
#> $multiple_answers$isIssue
#> [1] TRUE
#> 
#> $multiple_answers$issues
#>                question Unexpected.value Count       Indices
#> 1   Theme_1__Question_3               99     1           344
#> 2   Theme_1__Question_4               99     3 240, 251, 402
#> 3   Theme_2__Question_9               99     2      241, 260
#> 4  Theme_3__Question_20               99     1            56
#> 6  Theme_3__Question_21               99     1           387
#> 8  Theme_5__Question_32               99     2      272, 480
#> 9  Theme_5__Question_35               99     2      154, 177
#> 10 Theme_5__Question_37               99     1            21
#> 12 Theme_6__Question_44               99     2      278, 317
#> 13 Theme_7__Question_52               99     3  26, 128, 335
#> 14 Theme_8__Question_56               99     1           369
#>                                                                          expected
#> 1         Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 2         Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 3         Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 4  Almost always, Most of the time, Half the time, Some of the time, Almost never
#> 6  Almost always, Most of the time, Half the time, Some of the time, Almost never
#> 8         Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 9         Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 10        Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 12        Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 13        Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 14        Agree a lot, Agree a little, Neither, Disagree a little, Disagree a lot
#> 
#> $multiple_answers$barcodes
#>  [1] 1200700304 1200700360 1200700413 1200700392 1200700402 1200700410
#>  [7] 1200800516 1200700389 1200700423 1200900701 1200800572 1200800585
#> [13] 1200800456 1201000757 1201000896 1200800469 1200800497 1201000908
#> [19] 1200700320
```
