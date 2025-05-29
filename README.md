
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OMESurvey <a href="https://jake-powell.github.io/OMESurvey/"><img src="man/figures/logo.png" align="right" height="139" alt="OMESurvey website" /></a><!-- badges: start -->

<!-- badges: end -->

📦 The `OMESurvey` package contains tools for analysing survey data for
the OME, in particular (for now) converting raw survey data into
publishable excel workbooks.

## Installation

You can install the development version of OMESurvey from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Jake-Powell/OMESurvey")
```

## Examples

Creating survey results for a given theme

``` r
library(OMESurvey)
data = OMESurvey::survey_example

OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')
#> Warning in OMESurvey::plot_theme(data, theme = "Theme_1", kind = "ggplot"): The
#> expected answers are: "Agree a lot", "Agree a little", "Neither", "Disagree a
#> little", "Disagree a lot" which does not include the following found in the
#> data: "N-A"
```

<img src="man/figures/README-example-1.png" width="100%" />
