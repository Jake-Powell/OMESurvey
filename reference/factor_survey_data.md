# Factor survey questions in data

Factor survey questions in data

## Usage

``` r
factor_survey_data(data, survey_values = OMESurvey::survey_values, verbose = T)
```

## Arguments

- data:

  survey data

- survey_values:

  a list of allowed grouped survey values. Default is
  OMESurvey::survey_values.

- verbose:

  Flag (TRUE/FALSE) for whether to print console messages.

## Value

data with factored survey columns

## Examples

``` r
if (FALSE) {
data = OMESurvey::survey_example
for(i in 1:ncol(data)){
  data[[i]][which(data[[i]] == 'N-A')] = NA
}
clean = data |> factor_survey_data()
}
```
