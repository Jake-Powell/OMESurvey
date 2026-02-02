# Create OME branded excel output

``` r
library(OMESurvey)
```

*In this example we show how to go from raw survey data to formatted
excel outputs.*

We need to load in some fake survey results.

``` r
data = OMESurvey::survey_example
# Clean 1. 
# B) NAs
for(i in 1:ncol(data)){
  data[,i][data[,i] == 'N-A'] = NA
  data[,i] = data[,i] |> stringr::str_to_sentence() # commented out for example.
}

# C + D) School ID and combine school and class information.
school = data$Barcode_ID |> as.character() |>  substr(3, 5)
school_and_class = paste0(school,'__', data$`Group_/_class`) 

# Combine into a cleaned dataset. 
data = data.frame(data[,1:4], school, school_and_class, data[,5:ncol(data)])

tibble::tibble(data)
#> # A tibble: 484 × 68
#>    Barcode_ID First_Name Surname Group_._class school school_and_class
#>    <chr>      <chr>      <chr>   <chr>         <chr>  <chr>           
#>  1 1200800551 Jake       Powell  15            008    008__15         
#>  2 1200800439 Jake       Powell  11            008    008__11         
#>  3 1200800442 Jake       Powell  11            008    008__11         
#>  4 1200800501 Jake       Powell  13            008    008__13         
#>  5 1200800539 Jake       Powell  14            008    008__14         
#>  6 1200800500 Jake       Powell  13            008    008__13         
#>  7 1200800511 Jake       Powell  13            008    008__13         
#>  8 1200800493 Jake       Powell  13            008    008__13         
#>  9 1200800498 Jake       Powell  13            008    008__13         
#> 10 1200800488 Jake       Powell  12            008    008__12         
#> # ℹ 474 more rows
#> # ℹ 62 more variables:
#> #   Tick_Appropriate_Box_Only_if_the_Child_did_not_take_the_Test <chr>,
#> #   Theme_1__Question_1 <chr>, Theme_1__Question_2 <chr>,
#> #   Theme_1__Question_3 <chr>, Theme_1__Question_4 <chr>,
#> #   Theme_1__Question_5 <chr>, Theme_1__Question_6 <chr>,
#> #   Theme_2__Question_7 <chr>, Theme_2__Question_8 <chr>, …
```

We can see that the data is formatted such that each row corresponds to
an individual’s response to the survey where the initial columns are the
individuals demographics followed by response to survey questions.
Moreover, note that the column names of questions is of the format
YY\_*ZZ, where YY is the theme and ZZ the question, where ’ ’ are
replaced with ’*’ and ‘?’ with ‘XX’ such as `Theme_1__Question_1`.

Now we can create some excel workbooks with breakdowns of how questions
were answered overall or by demographics.

### A single question with demographics

Show the results for Maths in Year 7:I am doing well in maths across
class, school and sex.

``` r
wb = OMESurvey::to_sheet_single_survey_question(data = data,
                                                question_column = 5,
                                                demographic_columns = 2:4,
                                                sheet = 'Table 1')
openxlsx::saveWorkbook(wb, file = 'example1.xlsx')
```

If wanted you can use other survey questions as demographic columns to
see the relationship between two questions.

``` r
wb = OMESurvey::to_sheet_single_survey_question(data = data,
                                                question_column = 5,
                                                demographic_columns = 6:8,
                                                sheet = 'Table 2')
openxlsx::saveWorkbook(wb, file = 'example2.xlsx')
```

------------------------------------------------------------------------

### Summary across a theme

Choose a theme and create a summary table of survey responses in the
theme. (Each theme must have the same allowable values, i.e. agree,
disagree, etc)

``` r
wb = OMESurvey::to_sheet_theme_summary(data = data, theme = 'Theme 1')
openxlsx::saveWorkbook(wb, file = 'example3.xlsx')
```

------------------------------------------------------------------------

### An excel document across all questions

We can use the above functions to make an excel workbook with multiple
sheets (one for each question), and add hyperlinking to a table of
contents sheet (using
[`add_TOC_sheet()`](https://jake-powell.github.io/OMESurvey/reference/add_TOC_sheet.md)).

``` r
question_columns = 5:15
demographic_columns = 2:4
wb <- openxlsx::createWorkbook()
for(i in 1:length(question_columns)){
  wb = to_sheet_single_survey_question(data = data,
                                       question_column = question_columns[i],
                                       demographic_columns = demographic_columns,
                                       sheet = paste0('Table ',i),
                                       wb = wb
  )
}

# Add TOC sheet (with hyperlinks to individual tables)
questions = names(data)[question_columns] |>
  stringr::str_replace_all('__',': ') |>
  stringr::str_replace_all('_',' ') |>
  stringr::str_replace_all('XX','?') 
wb = add_TOC_sheet(wb, link_text = questions)
openxlsx::saveWorkbook(wb, file = 'all_questions_with_TOC.xlsx', overwrite = T)
```

------------------------------------------------------------------------
