# Create possible survey values.
survey_values = list(agree5 = c('Agree a lot', 'Agree a little', 'Neither', 'Disagree a little', 'Disagree a lot'),
                     time5 = c('Almost always','Most of the time','Half the time','Some of the time','Almost never'),
                     often3 = c('Often', 'Sometimes', 'Never'),
                     quant4 = c('A lot', 'Sometimes', 'Not much', 'Did not answer'),
                     good_bad_4 = c('Good', 'Okay', 'Bad', 'Did not answer'),
                     like4 = c('Like', 'Okay', "Don't like", 'Did not answer'),
                     NS_choice = c('1','2')
)
usethis::use_data(survey_values, overwrite = TRUE)
