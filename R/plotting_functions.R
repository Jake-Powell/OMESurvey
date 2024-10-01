#' plot survey theme result
#'
#' @param data survey data
#' @param theme A string containing the theme name (with or without "_" as spacing).
#' @param theme_columns column index relating to theme
#' @param kind what kind of object we want to be returned. Allowable values are "ggplot", "plotly" or "data.frame".
#' @param rm99 Flag (TRUE/FALSE) for whether we want to set '99' values to NA to suppress multiple values as an option. Default is TRUE.
#' @param survey_values survey_values
#'
#' @return either a plotly object, ggplot object or a data frame depending on `kind`.
#' @export
#'
plot_theme <- function(data,
                       theme = NA,
                       theme_columns = NA,
                       kind = 'ggplot',
                       rm99 = TRUE,
                       survey_values = OMESurvey::survey_values){
  if(!is.na(theme)){
    theme_columns = get_theme_columns(data, theme)
  }

  data = data[,theme_columns]
  if(rm99){
    for(i in 1:ncol(data)){
      data[which(data[,i] == '99'),i] = NA
    }
  }

  ##########
  # 1) Extract the theme and question names from the data.
  ##########
  theme_and_questions = get_theme_and_questions(names(data))
  theme = theme_and_questions$theme
  questions = theme_and_questions$questions

  ##########
  # 2) Format data
  ##########
  freq_tables = lapply(1:ncol(data), function(index){
    d = data[,index] |> table() |> data.frame()
    d = data.frame(rep(questions[index], nrow(d)), d)
    d$percent = d$Freq / sum(d$Freq) *100
    d
  })
  data_format = do.call(rbind, freq_tables)
  names(data_format) = c('question', 'answer', 'count', 'percent')

  unique_values = data_format$answer |> as.character() |> unique() ; unique_values = unique_values[!is.na(unique_values)] |> as.character()

  # Generate the expected values if not given.
  best_index = lapply(survey_values, function(x){
    return(sum(x %in% unique_values))
  }) |> unlist() |> which.max()
  expected_values = survey_values[[best_index[1]]]
  data_format$answer = factor(data_format$answer, levels = expected_values)

  if(kind == 'data.frame'){
    return(data_format)
  }


  ##########
  # 1) Create ggpplot
  ##########
  colo = get_OME_colours(n = length(expected_values), type = 'contrast')


  if(kind == 'ggplot'){
    p <- ggplot2::ggplot(data = data_format,
                  ggplot2::aes(x = question, y = count, fill = factor(answer))
                )+
      ggplot2::geom_bar(stat="identity") +
      ggplot2::scale_fill_manual(values = colo) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      ggplot2::xlab('') +
      ggplot2::ylab('Number of responses') +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.title=ggplot2::element_blank())
    return(p)

  }

  if(kind == 'plotly'){
    data_format$hover = paste0(data_format$answer, '<br>',
                              data_format$count, ' Responses', '<br>',
                              data_format$percent |> round(1), '%')
    data_format$wrap_q = stringr::str_wrap(data_format$question,width = 20) |>
      stringr::str_replace_all('\n', '<br>')

    fig = data_format |>
      plotly::group_by(answer) |>
      plotly::plot_ly(type = 'bar',
                  y = ~wrap_q,
                  x = ~count,
                  color = ~answer,
                  colors = colo,
                  orientation = 'h',
                  hovertext = ~hover, hoverinfo = 'text'
                  # showlegend = TRUE,
                  # marker = list(color = ~color)
    ) |>
      plotly::layout(xaxis = list(title = "Number of responses"),
                     yaxis = list(title = ''), barmode = 'stack'
                     # hovermode = 'y unified'
      )
    return(fig)

  }

}



plot_theme_by_demographic <- function(data,
                                      theme = NA,
                                      theme_columns = NA,
                                      demographic_column,
                                      agree_values = c('Agree a little', 'Agree a lot'),
                                      kind = 'ggplot',
                                      rm99 = TRUE,
                                      ...){
  ##########
  # 1) Setup.
  ##########
  # A) Get theme columns (if necessary)
  if(!is.na(theme)){
    theme_columns = get_theme_columns(data, theme)
  }

  # B) Slice the part of the data we need.
  data = data[,c(demographic_column, theme_columns)]
  if(rm99){
    for(i in 2:ncol(data)){
      data[which(data[,i] == '99'),i] = NA
    }
  }

  # C) Get formatted theme and questions.
  theme_and_questions = get_theme_and_questions(names(data)[-1])
  theme = theme_and_questions$theme
  questions = theme_and_questions$questions
  no_questions = length(questions)
  demographic = names(data)[1] |> stringr::str_replace_all('_', ' ')


  ##########
  # 2) Format data
  ##########
  demo_values = data[,1] |> unique() ; demo_values  = demo_values[!is.na(demo_values)]

  data_format = lapply(demo_values, function(demo_value){
    d = data[data[,1] %in% demo_value,-1]
    demo_size = nrow(d)
    vals = rep(0,no_questions)
    for(i in 1:no_questions){
      vals[i] = sum(d[,i] %in% agree_values)
    }
    percent = vals/demo_size*100

    return(c(demo_value, vals, percent))
  }) |> data.frame() |> t() |> data.frame()
  names(data_format) = c('demographic', questions, paste0(questions,'_percent_'))
  data_format = stats::reshape(data_format, idvar = 'demographic', varying = list(2:ncol(data_format)), v.names = "value", timevar = "question", times = names(data_format)[-1], direction = "long")
  type = c(rep('count', nrow(data_format)/2), rep('percent',  nrow(data_format)/2) )
  data_format$type = type
  data_format$question = data_format$question |> stringr::str_remove_all('_percent_')
  data_format = data_format[,c(1,2,4,3)]
  rownames(data_format) = 1:nrow(data_format)
  data_format$value  = data_format$value |> as.numeric()
  if(kind == 'table'){
    names(data_format)[1] = 'demographic'
    return(data_format)
  }

  ##########
  # 3) Create plots
  ##########
  colo = get_OME_colours(n = length(demo_values), type = 'contrast')

  if(kind == 'ggplot'){
    p <- ggplot2::ggplot(data = data_format[data_format$type == 'percent',],
                         ggplot2::aes(x = question, y = value, fill = factor(demographic))
    )+
      ggplot2::geom_bar(position="dodge", stat="identity") +
      ggplot2::scale_fill_manual(values = colo,
                                 name = demographic |> stringr::str_to_sentence()) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
      ggplot2::xlab('') +
      ggplot2::ylab('Agree or strongly agree (%)') +
      ggplot2::coord_flip() +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
    return(p)

  }

  if(kind == 'plotly'){
    data_format$hover = paste0(data_format$answer, '<br>',
                               data_format$count, ' Responses', '<br>',
                               data_format$percent |> round(1), '%')
    data_format$wrap_q = stringr::str_wrap(data_format$question,width = 20) |>
      stringr::str_replace_all('\n', '<br>')

    fig = data_format |>
      plotly::group_by(answer) |>
      plotly::plot_ly(type = 'bar',
                      y = ~wrap_q,
                      x = ~count,
                      color = ~answer,
                      colors = colo,
                      orientation = 'h',
                      hovertext = ~hover, hoverinfo = 'text'
                      # showlegend = TRUE,
                      # marker = list(color = ~color)
      ) |>
      plotly::layout(xaxis = list(title = "Number of responses"),
                     yaxis = list(title = ''), barmode = 'stack'
                     # hovermode = 'y unified'
      )
    return(fig)

  }

}

