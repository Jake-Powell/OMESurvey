#' plot survey theme result
#'
#'
#' @param data survey data
#' @param theme A string containing the theme name (with or without "_" as spacing).
#' @param theme_columns column index relating to theme. This input is overwritten by the columns found by parameter 'theme' if inputte.
#' @param kind what kind of object we want to be returned. Allowable values are "ggplot", "plotly" or "data.frame".
#' @param rm99 Flag (TRUE/FALSE) for whether we want to set '99' values to NA to suppress multiple values as an option. Default is TRUE.
#' @param survey_values A list of survey answer types and their allowble values. Default is OMESurvey::survey_values.
#' @param do_percent Flag (TRUE/FALSE) for whether we want to show the number of responses (F) or the percentage of responses (T).
#'
#'
#' @return either a plotly object, ggplot object or a data frame depending on `kind`.
#' @export
#'
#'@examplesIf FALSE
#' # Load in some example survey data.
#' data = OMESurvey::survey_example
#'
#' # Create the theme plot.
#' p = OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')
#'
#'
#' @details
#' See \code{vignette("Create-survey-figures", package = "OMESurvey")} article for further examples.
#'
#'
plot_theme <- function(data,
                       theme = NA,
                       theme_columns = NA,
                       kind = 'ggplot',
                       rm99 = TRUE,
                       survey_values = OMESurvey::survey_values,
                       do_percent = FALSE){
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
    d = data.frame(a=rep(questions[index], nrow(d)), d)
    d$percent = d$Freq / sum(d$Freq) *100
    names(d) = c(letters[1:4])
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

  # Check for extra values in the data that are not included in the expected values.
  missing = unique_values[!unique_values %in% expected_values]
  if(length(missing > 0)){
    warning(paste0('The expected answers are: "',
                   paste0(expected_values, collapse = '", "'),
                   '" which does not include the following found in the data: "',
                   paste0(missing, collapse = '", "'), '"'))
  }
  data_format$answer = factor(data_format$answer, levels = expected_values)

  if(kind == 'data.frame'){
    return(data_format)
  }


  ##########
  # 1) Create ggpplot
  ##########
  colo = get_OME_colours(n = length(expected_values), type = 'contrast')
  xaxis_label = 'Number of responses'
  if(do_percent){
    data_format$count = data_format$percent
    xaxis_label = 'Percentage of responses'
    }

  if(kind == 'ggplot'){
    p <- ggplot2::ggplot(data = data_format,
                  ggplot2::aes(x = question, y = count, fill = factor(answer))
                )+
      ggplot2::geom_bar(stat="identity") +
      ggplot2::scale_fill_manual(values = colo) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
      ggplot2::xlab('') +
      ggplot2::ylab(xaxis_label) +
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
      plotly::layout(xaxis = list(title = xaxis_label),
                     yaxis = list(title = ''), barmode = 'stack'
                     # hovermode = 'y unified'
      )
    return(fig)

  }

}



#' Plot survey theme result by demographic
#'
#' @inheritParams plot_theme
#' @param demographic_column the index of the demographic column.
#' @param agree_values The answers that correspond to agree.
#'
#' @return either a plotly object, ggplot object or a data frame depending on `kind`.
#' @export
#'
#'@examplesIf FALSE
#' # Load in some example survey data.
#' data = OMESurvey::survey_example
#'
#' # Add a school column using the barcode.
#' school = data$Barcode_ID |> as.character() |>  substr(3, 5)
#' data$school = school
#' school_column = ncol(data)
#'
#' # Create the theme by demographic plot.
#' OMESurvey::plot_theme_by_demographic(data,
#' theme = 'Theme_1',
#' demographic_column = school_column,
#' kind = 'ggplot')
#'
#'
#' @details
#' See `Create survey figures` article for examples.
#'
plot_theme_by_demographic <- function(data,
                                      theme = NA,
                                      theme_columns = NA,
                                      demographic_column,
                                      agree_values = c('Agree a little', 'Agree a lot'),
                                      kind = 'ggplot',
                                      rm99 = TRUE){
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
  if(kind == 'data.frame'){
    names(data_format)[1] = 'demographic'
    return(data_format)
  }

  ##########
  # 3) Create plots
  ##########
  colo = get_OME_colours(n = length(demo_values), type = 'contrast')

  if(kind == 'ggplot'){
    df = data_format[data_format$type == 'percent',]
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = forcats::fct_reorder(question,value), y = value, fill = factor(demographic))
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
    df = data_format[data_format$type == 'percent',]
    df$hover = paste0(df$question, '<br>',
                      names(data)[1] |> stringr::str_to_sentence(),
                      ': ', df$demographic, '<br>',
                      df$value |> round(1), '% of responses')
    df$wrap_q = stringr::str_wrap(df$question,width = 20) |>
      stringr::str_replace_all('\n', '<br>')

    fig = df |>
      plotly::group_by(demographic) |>
      plotly::plot_ly(type = 'bar',
                      y = ~wrap_q,
                      x = ~value,
                      color = ~demographic,
                      colors = colo,
                      orientation = 'h',
                      hovertext = ~hover, hoverinfo = 'text'
                      # showlegend = TRUE,
                      # marker = list(color = ~color)
      ) |>
      plotly::layout(xaxis = list(title = "Agree or strongly agree (%)"),
                     yaxis = list(title = '')
                     # hovermode = 'y unified'
      )
    return(fig)

  }

}

#' Add OME logo to a ggplot object
#'
#' @param p ggplot object.
#' @param type Specifies the type on logo. Either 'bw' or 'standard'.
#' @param position Specifies the logo position. Either 'top left', 'top right', 'bottom left' or 'bottom right'.
#' @param logo_sizing A numeric vector of length 2. The first element defines the amount of vertical spacing the logo uses (default is 11% of the figure height). The second element defines the logo width (min 0, max 1 default is 0.2)
#'
#' @return original plot with the logo added.
#' @export
#'
#'@examplesIf FALSE
#' # Load in some example survey data and create a plot.
#' data = OMESurvey::survey_example
#' p = OMESurvey::plot_theme(data, theme = 'Theme_1', kind = 'ggplot')
#'
#' # Add logo to plot either top right or bottom right.
#' p |> OMESurvey::add_logo(position = 'top right')
#' p |> OMESurvey::add_logo(position = 'bottom right', logo_sizing = c(0.21, 0.3))
#'
#'
#'@details
#'This function currently only works for ggplot objects and always adds the logo outside the original plot (In an extended margin).
#'
#'Note that you cannot use '+' to add this to the ggplot object, as the function returns a non-ggplot object (as we use gridExtra::grid.arrange() to add the logo). This should always be the last step of creating a plot.
#'
#' Play around with the logo sizing parameter until you find your desired logo size.
add_logo <- function(p,
                     type ='',
                     position = 'bottom right',
                     logo_sizing = c(.11,0.2)){
  heights = c(1-logo_sizing[1], logo_sizing[1])

  filepath = paste0(system.file(package = "OMESurvey"), "/logos/OME_whiteBack_with_text.png")
  l = grid::rasterGrob(png::readPNG(filepath), interpolate = TRUE)

  p3 <- ggplot2::ggplot(mapping = ggplot2::aes(x = 0:1, y = 1)) +
    ggplot2::theme_void()

  if(grepl('left', position)){
    p3 = p3 + ggplot2::annotation_custom(l, xmin = 0, xmax = logo_sizing[2])

  }else{
    p3 = p3 + ggplot2::annotation_custom(l, xmin = 1 - logo_sizing[2], xmax = 1)
  }


  if(grepl('bottom', position)){
    return(gridExtra::grid.arrange(p, p3, heights = heights))
  }else{
    return(gridExtra::grid.arrange(p3, p, heights = heights |> rev()))
  }
}


#' Plot OME bar plot
#' @inheritParams plot_theme
#' @param column column index or name to select the data.
#' @param type Either 'vertical' or 'horizontal'. Default is 'horizontal'.
#' @param showValue Flag (TRUE/FALSE) for whether to display the values on the plot.
#' @param ... parameters for table(). I.e whether you want to include NA or not by using useNA = 'always' or 'ifany'.
#'
#' @return either a data.frame, ggplot object or plotly object.
#' @export
#'
#'@examplesIf FALSE
#' # Load in some example survey data and create a plot.
#' data = OMESurvey::survey_example
#'
#' # Create bar plot for column 5.
#' OMESurvey::plot_bar(data, column = 5, kind = 'ggplot')
#'
#'
plot_bar <- function(data,
                     column,
                     type = 'horizontal',
                     kind = 'ggplot',
                     rm99 = TRUE,
                     showValue = FALSE,
                     ...){

  ##########
  # 1) Setup.
  ##########

  # A) Slice the part of the data we need.
  if(is.numeric(column)) column = names(data)[column]
  data = data[[column]]
  if(rm99) data[which(data == '99')] = NA

  ##########
  # 2) Format data
  ##########
  data_format = data |> table(...) |> as.data.frame()
  if(kind == 'data.frame'){
    names(data_format) = c(column |> stringr::str_replace_all('_', ' '), 'Number of responses')
    return(data_format)
  }

  ##########
  # 3) Create plots
  ##########
  colo = get_OME_colours(n = 1, type = 'contrast')

  names(data_format) = c('question','value')
  if(kind == 'ggplot'){
    df = data_format
    if(type == 'vertical'){
      p <- ggplot2::ggplot(data = df, ggplot2::aes(x = forcats::fct_reorder(question,value,
                                                                            .fun = function(x){-x}), y = value))+
        ggplot2::geom_bar(position="dodge", stat="identity", fill = colo) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
        ggplot2::xlab('')+
        ggplot2::ylab('Number of responses') +
        ggplot2::ggtitle(paste0('Responses by "',
                                column |> stringr::str_replace_all('_', ' '),
                                '"') |> stringr::str_wrap(width = 50)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white"),
          panel.background = ggplot2::element_rect(fill = "white"),
          axis.line.x = ggplot2::element_line(color = "black"),
          panel.grid.major.y = ggplot2::element_line(color = "grey",
                                                     size = 0.5,
                                                     linetype = 2)
        )

      if(showValue){
        p = p + ggplot2::geom_text(ggplot2::aes(label=value), vjust=-0.1) +
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$value)*1.1))
      }

      return(p)
    }else{
      p <- ggplot2::ggplot(data = df, ggplot2::aes(x = forcats::fct_reorder(question,value,
                                                                            .fun = function(x){x}), y = value))+
        ggplot2::geom_bar(position="dodge", stat="identity", fill = colo) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
        ggplot2::xlab('') +
        ggplot2::ylab('Number of responses') +
        ggplot2::ggtitle(paste0('Responses by "',
                                column |> stringr::str_replace_all('_', ' '),
                                '"') |> stringr::str_wrap(width = 50)) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "white"),
          panel.background = ggplot2::element_rect(fill = "white"),
          axis.line.y = ggplot2::element_line(color = "black"),
          panel.grid.major.x = ggplot2::element_line(color = "grey",
                                                     size = 0.5,
                                                     linetype = 2)
        )

      if(showValue){
        p = p + ggplot2::geom_text(ggplot2::aes(label=value), hjust=-0.1) +
          ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$value)*1.1))
      }

      return(p)
    }


  }

  if(kind == 'plotly'){
    df = data_format
    df = df[order(df$value),]
    df$question = factor(df$question, levels = df$question)
    df$percent = (df$value / sum(df$value)*100) |> round(2)
    df$hover = paste0(df$question, ': ', df$value, '<br>',
                      df$percent, '% of responses')
    df$wrap_q = stringr::str_wrap(df$question,width = 20) |>
      stringr::str_replace_all('\n', '<br>')
    df$wrap_q =  factor(df$wrap_q, levels = df$wrap_q)
    fig = df |>
      plotly::plot_ly(type = 'bar',
                      y = ~wrap_q,
                      x = ~value,
                      color = colo,
                      colors = colo,
                      orientation = 'h',
                      hovertext = ~hover, hoverinfo = 'text'
                      # showlegend = TRUE,
                      # marker = list(color = ~color)
      ) |>
      plotly::layout(xaxis = list(title = "Number of responses"),
                     yaxis = list(title = '')
                     # hovermode = 'y unified'
      )
    return(fig)

  }

}
