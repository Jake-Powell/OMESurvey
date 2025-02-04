#' Missingness report
#'
#'@description
#'Create a report on the missingness of entries in a survey.
#'
#' @param survey_data The cleaned survey data
#' @param survey_name The name of the survey
#' @param report_kind Either 'interactive'(.html) or 'static'(.docx)
#' @param output_dir The path of the output directory
#' @param reference_docx Path to a .docx file whose style (design) the report copies
#' @param participant_ID column name of the column in `survey_data` corresponding to the participant ID.
#' @param demographic_columns column names (or indices) of the columns in `survey_data` corresponding to demographic columns. These will be used to compare missingness across the demographics.
#' @param output_file  the filename of the output. If `NULL` we use paste0(survey_name, '_missingness_report', "extension") as the filename.
#'
#' @return html/word report
#' @export
#'
#' @examplesIf FALSE
#' data = OMESurvey::survey_example
#'
#'# Run with no missingness.
#'OMESurvey::create_missingness_report(survey_data = data,
#'                                      survey_name = 'example data',
#'                                      participant_ID = "Barcode_ID",
#'                                      report_kind = "interactive",
#'                                      demographic_columns = NULL,
#'                                      output_dir = NULL,
#'                                      output_file = NULL,
#'                                      reference_docx = NULL)
#'
#' # Convert 'N-A' to NA, to imply missing
#' for(i in 1:ncol(data)){
#'   data[,i][data[,i] == 'N-A'] = NA
#' }
#'
#' # Run with missingness.
#' OMESurvey::create_missingness_report(survey_data = data,
#'                                      survey_name = 'example data',
#'                                      report_kind = "interactive")
#' # Knit to word.
#' OMESurvey::create_missingness_report(survey_data = data,
#'                                      survey_name = 'example data',
#'                                      report_kind = "static",
#'                                      output_file = 'example_word.docx',
#'                                      output_dir = '/Users/jakepowell/OME/')
#'
#' # Knit to word with styling. (using example template from within the OMESurvey package)
#' OMESurvey::create_missingness_report(survey_data = data,
#'                                      survey_name = 'example data',
#'                                      participant_ID = "Barcode_ID",
#'                                      report_kind = "static",
#'                                      reference_docx = paste0(
#'                                      system.file(package = "OMESurvey"), "/template.docx"
#'                                      ),
#'                                      output_file = 'example_word_styled.docx',
#'                                      output_dir = '/Users/jakepowell/OME/')
#'
#'@details
#' Note that currently if report_kind = 'static', then the tables and plots are screenshots of the interactive plots (using the webshot package). Moreover, chromote version 0.4.0 is needed to avoid rendering issues.
#'
create_missingness_report <- function(survey_data,
                                      survey_name,
                                      participant_ID = 'Barcode_ID',
                                      report_kind = 'interactive',
                                      demographic_columns = NULL,
                                      output_dir = NULL,
                                      output_file = NULL,
                                      reference_docx = NULL

){



  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(survey_name)){
      output_file = 'missingness_report.html'
    }
    else if(is.null(output_file) & !is.null(survey_name)){
      output_file = paste0(survey_name, '_missingness_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(survey_name)){
      output_file = 'missingness_static.docx'
    }
    else if(is.null(output_file) & !is.null(survey_name)){
      output_file = paste0(survey_name, '_missingness_static.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }


  # 2) Render basic stats_static document.
  if(report_kind == 'static'){
    if(!is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/missingness_report.Rmd"),
                        params = list(survey_data = survey_data,
                                      survey_name = survey_name,
                                      report_kind = report_kind,
                                      participant_ID = participant_ID,
                                      demographic_columns = demographic_columns,
                                      output_dir = output_dir),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }else{
      rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/missingness_report.Rmd"),
                        params = list(survey_data = survey_data,
                                      survey_name = survey_name,
                                      report_kind = report_kind,
                                      participant_ID = participant_ID,
                                      demographic_columns = demographic_columns,
                                      output_dir = output_dir),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/missingness_report.Rmd"),
                      params = list(survey_data = survey_data,
                                    survey_name = survey_name,
                                    report_kind = report_kind,
                                    participant_ID = participant_ID,
                                    demographic_columns = demographic_columns,
                                    output_dir = output_dir),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}


#' Initial analysis report
#'
#'@description
#'Create a report on the initial finding of a survey.
#'
#'
#'
#' @param survey_data The cleaned survey data
#' @param survey_name The name of the survey
#' @param report_kind Either 'interactive'(.html) or 'static'(.docx) for the report type
#' @param output_dir The path of the output directory
#' @param reference_docx Path to a .docx file whose style (design) the report copies.
#' @param participant_ID column name of the column in `survey_data` corresponding to the participant ID.
#' @param demographic_columns column names (or indices) of the columns in `survey_data` corresponding to demographic columns. These will be used to compare missingness across the demographics.
#' @param output_file  the filename of the output. If `NULL` we use paste0(survey_name, '_initial_analysis_report', "extension") as the filename.
#'
#' @return html/word report
#' @export
#'
create_initial_analysis_report <- function(survey_data,
                                      survey_name,
                                      participant_ID = 'Barcode_ID',
                                      report_kind = 'interactive',
                                      demographic_columns = NULL,
                                      output_dir = NULL,
                                      output_file = NULL,
                                      reference_docx = NULL

){



  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(survey_name)){
      output_file = 'initial_analysis_report.html'
    }
    else if(is.null(output_file) & !is.null(survey_name)){
      output_file = paste0(survey_name, '_missingness_interactive.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(survey_name)){
      output_file = 'missingness_static.docx'
    }
    else if(is.null(output_file) & !is.null(survey_name)){
      output_file = paste0(survey_name, '_missingness_static.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }


  # 2) Render basic stats_static document.
  if(report_kind == 'static'){
    if(!is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/Initial_analysis_report.Rmd"),
                        params = list(survey_data = survey_data,
                                      survey_name = survey_name,
                                      report_kind = report_kind,
                                      participant_ID = participant_ID,
                                      demographic_columns = demographic_columns,
                                      output_dir = output_dir),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }else{
      rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/Initial_analysis_report.Rmd"),
                        params = list(survey_data = survey_data,
                                      survey_name = survey_name,
                                      report_kind = report_kind,
                                      participant_ID = participant_ID,
                                      demographic_columns = demographic_columns,
                                      output_dir = output_dir),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "OMESurvey"), "/markdown_reports/Initial_analysis_report.Rmd"),
                      params = list(survey_data = survey_data,
                                    survey_name = survey_name,
                                    report_kind = report_kind,
                                    participant_ID = participant_ID,
                                    demographic_columns = demographic_columns,
                                    output_dir = output_dir),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
