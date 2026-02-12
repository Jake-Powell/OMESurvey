#' Produce html document summarising a single survey
#'
#' Produce a summary html document of all responses in an OME survey,
#' based on preprocessed data and the data dictionary.
#' Structure of the summary document is encoded in specified columns of the data dictionary.
#'
#' Here is some text for the details. Needs to say something about the arguments
#' est_char_vars,est_char_types,est_char_values,est_char_statements.
#' And possibly quiet, show, rmd too.
#'
#' @param data_path path to the survey data (assumed to be xls/xlsx).
#' @param dict_path,dict_sheet path to the data dictionary (assumed to be xls/xlsx) and name of sheet in that file to use.
#' @param output_file name of output html file (needs to end with ".html", defaults to "<name of data file>_summary.html")
#' @param output_dir (optional) directory to save the output file to. Defaults to current working directory. Ignored if `output_file` is not specified.
#' @param output_title (optional) text string to use as title for html report output
#' @param output_author (optional) text string to use as author for html report output
#' @param output_date (optional) text string to use as date for html report output
#' @param overwrite (optional) whether to overwrite `output_file` if it already exists. Defaults to FALSE for safety but in use probably TRUE will be more typical.
#' @param est_chars_path path to establishment characteristics data (xls/xlsx).
#' @param est_chars_sheet name of sheet in est't chars to use.
#' @param est_char_vars,est_char_types,est_char_values,est_char_statements specification of establishment characteristics to use, see Details (write).
#' @param quiet (optional) whether to quieten the Rmd rendering information. Defaults to TRUE; FALSE useful for testing.
#' @param show (optional) whether to show intermediate results in the RStudio Viewer. Defaults to FALSE; unclear when TRUE might be useful.
#' @param rmd (optional) path to .Rmd document to use for report-making. Using anything other than the default should be rare.
#'
#' @returns a string giving the path of the saved .html document
#' @export
#'
#' @examples #No examples yet - probably difficult and low priority,
#' # given the need to have data and a dictionary and that we produce a file.
render_survey_summary <- function(data_path,
                                  dict_path,
                                  dict_sheet,
                                  output_dir = getwd(),
                                  output_file = NULL,
                                  output_title = NULL,
                                  output_author = NULL,
                                  output_date = NULL,
                                  overwrite = FALSE,
                                  est_chars_path=NULL,
                                  est_chars_sheet=NULL,
                                  est_char_vars=NULL,
                                  est_char_types=NULL,
                                  est_char_values=NULL,
                                  est_char_statements=NULL,
                                  quiet = TRUE,
                                  show = FALSE,
                                  rmd = NULL) {

  # check that the rmd being pointed to exists, or if not supplied point to the default
  if (is.null(rmd)) {
    rmd <- system.file("markdown_reports/survey_summary_report.Rmd", package="OMESurvey")
    if (!nzchar(rmd)) {
      stop("Template Rmd not found in installed package. Did you put it in inst/markdown_reports/ and reinstall the package?")
    }
  } else {
    if (!file.exists(rmd)) stop("rmd not found")
  }

  message("After system.file(), rmd = ", rmd)
  message("file.exists(rmd) = ", file.exists(rmd))


  # check that data_path is a real file
  if (!file.exists(data_path)) {
    stop("File not found: ", data_path)
  }

  if (!file.exists(dict_path)) {
    stop("File not found: ", dict_path)
  }


  # default output filename if not provided
  if (is.null(output_file)) {
    base <- tools::file_path_sans_ext(basename(data_path))
    output_file <- paste0(base, "_summary.html")
  }

  # ensure output_dir exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # compute full output path (what rmarkdown::render will create)
  out_full <- normalizePath(file.path(output_dir, output_file), mustWork = FALSE)

  # if file exists, handle according to overwrite flag and interactivity
  if (file.exists(out_full)) {
    if (!overwrite) {
      if (interactive()) {
        ans <- tolower(trimws(readline(
          paste0("Output file ", out_full, "\nalready exists. Overwrite? [y/N]: ")
        )))
        if (nzchar(ans) && substr(ans, 1, 1) == "y") {
          # proceed and overwrite
        } else {
          stop("Rendering cancelled by user to avoid overwriting existing file.")
        }
      } else {
        stop("Output file already exists. Set overwrite = TRUE to allow overwriting in non-interactive sessions.")
      }
    } else {
      # overwrite = TRUE: proceed (optionally remove first)
      # file.remove(out_full) # optional: remove before render
    }
  }

  # set some default params if they aren't supplied
  output_title <- if (!is.null(output_title) && nzchar(output_title)) {
    output_title
  } else {
    paste("Report for", basename(data_path))
  }

  output_author <- if (!is.null(output_author) && nzchar(output_author)) {
    output_author
  } else {
    NULL   # NULL to default to no author element
  }

  output_date <- if (!is.null(output_date) && nzchar(output_date)) {
    output_date
  } else {
    format(Sys.Date(), "%d %b %Y")
  }


  # set up params
  params <- list(
    data_path = data_path,
    dict_path = dict_path,
    dict_sheet = dict_sheet,
    est_chars_path = est_chars_path,
    est_chars_sheet = est_chars_sheet,
    est_char_vars = est_char_vars,
    est_char_types = est_char_types,
    est_char_values = est_char_values,
    est_char_statements = est_char_statements,
    output_title = output_title,
    output_author = output_author,
    output_date = output_date
  )

  # optionally suppress RStudio Viewer during render
  if (!isTRUE(show)) {
    old_viewer <- getOption("viewer")
    on.exit(options(viewer = old_viewer), add = TRUE)
    options(viewer = function(url) invisible())
  }

  message("Before render(), rmd = ", rmd)
  message("file.exists(rmd) = ", file.exists(rmd))


  out_path <- rmarkdown::render(
    input = rmd,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    envir = new.env(parent = globalenv()), # isolate execution
    quiet = quiet
  )

  return(normalizePath(out_path))
}



#function - given data (dat) of 1 or 2 or 3 columns,
# make stacked bar charts showing relative frequencies of values in the first variable,
# splitting into separate bars by the second variable if present
# and facetting by third variable if present



#' Make stacked bar chart (possibly split into separate bars and facetted) in the OME style. (Dave's version.)
#'
#' `initial_bar` builds a proportional (stacked, filled) bar chart from a
#' tidy data frame of survey responses. The first column is treated as the
#' response factor (fill), the optional second column as the grouping/subdivision,
#' and the optional third column is used for faceting.
#' Percentages of bars are overlaid on the plot and number of observations/responses are shown at the end of each bar.
#' The function returns a `ggplot` object ready for further customization or direct printing.
#'
#' @param dat data frame of 1, 2 or 3 variables. Bar charts show relative frequencies of the first variable,
#' split into separate bars according to the second variable (if present),
#' and facetted by the third variable (if present).
#' All variables should be factors (but characters might work?).
#' @param percCut numeric scalar (0-100). Cutoff below which percentages are not shown in bar segments. Default `5`.
#' @param colo (optional but recommended) vector of colours to use for the fill scale (character vector of colours,
#' length the same as the number of levels of the factor that is the first variable of `dat`.)
#' If `NULL` (default) the pallete from `OMESurvey::get_OME_colours(type='distinct')` is used.
#' When `na.rm = FALSE` a grey colour is prepended for the "No response" level.
#' Note that percentage labels are in white, so the pallete needs to work with that.
#'  (... or this function needs upgrading!)
#' @param na.rm logical. If `TRUE` then remove `NA` responses from the data;
#' if `FALSE` (default) then they are converted to "(No response)" and treated as an additional allowed response.
#' @param horiz logical (default FALSE) determining whether to coord_flip() so that bars are horizontal and place legend below the plot.
#' @param text_scale positive number (default 1) for scaling the size of the percentage and #response labels.
#' @param fillLabText Optional character. Label for the fill/legend.
#'   If `NULL` (the default) the legend title is removed (no label).
#'   If `""` the name of the first variable in `dat` is used.
#' @param xLabText optional text for labelling the second-variable-in-`dat` axis.
#'   If `NULL` (the default) the label is removed (no label).
#'   If `""` the name of the first variable in `dat` is used.
#' @param yLabText optional text for labelling the 'proportion' axis. Default `"Proportion of responses"`.
#' @param titleText optional text for plot title. With default `NULL` the title is removed.
#' @param facet_labels Optional named character vector used as a labeller for
#'   facets (names are original facet values, values are labels to display).
#'   Default `NULL` uses the values of that variable.
#' @param facet_layout Optional character. If `"1row"` the facets are
#'   arranged in a single row; otherwise default facet layout is used.
#'   (`"1col"` option planned but not yet supported.)
#'
#' @returns ggplot object - a stacked proportional bar chart.
#'
#' @details
#' Percentages are displayed in segments that comprise at least `percCut` of their bar;
#' the percentage displayed is this rounded to a whole number.
#'
#' @export
#'
#' @examples # NEED A SHORT EXAMPLE
initial_bar = function(dat, percCut=NULL, colo=NULL, na.rm=FALSE,
                       horiz=FALSE, text_scale=1,
                       fillLabText=NULL, xLabText=NULL,
                       yLabText="Proportion of responses",
                       titleText=NULL,
                       facet_labels=NULL, facet_layout=NULL){

  if (ncol(dat)==1){
    dat[,2] <- ""
    xLabText <- NULL
  }else if (identical(xLabText,"")){
    xLabText <- colnames(dat)[2]
  }

  if (ncol(dat)==2){
    dat[,3] <- ""
  }#else{ }

  if (identical(fillLabText,"")){
    fillLabText <- colnames(dat)[1]
  }


  colnames(dat)[1:3] = c("var_name", "var_subdivide", "var_facet")

  has_facet <- any(dat$var_facet != "")

  # convert percCut to proportion (with defualt 5%)
  percCut <- ifelse(is.null(percCut), 0.05, percCut / 100)

  # in normal usage this should not be needed, but keeping it because I'm scared it's used somewhere
  colo <- convert_colo(colo)

  # if removing NAs filter them out and sort out a colour pallete if needed
  if(na.rm){
    dat <- dat |> dplyr::filter(!is.na(var_name))
    if (is.null(colo)){
      colo <- dat |> dplyr::pull(var_name) |> nlevels() |> get_OME_colours(type='distinct')
    }
  }
  else{ # if not removing NAs convert them to a proper level, sort out a colour pallete if needed,
    # then prepend grey to the pallete for the NAs
    dat <- dat |>
      dplyr::mutate(dplyr::across(1, ~ .x |>
                                    forcats::fct_expand("No response") |>
                                    forcats::fct_na_value_to_level(level = "No response") |>
                                    forcats::fct_relevel("No response", after=0)))
    if (is.null(colo)){
      colo <- (dat |> dplyr::pull(var_name) |> nlevels() - 1) |> get_OME_colours(type='distinct')
    }
    colo = c("grey",colo)
  }


  thePlot <-
    ggplot2::ggplot(dat, ggplot2::aes(x=var_subdivide, by=var_subdivide, fill=var_name)) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.title.position = "plot")

  if (horiz) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="bottom",
        panel.grid.major.x = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    fill_guide <- ggplot2::guide_legend(nrow=1, reverse=TRUE)
  } else {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="right",
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        panel.grid.minor = ggplot2::element_blank()
      )
    fill_guide <- ggplot2::guide_legend()
  }

  fill_labels <- function(x) stringr::str_wrap(x,width=20)

  if (is.null(colo)){
    thePlot <- thePlot +
      ggplot2::scale_fill_discrete(labels=fill_labels, guide=fill_guide)
  } else {
    thePlot <- thePlot +
      ggplot2::scale_fill_manual(values=colo, labels=fill_labels, drop=FALSE, guide=fill_guide)
  }


  dat_sum <-
    dat |>
    dplyr::summarise(
      num_resp = dplyr::n(),
      .by = c(var_facet,var_subdivide)
      ) |>
    dplyr::mutate(num_resp = num_resp |> scales::label_comma(prefix="(", suffix=")") )

  # alternative approach is to put this as the data argument of the geom_text below and delete calculation of dat_sum above
  #  data = ~ summarise(.x, num_resp = n(), .by=question) %>% mutate(num_resp = num_resp %>% (scales::label_comma(prefix="(", suffix=")")) ),

  vjust_val <- if (horiz) 0.5 else 0
  hjust_val <- if (horiz) 0 else 0.5

  # add text for the "(N)" labels
  thePlot = thePlot +
    ggplot2::geom_text(
      ggplot2::aes(
        x = var_subdivide,
        y = 1.02,
        by = NULL,
        fill = NULL,
        label = num_resp
      ),
      dat_sum,
      colour = "black",
      vjust = vjust_val,
      hjust = hjust_val,
      size = 3*text_scale
      )

  # add axis labels, fill/legend label, title
  # and set breaks/labels on the proportion axis
  thePlot = thePlot +
    ggplot2::labs(x=xLabText, y=yLabText,
                  fill=fillLabText, title=titleText) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.25),
                                labels=scales::percent_format(accuracy=1),
                                minor_breaks=seq(0,1,0.05))

  # add percentage labels on bar segments
  thePlot <- thePlot +
    ggplot2::geom_text(
      ggplot2::aes(
        x = var_subdivide,
        label = ifelse(ggplot2::after_stat(prop) < percCut,
                       "",
                       scales::percent(ggplot2::after_stat(prop), accuracy = 1))
      ),
      stat = "prop",
      position = ggplot2::position_fill(vjust = 0.5),
      colour = "white",
      size = 3 * text_scale
    )

  # Add faceting if needed
  if (has_facet) {
    facet_args <- list(facets = ~var_facet)

    # Apply custom labels if provided
    if (!is.null(facet_labels)) {
      facet_args$labeller <- ggplot2::labeller(var_facet = facet_labels)
    }

    # Apply layout control if requested
    if (!is.null(facet_layout) && facet_layout == "1row") {
      facet_args$nrow <- 1
    }

    thePlot <- thePlot + do.call(ggplot2::facet_wrap, facet_args)
  }



  if (horiz) {
    thePlot <- thePlot + ggplot2::coord_flip(ylim = c(NA, 1.1))
  } else {
    thePlot <- thePlot + ggplot2::coord_cartesian(ylim = c(NA, 1.1))
  }


  return(thePlot)

}



#' Make a stacked bar chart summarising many survey questions
#'
#' Longer version - INCOMPLETE
#'
#' @param dat INCOMPLETE
#' @param labels_vec INCOMPLETE
#' @param percCut INCOMPLETE
#' @param colo INCOMPLETE
#' @param order_values INCOMPLETE
#' @param titleText INCOMPLETE
#'
#' @returns INCOMPLETE
#'
#' @examples
#' # NEED A SHORT EXAMPLE HERE
#'
plot_many_questions <- function(dat, labels_vec, percCut=2,
                                colo=NULL, order_values = NULL,
                                titleText=NULL){

  # In the context of using this for the survey_summary_report(.Rmd) this should be unnecessary, but maybe leave it for other use?
  colo <- convert_colo(colo)

  # pivot the many questions/variables to long question&response form,
  # reorder questions according to proportion of responses that are in order_values
  dat <-
    dat |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "question",
                        values_to = "Response") |>
    dplyr::mutate(question = as.factor(question),
                  order_flag = as.character(Response) %in% order_values,
                  question = forcats::fct_reorder(question, order_flag, .fun=mean, .na_rm=TRUE))

  # get plotting
  thePlot <-
    dat |>
    dplyr::select(Response, question) |>
    initial_bar(horiz=TRUE,
                percCut=percCut,
                fillLabText=NULL,
                yLabText=NULL,
                xLabText=NULL,
                colo=colo,
                titleText=titleText) +
    ggplot2::scale_x_discrete(labels = stringr::str_wrap(labels_vec, width=30))

  # now use cowplot to centre the legend horizontally underneath the whole figure,
  # not just under the plotting area

  if (requireNamespace("cowplot", quietly=TRUE)){
    legend <- cowplot::get_legend(thePlot)
    thePlot_clean <- thePlot + ggplot2::theme(legend.position = "none")
    thePlot <- cowplot::plot_grid(thePlot_clean, legend,
                                  ncol=1, rel_heights=c(1,0.15))
  } else {
    # no warning/similar at this point that cowplot is required to centre legends under plots
  }

  return(thePlot)
}



#' Convert shorthand colour name to a colour vector
#'
#' `convert_colo` accepts `NULL`, a character vector of colour hex codes, or a
#' single keyword and returns a character vector of hex colours (or `NULL`).
#' This helper is intended for normalising user-supplied colour arguments into
#' a vector usable by `ggplot2::scale_fill_manual()` and similar functions.
#' Tidies a situation where a variable (usually called `colo`) is passed around
#' as either a string of hex code colours or a shortcut name/keyword.
#' Mainly kept for backwards-compatibility - in most contexts this function
#' SHOULD NOT BE USED, as `OMESurvey::get_OME_colours()` is more direct &
#'  transparent and less hard-coded.
#'
#' @param colo `NULL`, a character vector of colour hex codes, or a single
#'   character keyword. If it is a character of length 1 it is expanded into a
#'   character vector of colours (see Details). Otherwise it is returned
#'   unchanged.
#'
#' @returns A character vector of hex colour codes, or `NULL` if `colo` is
#'   `NULL`.
#'
#' @details The following single-string keywords are recognised and expanded:
#' \describe{
#'   \item{`"likert5"`}{5-colour palette: `c("#009BC1","#08607E","#10263B","#732C53","#D7336C")`}
#'   \item{`"scale4"`}{4-colour palette: `c("#009BC1","#08607E","#732C53","#D7336C")`}
#'   \item{`"ynsn3"`}{3-colour palette: `c("#009BC1","#10263B","#D7336C")`}
#'   \item{`"scale2"`}{2-colour palette: `c("#009BC1","#D7336C")`}
#' }
#' Any other single string is returned as-is, i.e. it DOESN'T throw an error/warning!
#'
#' @examples
#' convert_colo(NULL)            # NULL -> NULL
#' convert_colo(c("#000000","#FFFFFF"))  # vector -> returned unchanged
#' convert_colo("likert5")       # returns the 5-colour likert palette
#' convert_colo("an_odd_name")       # Unintended use: unknown and so returned unchanged
#' convert_colo(c("thing","amijig"))  # Unintended use: vector -> returned unchanged
#'
#' @export

convert_colo <- function(colo){
  if (is.null(colo)) return(NULL)

  if (length(colo)>1) return(colo)

  if(length(colo)==1) {
    colo <- switch(colo,
                   "likert5" = c("#009BC1", "#08607E", "#10263B", "#732C53", "#D7336C"),
                   "scale4" = c("#009BC1", "#08607E", "#732C53", "#D7336C"),
                   "ynsn3" = c("#009BC1", "#10263B", "#D7336C"),
                   "scale2" = c("#009BC1", "#D7336C"),
                   colo
    )
  }
  return(colo)
}
