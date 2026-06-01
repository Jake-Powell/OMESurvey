

#' Safely read Excel files, including when open or locked
#'
#' A wrapper around \code{readxl::read_excel()} that attempts to read an Excel
#' file directly, and if that fails (e.g. because the file is open or locked in
#' Excel on Windows), falls back to copying the file to a temporary location and
#' reading from the copy.
#'
#' This improves robustness in workflows where Excel files may be open during
#' execution, such as interactive data preparation or reporting pipelines.
#'
#' @param path A character string giving the path to the Excel file.
#' @param sheet Sheet to read from. Passed to \code{readxl::read_excel()}.
#' @param ... Additional arguments passed to \code{readxl::read_excel()}.
#'
#' @return A tibble containing the data read from the Excel file.
#'
#' @details
#' On some systems (notably Windows), Excel may lock files in a way that prevents
#' them being read by other processes. This function attempts a direct read first,
#' and only falls back to copying the file if that fails.
#'
#' If both the direct read and the copy fail, an error is thrown.
#'
#' @examples
#' \dontrun{
#' # Read normally
#' df <- safe_read_excel("data.xlsx")
#' df <- safe_read_excel("data.xlsx", sheet = 2)
#' }
#'
#' @export
safe_read_excel <- function(path, sheet = NULL, ...) {

  # try direct read first
  out <- try(readxl::read_excel(path, sheet = sheet, ...), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)

  # try copy fallback
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  ok <- file.copy(path, tmp, overwrite = TRUE)

  if (ok) {
    return(readxl::read_excel(tmp, sheet = sheet, ...))
  }

  # LAST fallback: give a more helpful error
  stop(
    "Could not read file:\n",
    path,
    "\n\nThe file is likely open in Excel with an exclusive lock.\n",
    "Please close the file and try again."
  )
}





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
#' @param data_path path to the survey data (assumed to be xls/xlsx/csv).
#' @param dict_path,dict_sheet path to the data dictionary (assumed to be xls/xlsx)
#'  and name of sheet in that file to use.
#' @param output_file name of output html file (needs to end with ".html",
#'  defaults to "<name of data file>_summary.html")
#' @param output_dir (optional) directory to save the output file to.
#'  Defaults to current working directory.
#' @param output_title (optional) text string to use as title for html report output
#' @param output_author (optional) text string to use as author for html report output
#' @param output_date (optional) text string to use as date for html report output
#' @param overwrite (optional) whether to overwrite `output_file` if it already exists.
#'  Defaults to FALSE for safety but in use probably TRUE will be more typical.
#' @param percCutHoriz,percCutVert Optional numeric. Percentage cutoff below which categorical
#'   levels are not labelled, for horizontal-barred section summary plots and
#'   vertical-barred variable-level plots respectively. Default 5.
#' @param est_chars_path (optional) path to establishment characteristics data (xls/xlsx).
#' @param est_chars_sheet (optional, needed if `est_chars_path` is used) name of sheet in est't chars to use.
#' @param est_char_vars,est_char_types,est_char_values,est_char_statements
#'  (optional, needed if `est_chars_path` is used) specification of establishment
#'   characteristics to use, see Details.
#' @param quiet (optional) whether to quieten the Rmd rendering information.
#'  Defaults to TRUE; FALSE useful for testing.
#' @param verbose (optional) whether to include extra/verbose detail in the report.
#'  Defaults to FALSE; TRUE useful for testing.
#' @param show (optional) whether to show intermediate results in the RStudio Viewer.
#'  Defaults to FALSE; unclear when TRUE might be useful.
#' @param rmd (optional) path to .Rmd document to use for report-making.
#'  Using anything other than the default should be rare.
#'
#' @returns a string giving the path of the saved .html document
#'
#' @details
#' Additional grouping variables using Establishment Characteristics can be added
#' using `est_chars_path` and `est_chars_sheet` to point to the Establishment
#' Characteristics spreadsheet and the relevant sheet thereof.
#' If used then `est_char_vars`, `est_char_types`, `est_char_values` and `est_char_statements`
#' must also be used, to specify the establishment characteristics variables
#' to be used and their details as in the data dictionary. See example below.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simplest usage
#' render_survey_summary(
#'   data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",
#'   dict_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME - Cohort Studies - SAG documents/20250507_data dictionary_master copy.xlsx",
#'   dict_sheet = "pupil_survey_Y12",
#'   output_dir = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/",
#'   output_file = "y12_pupil_survey_summary.html",
#'   output_title = "Y12 student survey summary",
#'   output_author = "D Sirl",
#' )
#'
#' # Using establishment characteristics too
#' render_survey_summary(
#'   data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",
#'   dict_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME - Cohort Studies - SAG documents/20250507_data dictionary_master copy.xlsx",
#'   dict_sheet = "pupil_survey_Y12",
#'   output_dir = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/",
#'   output_file = "y12_pupil_survey_summary.html",
#'   output_title = "Y12 student survey summary",
#'   output_author = "D Sirl",
#'   est_chars_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME Research Data - Partner Establishment Characteristics/Partner Establishment Characteristics Pseudonymised Data/2026-01-29-partner_characteristics_june_2025_data.xlsx",
#'   est_chars_sheet = "partner_characteristics_sec",
#'   est_char_vars =
#'     c("TrustCategory",
#'       "UrbanRural",
#'       "PercentageFSM"),
#'   est_char_types =
#'     c("factor-lo-hi",
#'       "factor-unordered",
#'       "numeric-quintiles"),
#'   est_char_values =
#'     c("No trust; Trust of 1-9; Trust of 10-19; Trust of 20+",
#'       "Conurbation; City or town; Rural",
#'       NA),
#'   est_char_statements =
#'     c("Total trust size",
#'       "Urban/Rural classification",
#'       "FSM quintile")
#' )
#' }
#'
render_survey_summary <- function(data_path,
                                  dict_path,
                                  dict_sheet,
                                  output_dir = getwd(),
                                  output_file = NULL,
                                  output_title = NULL,
                                  output_author = NULL,
                                  output_date = NULL,
                                  overwrite = FALSE,
                                  percCutHoriz = 5,
                                  percCutVert = 5,
                                  est_chars_path=NULL,
                                  est_chars_sheet=NULL,
                                  est_char_vars=NULL,
                                  est_char_types=NULL,
                                  est_char_values=NULL,
                                  est_char_statements=NULL,
                                  quiet = TRUE,
                                  verbose = FALSE,
                                  show = FALSE,
                                  rmd = NULL) {

  # check that the rmd being pointed to exists, or if not supplied point to the default
  if (is.null(rmd)) {
    rmd <- system.file("markdown_reports/survey_summary_report.Rmd", package="OMESurvey")
    if (!nzchar(rmd)) {
      stop("Template Rmd not found in installed package.\n
           Did you (or Dave!) put it in inst/markdown_reports/ and reinstall the package?")
    }
  } else {
    if (!file.exists(rmd)) stop("rmd not found")
  }


  # default output filename if not provided
  if (is.null(output_file)) {
    base <- tools::file_path_sans_ext(basename(data_path))
    output_file <- paste0(base, "_summary.html")
  }


  # ensure output_dir exists (and can be created if needed)
  if (!dir.exists(output_dir)) {
    ok <- tryCatch(
      dir.create(output_dir, recursive = TRUE),
      warning = function(w) FALSE,
      error   = function(e) FALSE
    )

    if (!ok || !dir.exists(output_dir)) {
      stop("Could not create output directory: ", output_dir)
    }
  }


  # compute full output path (what rmarkdown::render will create)
  out_full <- normalizePath(file.path(output_dir, output_file), mustWork = FALSE)

  # if file exists, handle according to overwrite flag and interactivity
  if (file.exists(out_full)) {
    if (!overwrite) {
      if (interactive()) {
        cat("Chosen output file path \n", out_full)
        ans <- tolower(trimws(readline(
          paste0("File already exists. Overwrite? [y/N]: ")
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
    output_date = output_date,
    verbose = verbose,
    percCutHoriz = percCutHoriz,
    percCutVert = percCutVert
  )

  # (optionally) suppress RStudio Viewer during render
  if (!isTRUE(show)) {
    old_viewer <- getOption("viewer")
    on.exit(options(viewer = old_viewer), add = TRUE)
    options(viewer = function(url, ...) invisible())
  }

  # managing working directory for the .Rmd rendering
  old <- setwd(dirname(rmd))
  on.exit(setwd(old), add = TRUE)


  # render the .Rmd
  out_path <- rmarkdown::render(
    input = rmd,
    output_file = output_file,
    output_dir = output_dir,
    params = params,
    envir = new.env(parent = globalenv()), # isolate execution
    knit_root_dir = dirname(rmd),
    quiet = quiet
  )

  return(normalizePath(out_path))
}



#' Stacked proportional bar chart in OME style (Dave's version.)
#'
#' `OME_stacked_bar()` builds a stacked (filled) proportional bar chart from
#' survey-style data using **bare column names**.
#' `OME_stacked_bar_()` is the programmatic interface and underlying engine,
#' designed for use when column names are supplied as **strings** or **symbols**.
#'
#' The plot displays:
#' * relative frequencies of the response variable,
#' * optional subdivision into separate bars,
#' * optional faceting,
#' * percentage labels inside bar segments (above a cutoff),
#' * optional `(n)` labels showing the number of responses per bar.
#'     (though these need care if combined with faceting)
#'
#' @author Dave Sirl
#'
#' @note Future/possible extensions include
#'   * edit show_counts argument to allow optional (m/n) formatting of labels
#'       (where m = # non-missing values and n = # values)
#'   * Letting (n) labelling work with faceting.
#'
#' @param dat A data frame.
#' @param response_var Bare column name giving the response categories (used for fill).
#' @param group_var Optional bare column name defining separate bars.
#' @param facet_var Optional bare column name used for faceting.
#'   All variables should be factors (characters may work but are not guaranteed).
#'
#' @param percCut Numeric scalar (0-100). Cutoff below which percentages are not
#'   shown in bar segments. Default `5`; use a number >100 to suppress all percentages.
#'
#' @param colo Optional character vector of fill colours (one per response
#'   level). If `NULL` (default), colours are taken from
#'   `OMESurvey::get_OME_colours(type = "distinct")`.
#'   When `na.rm = FALSE`, a grey colour is prepended for the `"Missing"` level.
#'
#' @param na.rm Logical. If `TRUE`, remove `NA` responses; if `FALSE` (default)
#'   convert them to `"Missing"` and treat as an additional response level.
#'
#' @param NA_label Character, default `"Missing"`. Label to use for `NA` responses.
#'
#' @param show_counts Logical; whether to display (n) counts for each bar.
#'  Defaults to TRUE. Needs care if used with faceting.
#'
#' @param horiz Logical (default `FALSE`). If `TRUE`, flip coordinates so bars
#'   are horizontal and place the legend below the plot.
#'
#' @param text_scale Positive number (default `1`) scaling the size of
#'   percentage labels.
#'
#' @param fillLabText Optional legend title for the fill variable. If `NULL`
#'   (default) the title is removed; if `""` the name of `response_var` is used.
#'
#' @param groupLabText Optional label for the bar/group axis. If `NULL` the label is
#'   removed; if `""` the name of `group_var` is used.
#'
#' @param propLabText Label for the proportion axis. Default `"Proportion of responses"`.
#'
#' @param titleText Optional plot title. Default `NULL` removes the title.
#'
#' @param facet_labels Optional named character vector used as a labeller for
#'   facets (names are original facet values, values are labels to display).
#'   Default `NULL` uses the raw facet values.
#'
#' @param facet_layout Optional character. If `"1row"` or `"1col"` the facets
#'   are arranged in a single row or column. Otherwise (or if `NULL`) the default
#'   facet layout is used. Should be `"1col"` if used with `show_counts=TRUE`.
#'
#' @param separate_at Optional integer specifying where to draw a horizontal
#'   separation line between groups defined by `group_var`.
#'   * If positive n the separation line is after the first n categories
#'   * If negative n the separation line is before the n-th last category.
#'  Default `NULL` or 0 omits the line.
#'
#' @param fill_label_width Optional integer. Width (in characters) used when
#'   wrapping fill labels with `stringr::str_wrap()`. Default is 20.
#'
#' @param omitGroupLabels Optional logical controlling whether to omit group labels.
#'   Helpful when group_var=NULL. Default FALSE.
#' @param group_label_width Optional integer. Width (in characters) used when
#'   wrapping group labels with `stringr::str_wrap()`. Default is `NULL`, to not wrap.
#' @param group_labels Optional named character vector providing alternative
#'   labels for the grouping variable. Should be of the form
#'   `c(level1 = "Label 1", level2 = "Label 2")`. Default is `NULL`, which
#'   uses the factor's existing levels.
#'
#' @param ... Additional arguments passed from \code{OME_stacked_bar()} to
#'   \code{OME_stacked_bar_()}. Not used when calling \code{OME_stacked_bar_()}
#'   directly.
#'
#' @returns A `ggplot` object.
#'
#' @details
#' `OME_stacked_bar()` uses tidy evaluation;
#' `OME_stacked_bar_()` uses standard evaluation and is safe for loops and
#' programmatic workflows.
#' @examples
#' # Minimal example data
#' dat <- tibble::tibble(
#'   Response = factor(c("Yes", "No", "No", NA, "Yes", "Maybe"),
#'                     levels = c("No", "Maybe", "Yes")),
#'   Group    = factor(c("A", "A", "A", "A", "B", "B"))
#' )
#'
#' # Basic example (NA is made explicit)
#' OME_stacked_bar(dat, Response, Group)
#'
#' # NA's ignored
#' OME_stacked_bar(dat, Response, Group, na.rm=TRUE)
#'
#' # Horizontal version with a title
#' OME_stacked_bar(
#'    dat,
#'    response_var = Response,
#'    group_var = Group,
#'    horiz = TRUE,
#'    titleText = "Example bar chart"
#' )
#'
#' # Example programmatic use
#' \dontrun{
#'   vars <- c("Gender", "Ethnicity", "FSM")
#'   for (v in vars) {
#'     p <- OME_stacked_bar_(
#'       dat = survey_df,
#'       response_var = "some_variable",
#'       group_var = v
#'      )
#'     print(p)
#'   }
#' }
#'
#' @export
#'
OME_stacked_bar_ = function(dat, response_var,
                           group_var=NULL, facet_var=NULL,
                           percCut=NULL, colo=NULL,
                           na.rm=FALSE, NA_label="Missing",
                           show_counts=TRUE,
                           horiz=FALSE, text_scale=1,
                           fillLabText=NULL, groupLabText=NULL,
                           propLabText="Proportion of responses",
                           titleText=NULL,
                           facet_labels=NULL, facet_layout=NULL,
                           separate_at = NULL,
                           fill_label_width=20,
                           omitGroupLabels = FALSE,
                           group_label_width = NULL,
                           group_labels = NULL){

  # cat("DEBUG: response_var =", response_var, "\n")
  # cat("DEBUG: group_var    =", group_var, "\n")
  # cat("DEBUG: facet_var    =", facet_var, "\n")


  # --- Handle NULL grouping/faceting, then capture tidy-eval response/grouping/faceting inputs ----------------------------------------------

  if (is.null(group_var)) {
    dat <- dat |> dplyr::mutate(.__group__ = factor("All"))
    group_var <- ".__group__"
  }

  if (is.null(facet_var)) {
    dat <- dat |> dplyr::mutate(.__facet__ = factor("All"))
    facet_var <- ".__facet__"
  }


  response_sym <- rlang::sym(response_var)
  group_sym    <- if (!is.null(group_var)) rlang::sym(group_var) else NULL
  facet_sym    <- if (!is.null(facet_var)) rlang::sym(facet_var) else NULL

  response_name <- rlang::as_name(response_sym)
  group_name    <- if (!is.null(group_sym)) rlang::as_name(group_sym) else NULL
  facet_name    <- if (!is.null(facet_sym)) rlang::as_name(facet_sym) else NULL

  # cat("\n--- OME_stacked_bar DEBUG ---\n")
  # cat("raw response_var: "); print(response_var)
  # cat("response_sym: ");    print(response_sym)
  # cat("response_name: ");   print(response_name)
  # cat("names(dat): ");      print(names(dat))
  # cat("--- END DEBUG ---\n\n")


  # --- Validate response/group/facet inputs ----

  if (!response_name %in% names(dat)) {
    stop("response_var must be a column in `dat`.")
  }
  if (!group_name %in% names(dat)) {
    stop("group_var must be a column in `dat`.")
  }
  if (!facet_name %in% names(dat)) {
    stop("facet_var must be a column in `dat`.")
  }

  if (!is.factor(dat[[group_name]])) {
    stop("group_var must be a factor.")
  }
  if (!is.factor(dat[[facet_name]])) {
    stop("facet_var must be a factor.")
  }


  # --- Build internal 3-column structure -------------------------------------

  dat2 <- dat |>
    dplyr::mutate(
      var_name      = .data[[response_name]],
      var_subdivide = .data[[group_name]],
      var_facet     = .data[[facet_name]]
    )

  # Ensure factors
  if (!is.factor(dat2$var_subdivide))
    dat2$var_subdivide <- factor(dat2$var_subdivide)
  if (!is.factor(dat2$var_facet))
    dat2$var_facet <- factor(dat2$var_facet)

  # Label defaults
  if (identical(fillLabText, ""))
    fillLabText <- response_name
  if (identical(groupLabText, ""))
    groupLabText <- group_name

  has_facet <- length(unique(dat2$var_facet)) > 1



  if (show_counts && has_facet) {
    warning("Faceting detected: count labels are not supported with facets; suppressing them.")
  }



  # --- Convert percCut to proportion, normalise colours -----------------------------------------

  percCut <- ifelse(is.null(percCut), 0.05, percCut / 100)
  colo <- convert_colo(colo)
  # in normal usage this should not be needed, but keeping it because I'm scared it's used somewhere


  # --- deal with NAs and how they impact colours ----------------

  if(na.rm){ # if removing NAs: do so and make a colour palette if needed
    dat2 <- dat2 |> dplyr::filter(!is.na(var_name))
    if (is.null(colo)){
      colo <- dat2 |> dplyr::pull(var_name) |> nlevels() |> get_OME_colours(type='distinct')
    }
  }
  else{ # if not removing NAs: convert them to a proper level, sort out a colour pallete if needed,
    # then prepend grey to the pallete for the NAs
    dat2 <- dat2 |>
      dplyr::mutate(
        var_name = var_name |>
          forcats::fct_expand(NA_label) |>
          forcats::fct_na_value_to_level(level = NA_label) |>
          forcats::fct_relevel(NA_label, after = 0)
      )
    if (is.null(colo)){
      colo <- (dat2 |> dplyr::pull(var_name) |> nlevels() - 1) |> get_OME_colours(type='distinct')
    }
    colo = c(setNames("grey", NA_label), colo)
  }

  # print("here!")
  # print(str(dat2))


  # --- Build plot -------------------------------------------------------------

  thePlot <-
    #ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, by=var_subdivide, fill=var_name)) +
    #ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, group = interaction(var_subdivide, var_name), fill=var_name)) +
    ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, fill=var_name)) +
    ggplot2::geom_bar(position = "fill") +
    OMESurvey::ROME_ggtheme(base_size = 12) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.85), face = "plain", color = "black",
        margin = ggplot2::margin(5, 0, 5, 0)
      ),
      plot.title.position = "plot"
    )

  # Further theme()ing
  if (horiz) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="bottom",
        #panel.grid.major.x = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    fill_guide <- ggplot2::guide_legend(direction="horizontal", nrow=1, reverse=TRUE)
  } else {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="right",
        panel.grid.major.x = ggplot2::element_blank(),
        #panel.grid.major.y = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        panel.grid.minor = ggplot2::element_blank()
      )
    fill_guide <- ggplot2::guide_legend(direction="vertical")
  }

  # Labels and scales - for the fill
  fill_labels <- function(x) stringr::str_wrap(x, width=fill_label_width)

  if (is.null(colo)){
    thePlot <- thePlot +
      ggplot2::scale_fill_discrete(labels=fill_labels, drop=FALSE, guide=fill_guide)
  } else {
    thePlot <- thePlot +
      ggplot2::scale_fill_manual(values=colo, labels=fill_labels, drop=FALSE, guide=fill_guide)
  }

  # Labels and scales - for the groups
  group_levels <- levels(dat2$var_subdivide)

  # --- Left-axis labels (group labels)
  if (omitGroupLabels) {

    primary_labeller <- NULL

  } else if (is.null(group_labels)) {

    # No custom labels
    if (is.null(group_label_width)) {
      primary_labeller <- ggplot2::waiver()
    } else {
      primary_labeller <- function(x) stringr::str_wrap(x, width = group_label_width)
    }

  } else {

    # Custom labels supplied
    if (is.null(group_label_width)) {
      primary_labeller <- group_labels
    } else {
      primary_labeller <- stringr::str_wrap(group_labels, width = group_label_width) |>
        stats::setNames(names(group_labels))
    }
  }

  if (!is.null(group_labels) && !all(names(group_labels) %in% group_levels)) {
    warning("Names of `group_labels` do not match the levels of the grouping variable.")
  }



  # --- Secondary axis labeller (counts) --------------------------------


  if (exists("show_counts") && show_counts && !has_facet) {

    if (has_facet) {
      # Per facet counts
      group_counts <- dat2 |>
        dplyr::summarise(
          non_na = sum(!is.na(.data[[response_name]])),
          total  = dplyr::n(),
          .by = c(var_facet, var_subdivide)
        ) |>
        #dplyr::mutate(label = sprintf("(%s/%s)", scales::comma(non_na), scales::comma(total)))
        dplyr::mutate(label = sprintf("(%s)", scales::comma(total)))

      right_labels <- group_counts |>
        split(~var_facet) |>
        lapply(function(df) {
          idx <- match(group_levels, df$var_subdivide)
          stats::setNames(df$label[idx], group_levels)
        })


    } else {
      # Global counts (no real faceting)
      group_counts <- dat2 |>
        dplyr::summarise(
          non_na = sum(!is.na(.data[[response_name]])),
          total  = dplyr::n(),
          .by = var_subdivide
        ) |>
        #dplyr::mutate(label = sprintf("(%s/%s)", scales::comma(non_na), scales::comma(total)))
        dplyr::mutate(label = sprintf("(%s)", scales::comma(total)))

      idx <- match(group_levels, group_counts$var_subdivide)
      right_labels <- stats::setNames(group_counts$label[idx], group_levels)

    }
  }


  if (show_counts) {
    secondary_labeller <- right_labels
  } else {
    secondary_labeller <- NULL
  }



  thePlot <- thePlot + ggplot2::scale_x_discrete(
    labels = primary_labeller,
    sec.axis = if (show_counts) {
      ggplot2::dup_axis(
        labels = secondary_labeller,
        breaks = group_levels,
        name = NULL
      )
    } else {
      ggplot2::waiver()
    }
  )


  # if (horiz) {
  #
  #   # Horizontal: group labels on y-axis
  #   if (exists("show_counts") && show_counts) {
  #     thePlot <- thePlot +
  #       ggplot2::scale_x_discrete(
  #         labels = left_labeller,
  #         sec.axis = ggplot2::dup_axis(
  #           labels = right_labels,
  #           breaks = group_levels,
  #           name = NULL
  #         )
  #       )
  #   } else {
  #     thePlot <- thePlot +
  #       ggplot2::scale_x_discrete(labels = left_labeller)
  #   }
  #
  # } else {
  #
  #   # Vertical: group labels on x-axis
  #   if (exists("show_counts") && show_counts) {
  #     thePlot <- thePlot +
  #       ggplot2::scale_x_discrete(
  #         labels = left_labeller,
  #         sec.axis = ggplot2::dup_axis(
  #           labels = right_labels,
  #           breaks = group_levels,
  #           name = NULL
  #         )
  #       )
  #   } else {
  #     thePlot <- thePlot +
  #       ggplot2::scale_x_discrete(labels = left_labeller)
  #   }
  # }




  # Labels and scales - other

  thePlot <- thePlot +
    ggplot2::labs(x=groupLabText, y=propLabText,
                  fill=fillLabText, title=titleText) +
    ggplot2::scale_y_continuous(breaks = seq(0,1,0.25),
                                labels = scales::percent_format(accuracy=1),
                                minor_breaks = NULL)


  # --- Calculate and annotate with the (n) labels ------------------------
  # dat_sum <-
  #   dat2 |>
  #   dplyr::summarise(
  #     num_resp = dplyr::n(),
  #     .by = c(var_facet,var_subdivide)
  #     ) |>
  #   dplyr::mutate(num_resp = scales::label_comma(prefix="(", suffix=")")(num_resp) )
  #
  # vjust_val <- if (horiz) 0.5 else 0
  # hjust_val <- if (horiz) 0 else 0.5
  #
  # thePlot <- thePlot +
  #   ggplot2::geom_text(
  #     ggplot2::aes(
  #       x = var_subdivide, y = 1.02,
  #       by = NULL, fill = NULL,
  #       label = num_resp
  #     ),
  #     data = dat_sum,
  #     colour = "black", size = 3*text_scale,
  #     vjust = vjust_val, hjust = hjust_val
  #   )

  # --- Percentage labels inside bars -----------------------------------------

  dat3 <- dat2 |>
    dplyr::count(var_subdivide, var_name, var_facet) |>
    dplyr::group_by(var_subdivide, var_facet) |>
    dplyr::mutate(prop = n / sum(n)) |>
    dplyr::ungroup()

  thePlot <- thePlot +
    ggplot2::geom_text(
      data = dat3,
      ggplot2::aes(
        x = var_subdivide,
        y = prop,
        label = ifelse(
          prop < percCut,
          "",
          scales::percent(prop, accuracy = 1)
        )
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      colour = "white",
      size = 3 * text_scale
    )



  # suppressing warning that arises by doing stat_prop(geom=text) rather than
  # stat_geom() with after_stat(prop)
  # viz "Ignoring unknown parameters: `orientation`"

  # thePlot <- suppress_specific_warning(
  #   {
  #     thePlot +
  #       ggstats::stat_prop(
  #         ggplot2::aes(
  #           x = var_subdivide,
  #           group = var_subdivide,
  #           label = ifelse(ggplot2::after_stat(prop) < percCut,
  #                          "",
  #                          scales::percent(ggplot2::after_stat(prop), accuracy = 1))
  #         ),
  #         geom = "text",
  #         position = ggplot2::position_fill(vjust = 0.5),
  #         colour = "white",
  #         size = 3 * text_scale,
  #         show.legend = FALSE
  #       )
  #   },
  #   pattern = "Ignoring unknown parameters: `orientation`"
  # )$value


  # --- Faceting ---------------------------------------------------------------
  if (has_facet) {
    facet_args <- list(facets = ~var_facet)

    # Apply custom labels if provided
    if (!is.null(facet_labels)) {
      facet_args$labeller <- ggplot2::labeller(var_facet = facet_labels)
    }

    # Apply layout control if requested
    if (!is.null(facet_layout) && facet_layout == "1row") {
      facet_args$nrow <- 1
    } else if (!is.null(facet_layout) && facet_layout == "1col") {
      facet_args$ncol <- 1
    }

    thePlot <- thePlot + do.call(ggplot2::facet_wrap, facet_args)
  }

  # --- Optional dashed reference line -----------------------------------------
  if (!is.null(separate_at) && separate_at != 0) {

    # first interpret separate_at
    if (separate_at > 0) {
      intercept <- separate_at + 0.5
    } else {
      intercept <- length(levels(dat2$var_subdivide)) - abs(separate_at) + 0.5
    }
    thePlot <- thePlot +
      ggplot2::geom_vline(
        xintercept = intercept,
        linetype = "dashed",
        linewidth = 0.3
      )
  }


  # --- Final coordinate system ------------------------------------------------

  if (horiz) {
    thePlot <- thePlot + ggplot2::coord_flip()
  } else {
    thePlot <- thePlot + ggplot2::coord_cartesian()
  }


  return(thePlot)

}



#' @rdname OME_stacked_bar_
#' @export
OME_stacked_bar <- function(dat, response_var,
                            group_var = NULL, facet_var = NULL,
                            ...) {

  # Capture bare names safely (never evaluate)
  response_quo <- rlang::enquo(response_var)
  group_quo    <- rlang::enquo(group_var)
  facet_quo    <- rlang::enquo(facet_var)

  # Convert to strings (NULL stays NULL)
  response_str <- rlang::as_name(response_quo)
  group_str    <- if (!rlang::quo_is_null(group_quo)) rlang::as_name(group_quo) else NULL
  facet_str    <- if (!rlang::quo_is_null(facet_quo)) rlang::as_name(facet_quo) else NULL

  # Call the programmatic version
  OME_stacked_bar_(
    dat          = dat,
    response_var = response_str,
    group_var    = group_str,
    facet_var    = facet_str,
    ...
  )
}







#' Section-level summary plot for categorical questions
#'
#' Make a horizontal stacked bar chart to summarise several survey questions,
#' with questions ordered according to the proportion of responses that fit a
#' particular pattern.
#'
#' @param dat a tibble/data.frame, each variable corresponding to a question.
#'  All variables should be factors and all with the same possible values.
#' @param labels_vec a named vector of labels to use for the questions on the plot.
#'  Names are variable names and values are corresponding labels.
#' @param na.rm Logical. If `TRUE`, remove `NA` responses; if `FALSE` (default)
#'   convert them to `NA_label` and treat as an additional response level.
#' @param NA_label Character, default `"Missing"`. Label to use for `NA` responses.
#' @param percCut numeric scalar (0-100). Cutoff below which percentages are not
#'  shown in bar segments. Default `5`.
#' @param colo (optional but recommended) vector of colours to use for the fill scale
#'  (character vector of colours, length the same as the number of levels of the
#'   factor that is the first variable of `dat`.)
#' If `NULL` (default) the pallete from `OMESurvey::get_OME_colours(type='distinct')` is used.
#' When `na.rm = FALSE` a grey colour is prepended for the "No response" level.
#' Note that percentage labels are in white, so the pallete needs to work with that.
#'  (... or this function/package needs upgrading!)
#' @param order_values Optional. Controls how questions are ordered in the plot.
#'   May be a character vector of response levels, or the special value
#'   `"mean(as.numeric())"`. See Details.
#' @param titleText Optional text to use as plot title.
#' @param fill_label_width Optional integer. Width (in characters) used when wrapping
#'   fill labels with `stringr::str_wrap()`. Passed to `OME_stacked_bar()`. Default is 20.
#' @param group_label_width Optional integer. Width (in characters) used when wrapping
#'   vertical (group/question) axis labels with `stringr::str_wrap()`. Default is 30.
#'
#' @returns
#' A `ggplot` object, a horizontal stacked bar chart summarising the survey questions.
#'
#' @note
#' All variables in `dat` should have identical factor levels.
#' The names of `labels_vec` must match the column names of `dat`.
#' Values in `order_values` must be valid levels of the response factor.
#'
#' @details
#' The `order_values` argument controls how questions are ordered in the
#' horizontal bar chart.
#'
#' * If `order_values` is a character vector of response levels (e.g.
#'   `c("Strongly agree", "Agree")`), questions are ordered by the proportion
#'   of respondents whose answers fall into those levels.
#'   (Intended for factors with positive-negative / divergent scales.)
#'
#' * If `order_values` is exactly `"mean(as.numeric())"`, the function instead
#'   orders questions by the mean of the numeric codes of the response factor.
#'   (Intended for factors with low-high / sequential scales.)
#'
#' Axis labels are wrapped using `stringr::str_wrap()` with a width controlled
#' by `group_label_width`. Legend/fill labels are treated the same using `fill_label_width`.
#'
#' @section Legend placement:
#' By default, the legend is placed horizontally according to the standard
#' ggplot2 layout. If you want the legend to be centred across the full width
#' of the final figure, the helper function `centre_legend_below()` can be used
#' after plotting. (This must be done after any tweaks to the ggplot theme, annotations, etc)
#'
#'
#' @export
#'
#' @examples
#' # Minimal example with three questions and three response levels
#' dat <- tibble::tibble(
#'   Q1 = factor(c("Yes", "No", "Yes", NA), levels=c("No", "Maybe", "Yes")),
#'   Q2 = factor(c("No", "No", "Yes", "Maybe"), levels=c("No", "Maybe", "Yes")),
#'   Q3 = factor(c("Maybe", "Yes", "Maybe", "Yes"), levels=c("No", "Maybe", "Yes"))
#' )
#'
#' # Simplest use
#' summary_plot_stacked_bar(dat)
#'
#' # Add question labels
#' labels <- c(Q1 = "Question 1", Q2 = "Question 2", Q3 = "Question 3")
#' summary_plot_stacked_bar(dat, labels_vec = labels)
#'
#' # With custom wrapping if they are long
#' labels_long <- c(
#'   Q1 = "Question 1",
#'   Q2 = "Question 2",
#'   Q3 = "The third question asked as part of this series of three questions"
#' )
#' summary_plot_stacked_bar(dat, labels_vec = labels_long)
#' summary_plot_stacked_bar(dat, labels_vec = labels_long, group_label_width = 20)
#'
#' # Remove missing values
#' summary_plot_stacked_bar(dat, na.rm=FALSE)
#'
summary_plot_stacked_bar <- function(dat, labels_vec=NULL,
                                     na.rm=FALSE, NA_label="Missing",
                                     percCut=5,
                                     colo=NULL, order_values = NULL,
                                     titleText=NULL,
                                     fill_label_width=20,
                                     group_label_width=30){

  # In the context of using this for the survey_summary_report(.Rmd) this should be unnecessary, but maybe leave it for other use?
  colo <- convert_colo(colo)

  # check that all variables are factors and that they all have the same levels
  all_factors <- dat |> purrr::map_lgl(is.factor) |> all()

  same_levels <- all_factors && {
    levs <- dat |> purrr::map(levels)
    ref <- levs[[1]]
    levs[-1] |> purrr::map_lgl(~ identical(.x, ref)) |> all()
  }

  if (!all_factors | !same_levels){
    warning(paste0(
      "The dataframe passed to summary_plot_stacked_bar() should have all its variables (a) factors, (b) with the same levels\n",
      "Running with (a) ", all_factors, " and (b) ", same_levels, " ",
      "(note that if (a) is false then the check of (b) here is not meaningful)"))
  }

  if (!is.null(labels_vec) && !all(names(labels_vec) %in% names(dat))) {
    warning("Some names in labels_vec do not match column names in dat.")
  }



  # pivot the many questions/variables to long question&response form,
  # reorder questions according to proportion of responses that are in order_values
  dat <-
    dat |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "question",
                        values_to = "Response") |>
    dplyr::mutate(question = forcats::fct_inorder(question),
                  order_flag = dplyr::case_when(
                    length(order_values) == 1 &&
                      trimws(order_values) == "mean(as.numeric())" ~ as.numeric(Response),

                    TRUE ~ as.character(Response) %in% order_values
                  ),
                  question = forcats::fct_reorder(question, order_flag, .fun=mean, .na_rm=TRUE))


  # do the plotting
  thePlot <- dat |>
    OME_stacked_bar_(response_var = "Response",
                    group_var = "question",
                    horiz = TRUE,
                    percCut = percCut,
                    fillLabText = NULL,
                    propLabText = NULL,
                    groupLabText = NULL,
                    colo = colo,
                    na.rm = na.rm,
                    NA_label = NA_label,
                    titleText = titleText,
                    fill_label_width = fill_label_width,
                    group_label_width = group_label_width,
                    group_labels = labels_vec)
  return(thePlot)
}




#' @title Deprecated: Make a stacked bar chart summarising many survey questions
#' @description
#' This function is deprecated.
#' Use \code{\link{summary_plot_stacked_bar}} instead.
#'
#' @inheritParams summary_plot_stacked_bar
#' @param ... Additional arguments passed to \code{summary_plot_stacked_bar()}.
#' @seealso summary_plot_stacked_bar
#' @export
plot_many_questions <- function(...) {
  .Deprecated(
    new = "summary_plot_stacked_bar",
    package = "OMESurvey",
    old = "plot_many_questions"
  )
  summary_plot_stacked_bar(...)
}




#' Boxplot (horizontal), in an OME style.
#'
#' `OME_boxplot()` builds a boxplot from survey-style data using **bare column names**.
#' `OME_boxplot_()` is the programmatic interface and underlying engine,
#' designed for use when column names are supplied as **strings** or **symbols**.
#'
#' Features include:
#' * optional subdivision into separate boxes,
#' * optional labels showing the number of non-NA responses per box,
#' * optional dashed line to visually separate some of the boxes.

#' @author Dave Sirl
#'
#' @note Future/possible extensions include
#'   * support for dodging/colour
#'   * maybe vertical/horizontal switching
#'   * maybe faceting
#'
#' @param data A data frame.
#' @param value_var Numeric variable to plot.
#' @param group_var Optional grouping variable (factor).
#' @param show_counts Logical; whether to display (valid/total) counts on the
#'   right-hand axis. Defaults to TRUE.
#' @param na.rm Logical; whether to remove/ignore missing values.
#'   When `na.rm = TRUE`, count labels show the number of plotted (i.e. non-missing) observations;
#'   when `na.rm = FALSE`, labels show non-missing and total number of observations.
#'
#' @param valueLabText Optional title for the value variable axis. If `NULL`
#'   (default) the title is removed; if `""` the name of `value_var` is used.
#' @param groupLabText Optional title for the group variable axis. If `NULL`
#'   (default) the title is removed; if `""` the name of `group_var` is used.
#' @param omitGroupLabels Optional logical controlling whether to omit group labels.
#'   Helpful when group_var=NULL. Default FALSE.
#' @param titleText Optional plot title.
#' @param colour Boxplot outline colour (any valid R colour). Defaults to [get_OME_colours](1).
#'
#' @param separate_at Optional integer specifying where to draw a horizontal
#'   separation line between groups defined by `group_var`.
#'   * If positive n the separation line is after the first n categories
#'   * If negative n the separation line is before the n-th last category.
#'  Default `NULL` or 0 omits the line.
#' @param group_label_width Optional integer. Width (in characters) used when
#'   wrapping group labels with `stringr::str_wrap()`. Default is `NULL`, to not wrap.
#' @param group_labels Optional named character vector providing alternative
#'   labels for the grouping variable. Should be of the form
#'   `c(level1 = "Label 1", level2 = "Label 2")`. Default is `NULL`, which
#'   uses the factor's existing levels.
#'
#' @param ... Additional arguments passed from \code{OME_boxplot()} to
#'   \code{OME_boxplot_()}. Not used when calling \code{OME_boxplot_()}
#'   directly.
#'
#' @details
#' `OME_boxplot()` uses tidy evaluation;
#' `OME_boxplot_()` uses standard evaluation and is safe for loops and
#' programmatic workflows.

#' @return A ggplot object
#'
#' @examples
#' dat <- tibble::tibble(
#'   Score = c(5, 7, 6, 9, 4, NA),
#'   Group = factor(c("Alpha", "Alpha", "Alpha", "Beta", "Beta", "Beta")),
#'   Grouping2 = factor(c("G1", "G2", "G1", "G2", "G1", "G2"))
#' )
#'
#' # Basic example
#' OME_boxplot(dat, Score, Group)
#'
#' # Without counts
#' OME_boxplot(dat, Score, Group, show_counts = FALSE)
#'
#' # With a title
#' OME_boxplot(
#'   dat,
#'   value_var = Score,
#'   group_var = Group,
#'   title = "Example boxplot"
#' )
#'
#' # Example programmatic use
#' \dontrun{
#'   group_vars <- c("Group", "Grouping2")
#'   for (v in group_vars) {
#'     p <- OME_boxplot_(
#'       data      = dat,
#'       value_var = "Score",
#'       group_var = v
#'     )
#'     print(p)
#'   }
#' }
#'
#' @export
#'
OME_boxplot_ <- function(data,
                         value_var,
                         group_var = NULL,
                         show_counts = TRUE,
                         na.rm = FALSE,
                         valueLabText = NULL,
                         groupLabText = NULL,
                         omitGroupLabels = FALSE,
                         titleText = NULL,
                         colour = OMESurvey::get_OME_colours(1),
                         separate_at = NULL,
                         group_label_width = NULL,
                         group_labels = NULL) {

  # --- Handle NULL grouping (same as stacked-bar) -----------------------------
  if (is.null(group_var)) {
    data <- data |>
      dplyr::mutate(.__group__ = factor("All"))
    group_var <- ".__group__"
  }

  # --- Input validation -------------------------------------------------------
  if (!value_var %in% names(data) || !group_var %in% names(data)) {
    stop("group_var and value_var must be columns in `data`.")
  }

  if (!is.factor(data[[group_var]])) {
    stop("group_var must be a factor.")
  }
  if (!is.numeric(data[[value_var]])) {
    stop("value_var must be numeric.")
  }


  group_levels <- levels(data[[group_var]])

  # Label defaults
  if (identical(valueLabText, ""))
    valueLabText <- value_var
  if (identical(groupLabText, ""))
    groupLabText <- group_var


  # --- Summaries for counts ---------------------------------------------------
  if (show_counts) {
    group_counts <- data |>
      dplyr::summarise(
        non_na = sum(!is.na(.data[[value_var]])),
        total  = dplyr::n(),
        .by = dplyr::all_of(group_var)
      ) |>
      dplyr::mutate(label = if (na.rm) {
                              sprintf("(%d)", non_na)
                            } else {
                              sprintf("(%d/%d)", non_na, total)
                            }
      )

    idx <- match(group_levels, group_counts[[group_var]])
    right_labels <- stats::setNames(group_counts$label[idx], group_levels)
  }

  # --- Build plot -------------------------------------------------------------
  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[value_var]],
        y = .data[[group_var]]
      )
    ) +
    ggplot2::geom_boxplot(
      colour = colour,
      width = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::labs(x=groupLabText, y=valueLabText,
                  title=titleText) +
    OMESurvey::ROME_ggtheme(base_size = 12) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )



  # ggplot2::theme_bw() +
  # ggplot2::theme(
  #   panel.grid.major = ggplot2::element_line(colour = "grey90"),
  #   panel.grid.minor = ggplot2::element_line(colour = "grey95")
  # )

  # --- Label wrapping + optional counts ---------------------------------------
  # if (omitGroupLabels) {
  #   y_labeller <- NULL
  # } else {
  #   y_labeller <- function(x) stringr::str_wrap(x, width = group_label_width)
  # }
  #
  # if (show_counts) {
  #   p <- p +
  #     ggplot2::scale_y_discrete(
  #       labels = y_labeller,
  #       sec.axis = ggplot2::dup_axis(
  #         labels = right_labels,
  #         breaks = group_levels,
  #         name = NULL
  #       )
  #     )
  # } else {
  #   p <- p +
  #     ggplot2::scale_y_discrete(
  #       labels = y_labeller
  #     )
  # }

  # --- Axis labelling (group labels & counts) -----------------------------

  # Primary axis (group labels)

  if (omitGroupLabels) {

    primary_labeller <- NULL

  } else if (is.null(group_labels)) {

    # No custom labels
    if (is.null(group_label_width)) {
      primary_labeller <- ggplot2::waiver()
    } else {
      primary_labeller <- function(x) stringr::str_wrap(x, width = group_label_width)
    }

  } else {

    # Custom labels supplied
    if (is.null(group_label_width)) {
      primary_labeller <- group_labels
    } else {
      primary_labeller <- stringr::str_wrap(group_labels, width = group_label_width) |>
        stats::setNames(names(group_labels))
    }
  }

  # Secondary axis (counts)

  if (show_counts) {
    secondary_labeller <- right_labels
  } else {
    secondary_labeller <- NULL
  }


  p <- p + ggplot2::scale_y_discrete(
    labels = primary_labeller,
    sec.axis = if (show_counts) {
      ggplot2::dup_axis(
        labels = secondary_labeller,
        breaks = group_levels,
        name = NULL
      )
    } else {
      ggplot2::waiver()
    }
  )



  # --- Optional dashed reference line -----------------------------------------
  if (!is.null(separate_at) && separate_at != 0) {

    # first interpret separate_at
    if (separate_at > 0) {
      intercept <- separate_at + 0.5
    } else {
      intercept <- length(levels(data[[group_var]])) - abs(separate_at) + 0.5
    }

    p <- p +
      ggplot2::geom_hline(
        yintercept = intercept,
        linetype = "dashed",
        linewidth = 0.3
      )
  }


  p
}




#' @rdname OME_boxplot_
#' @export
OME_boxplot <- function(data,
                        value_var,
                        group_var = NULL,
                        ...) {
  # Capture bare names safely (never evaluate)
  value_quo <- rlang::enquo(value_var)
  group_quo    <- rlang::enquo(group_var)

  # Convert to strings (NULL stays NULL)
  value_str <- rlang::as_name(value_quo)
  group_str    <- if (!rlang::quo_is_null(group_quo)) rlang::as_name(group_quo) else NULL

  # Call the programmatic version
  OME_boxplot_(
    data      = data,
    value_var = value_str,
    group_var = group_str,
    ...
  )
}



#' Section-level summary plot for numeric questions
#'
#' Make a horizontal boxplot to summarise several numeric survey questions,
#' with questions ordered according to a summary statistic (by default the
#' median of observed responses).
#'
#' @param dat A tibble/data.frame, each variable corresponding to a survey
#' question. All variables should be numeric.
#'
#' @param labels_vec Optional named character vector of labels to use for the
#' questions on the plot. Names must match the column names of \code{dat}, and
#' values are the labels to display on the axis.
#'
#' @param na.rm Logical. Whether missing values should be removed/ignored for the
#' purposes of plotting. Passed through to \code{OME_boxplot_()}.
#' Defaults to \code{FALSE}. Note that missing values are always removed when
#' computing the ordering statistic.
#'
#' @param order_fun Function used to order questions in the plot.
#' This function should accept arguments \code{(x, na.rm = TRUE)} and return
#' a single numeric value (e.g. \code{median}, \code{mean}).
#' Defaults to \code{median}.
#'
#' @param titleText Optional text to use as the plot title.
#'
#' @param group_label_width Optional integer. Width (in characters) used when
#' wrapping question labels on the axis. Passed to \code{OME_boxplot_()}.
#' Default is 30.
#'
#' @param ... Additional arguments passed to \code{OME_boxplot_()}.
#'
#' @returns
#' A \code{ggplot} object, a horizontal boxplot summarising the survey questions.
#'
#' @details
#' The variables in \code{dat} are pivoted to long format, then ordered according
#' to the value returned by \code{order_fun} applied to each question's observed
#' responses (with \code{na.rm = TRUE}).
#'
#' The original column order of \code{dat} is preserved as a stable tie-breaker
#' when multiple questions have identical ordering statistics.
#'
#' Missing-value handling for ordering and for plotting are intentionally
#' separated: missing responses are ignored for ordering purposes, but their
#' treatment in the plot itself is controlled by \code{na.rm}.
#'
#' @export
#'
#' @examples
#' # Minimal example with three numeric questions
#' dat <- tibble::tibble(
#'   Q1 = c(1, 2, 3, NA),
#'   Q2 = c(5, 4, 3, 1),
#'   Q3 = c(2, 2, 2, 3)
#' )
#'
#' # Simplest use
#' dat |> summary_plot_boxplot()
#'
#'
#' # Add question labels
#' labels <- c(
#'   Q1 = "High-valued question",
#'   Q2 = "Low-valued question",
#'   Q3 = "Mid-valued question"
#' )
#' dat |> summary_plot_boxplot(labels_vec = labels)
#'
#' # With longer labels
#' labels_long <- c(
#'   Q1 = "Question where responses tend to be at the higher end of the scale",
#'   Q2 = "Question where responses tend to be at the lower end of the scale",
#'   Q3 = "Question where responses tend to be somewhere in the middle"
#' )
#' dat |> summary_plot_boxplot(labels_vec = labels_long)
#'
#' # Control label wrapping
#' summary_plot_boxplot(
#'   dat,
#'   labels_vec = labels_long,
#'   group_label_width = 20
#' )
#'
#' # Remove missing values for plotting
#' dat |> summary_plot_boxplot(na.rm = TRUE)
summary_plot_boxplot <- function(dat, labels_vec = NULL,
                                 na.rm = FALSE,
                                 order_fun = median,
                                 titleText = NULL,
                                 group_label_width = 30,
                                 ...) {

  # check
  if (!(dat |> purrr::map_lgl(is.numeric) |> all())) {
    warning("The dataframe passed to summary_plot_boxplot() should have all its variables numeric.")
  }

  if (!is.null(labels_vec) && !all(names(labels_vec) %in% names(dat))) {
    warning("Some names in labels_vec do not match column names in dat.")
  }


  # pivot to long form and reorder the factor `question`
  dat_long <- dat |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to  = "question",
      values_to = "value") |>
    dplyr::mutate(
      question = forcats::fct_inorder(question)) |>
    dplyr::mutate(
      order_stat = order_fun(value, na.rm = TRUE),
      .by = question) |>
    dplyr::mutate(
      question = forcats::fct_reorder(question, order_stat)
    )


  # plot
  p <- dat_long |>
    OME_boxplot_(
      value_var = "value",
      group_var = "question",
      titleText = titleText,
      group_label_width = group_label_width,
      group_labels = labels_vec,
      na.rm = na.rm,
      ...)

  return(p)

}




#' Centre a ggplot legend beneath the plot using cowplot
#'
#' Extract the legend from a ggplot object and place it centred beneath
#' the whole plot. (As opposed to centred under the axis area only.) If the
#' \pkg{cowplot} package is not installed, the plot is returned unchanged.
#'
#' @param p A ggplot object whose legend should be centred beneath the plot.
#' @param rel_heights A numeric vector of length two giving the relative
#'   heights of the plot area and the legend area when stacked vertically.
#'   Defaults to \code{c(1, 0.1)}, which gives the legend 10% of the vertical
#'   space the plot gets.
#'
#' @returns
#' If \pkg{cowplot} is installed, a \pkg{cowplot} object with the legend
#' centred underneath the plot. Otherwise, the original ggplot object is
#' returned unchanged.
#'
#' @details
#' This function extracts the legend from the supplied ggplot object,
#' removes the legend from the plot itself, and then stacks the plot and
#' legend vertically using \code{cowplot::plot_grid()}. The relative
#' heights of the two components can be controlled via \code{rel_heights}.
#'
#' @examples
#' \dontrun{
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg, fill = factor(cyl))) +
#'     ggplot2::geom_col()
#'   centre_legend_below(p)
#' }
#'
#' @export
#'
centre_legend_below <- function(p, rel_heights=c(1,0.1)) {

  # Only proceed if cowplot is available
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    return(p)
  }

  # Extract legend
  legend <- cowplot::get_legend(p)

  # Remove legend from the original plot
  p_clean <- p + ggplot2::theme(legend.position = "none")

  # Stack plot and legend
  cowplot::plot_grid(
    p_clean,
    legend,
    ncol = 1,
    rel_heights = rel_heights
  )
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
#'
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


#' Suppress warnings matching a specific pattern
#'
#' Evaluates an expression while suppressing only those warnings whose
#' message matches a given regular expression. All other warnings are
#' allowed to propagate normally.
#'
#' @param expr Expression to evaluate.
#' @param pattern Regular expression used to identify warnings that
#'   should be suppressed.
#'
#' @return A list with two elements:
#'   * `value` - the result of evaluating `expr`
#'   * `suppressed` - a character vector of suppressed warning messages
#'
#' @examples
#' suppress_specific_warning({
#'   warning("Ignore this one")
#'   123
#' }, pattern = "Ignore")
#'
#' # Suppress the usual "NAs introduced by coercion" warning
#' # and use $value to store the result and ignore the $suppressed component
#' numeric_version <-
#'   suppress_specific_warning(
#'     as.numeric(c("2", "3", "z")),
#'     pattern = "NAs introduced"
#'   )$value

#'
#' @export
#'
suppress_specific_warning <- function(expr, pattern) {
  suppressed <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl(pattern, msg, perl = TRUE)) {
        suppressed <<- c(suppressed, msg)
        invokeRestart("muffleWarning")   # stop this warning from printing
      }
      # otherwise let the warning proceed (not muffled)
    }
  )
  list(value = value, suppressed = suppressed)
}


#' Convert simple text fractions to numeric values
#'
#' Parses character strings of the form `"a/b"` and returns their numeric
#' value \eqn{a/b}. Intended for ordering or comparing simple fractions.
#'
#' @param x A character vector of fractions written as `"a/b"`.
#'
#' @return A numeric vector of the same length as `x`.
#' @export
#' @examples
#' frac_to_num(c("1/4", "1/2", "3/4"))
#'
frac_to_num <- function(x) {
  parts <- strsplit(x, "/", fixed = TRUE)
  sapply(parts, function(p) as.numeric(p[1]) / as.numeric(p[2]))
}


#' Relabel NA values in the first column of a data frame
#'
#' Converts the first column of a data frame to character and replaces any
#' `NA` values in that column with a specified label (default: "Missing").
#' Intended for use after `tabyl()` and related janitor formatting steps,
#' once the object has been converted to a plain data frame.
#'
#' @param df A data frame whose first column may contain `NA` values.
#' @param missing_label A character string used to replace `NA` values
#'   in the first column. Defaults to `"Missing"`.
#'
#' @return A data frame with the first column converted to character and
#'   any `NA` values replaced by `missing_label`.
#'
#' @export
#' @examples
#' c(rep("A", 3), rep("B", 2), rep(NA, 2)) |>
#'   janitor::tabyl() |>
#'   janitor::adorn_totals() |>
#'   janitor::adorn_pct_formatting() |>
#'   as.data.frame() |>
#'   rename_NA_first_col()
#'
rename_NA_first_col <- function(df, missing_label = "Missing") {
  # Identify the first column name
  name_var <- names(df)[1]

  # Convert first column to character
  df[[name_var]] <- as.character(df[[name_var]])

  # Replace NA with the chosen label
  na_row <- which(is.na(df[[name_var]]))
  df[[name_var]][na_row] <- missing_label

  df
}




#' ROME ggplot2 theme
#'
#' An OME ggplot2 theme (based on \code{ggplot2::theme_bw()}).
#'
#' !!! CURRENTLY COPIED FROM JAKE'S ROME_ggtheme(), with some-but-very-few tweaks !!!
#'
#' @param base_size Numeric. Base font size for the theme. Defaults to 16.
#'
#' @return A \code{ggplot2} theme object that can be added to a ggplot with \code{+}.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl))) +
#'     ggplot2::geom_bar() +
#'     ggplot2::labs(title = "ROME theme demo")
#'
#'   # Add the theme to a single plot:
#'   p + theme_OME()
#'
#'   # Set as the global default for the current session:
#'   ggplot2::theme_set(theme_OME())
#' }
#'
#' @export
#' @importFrom ggplot2 %+replace%
theme_OME <- function(base_size = 16) {
  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(
      # title/subtitle
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        hjust = 0,
        color = "black",
        margin = ggplot2::margin(t=0, r=0, b=5, l=0)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.7),
        face = "plain",
        hjust = -0.12,
        color = "black"
      ),

      #panel (the plot area)
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.border = ggplot2::element_blank(),

      #axis
      axis.title = ggplot2::element_text(size = ggplot2::rel(0.85), face = "plain"),
      axis.text  = ggplot2::element_text(size = ggplot2::rel(0.70), face = "plain"),
      axis.line  = ggplot2::element_line(color = "gray"),

      #legend
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.85), face = "plain"),
      legend.text  = ggplot2::element_text(size = ggplot2::rel(0.70), face = "plain"),
      legend.key   = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key.size = grid::unit(1.5, "lines"),
      legend.position  = "bottom",
      legend.box       = "vertical", # changed from horizontal
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),

      # strip (headings of facet elements)
      strip.background = ggplot2::element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        face = "plain",
        color = "white",
        margin = ggplot2::margin(t=5, r=0, b=5, l=0)
      )
    )
}






#' OME colour scales for ggplot2
#'
#' Provides OME palettes for use with colour and fill aesthetics.
#' (Convenience wrapper around \code{ggplot2::scale_colour_manual()} using
#' the standard OME colour palette.)
#'
#' @param type Character string specifying which palette to use.
#' Passed to \code{\link{get_OME_colours}}; see that function for available options.
#' Default is \code{"distinct"}.
#' @param ... Additional arguments passed to \code{scale_colour_manual()}.
#'
#' @return A ggplot2 scale for colours.
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Examples here use scale_colour_OME(); scale_fill_OME() is exactly analagous.
#'
#' # Prepare example dataset with labelled factors
#' mtcars_small <- mtcars |>
#'   mutate(
#'     am = factor(am, levels = c(0, 1), labels = c("Auto", "Man")),
#'     cyl = factor(cyl, levels = c(4, 6, 8))
#'   )
#'
#' # Scatterplot using OME palette & theme for colour aesthetic
#' # (Not OME-like data and illustration would be better if the factor had more than two levels,
#' #  but hopefully illustrates use of scale_colour_OME() when colouring by a factor)
#' mtcars_small |>
#'   ggplot(aes(x = wt, y = mpg, colour = am)) +
#'   geom_point() +
#'   scale_colour_OME() +
#'   theme_OME() +
#'   labs(
#'     x = "Weight (/1,000 lb)",
#'     y = "Miles per gallon",
#'     colour = "Transmission",
#'     title = "Fuel efficiency vs weight,\ncoloured by transmission type"
#'   )
#'
#' # Use a sequential palette for an ordered factor ("divergent" is available too)
#' mtcars_small |>
#'   ggplot(aes(x = wt, y = mpg, colour = cyl)) +
#'   geom_point() +
#'   scale_colour_OME(type = "sequential") +
#'   theme_OME() +
#'   labs(
#'     x = "Weight (/1,000 lb)",
#'     y = "Miles per gallon",
#'     colour = "Cylinders",
#'     title = "Fuel efficiency vs weight,\ncoloured by cylinders"
#'   )
#' @name scale_colour_OME
NULL


#' @rdname scale_colour_OME
#' @export
scale_colour_OME <- function(type = "distinct", ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "OME",
    palette = function(n) OMESurvey::get_OME_colours(n = n, type = type),
    ...
  )
}


#' @rdname scale_colour_OME
#' @export
scale_fill_OME <- function(type = "distinct", ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "OME",
    palette = function(n) OMESurvey::get_OME_colours(n = n, type = type),
    ...
  )
}









#' Read and assemble survey data and dictionary inputs
#'
#' Reads the survey data, data dictionary, and optionally establishment
#' characteristics files, and prepares them for downstream processing.
#' This includes merging establishment characteristics onto the main data
#' (if provided), augmenting the dictionary with corresponding additional
#' variables, simple dictionary validation.
#'
#' This function performs input and structural preparation of the data and
#' preparation & validation of the dictionary.
#' \code{\link{survey_data_prepare}} is designed to take the
#' output of this function and validate & coerce-to-type the data.
#'
#' @param data_path Character string. Path to the survey data file
#'   (csv, xls, or xlsx).
#' @param dict_path Character string. Path to the data dictionary file
#'   (xls or xlsx).
#' @param dict_sheet Character string. Name of the sheet within the
#'   data dictionary file to use.
#'
#' @param est_chars_path Optional character string. Path to the
#'   establishment characteristics file (xls or xlsx). If \code{NULL}
#'   (default) no additional data are merged.
#' @param est_chars_sheet Optional character string. Name of the sheet
#'   within the establishment characteristics file to use. Required if
#'   \code{est_chars_path} is supplied.
#' @param est_char_vars Optional character vector. Names of establishment
#'   characteristics variables to include.
#' @param est_char_types Optional character vector. Data types for
#'   establishment characteristics variables (same format as dictionary).
#' @param est_char_values Optional character vector. Allowed values for
#'   establishment characteristics variables (as in the dictionary).
#' @param est_char_statements Optional character vector. Descriptive
#'   statements/labels for establishment characteristics variables.
#'
#' @return A list with components:
#' \describe{
#'   \item{data}{A tibble containing the survey data, merged with
#'   establishment characteristics if supplied.}
#'   \item{dict}{A tibble containing the data dictionary, augmented
#'   with any establishment characteristics entries supplied.}
#'   \item{messages}{A list of message objects describing any issues
#'   encountered during reading or merging (e.g. missing files,
#'   duplicate keys, unmatched joins).}
#' }
#'
#' @details
#' The establishment characteristics to be merged into the data are specified
#' through the variables `est_char_vars`, `est_char_types`, `est_char_values` and
#' `est_char_statements`. They are vectors with elements corresponding to the
#' variable name, data type, allowed values, item statement entries of the data
#' dictionary. See the preprocessing SOP for details/examples.
#'
#'
#' @seealso \code{\link{survey_data_prepare}}
#'
#' @export
survey_read_inputs <- function(
    data_path,
    dict_path,
    dict_sheet,
    est_chars_path = NULL,
    est_chars_sheet = NULL,
    est_char_vars = NULL,
    est_char_types = NULL,
    est_char_values = NULL,
    est_char_statements = NULL
) {
  # initialise messages
  messages <- list()


  # check that data, dict, est_chars are real files
  if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path)
  }

  if (!file.exists(dict_path)) {
    stop("Dictionary file not found: ", dict_path)
  }

  if (!is.null(est_chars_path) && !file.exists(est_chars_path)) {
    stop("Establishment characteristics file not found: ", dict_path)
  }


  # load data (with file type check) & dict
  data_ext <- tolower(tools::file_ext(data_path))
  data <- switch(data_ext,
                 "csv"  = readr::read_csv(data_path, show_col_types = FALSE),
                 "xlsx" = OMESurvey::safe_read_excel(data_path, guess_max = 1e6),
                 "xls"  = OMESurvey::safe_read_excel(data_path, guess_max = 1e6),
                 stop("Unsupported data file type: ", data_ext)
  )

  dict <- OMESurvey::safe_read_excel(dict_path,
                                     sheet = dict_sheet)


  # check that dictionary has required columns
  required_vars <- c("variable_name", "data_type", "allowed_values",
                     "item_statement", "report_sec",
                     "grouping_var", "condition")
  missing <- setdiff(required_vars, names(dict))

  if (length(missing) > 0) {
    stop("The following required variables are missing from the data dictionary: ",
         paste(missing, collapse = ", "),
         "\n Note that 'grouping_var' and 'condition' are required columns, though they may be otherwise empty.")
  }


  # check for duplicated variable names in dictionary
  dup_vars <- dict |>
    dplyr::count(variable_name) |>
    dplyr::filter(n > 1)

  if (nrow(dup_vars) > 0) {

    # identify duplicates assigned to report sections
    dup_with_sec <- dict |>
      dplyr::filter(variable_name %in% dup_vars$variable_name & !is.na(report_sec)) |>
      dplyr::distinct(variable_name)

    # identify duplicates used as grouping variables
    dup_grouping <- dict |>
      dplyr::filter(variable_name %in% dup_vars$variable_name & grouping_var == TRUE) |>
      dplyr::distinct(variable_name)

    # warn about duplicates
    messages <- add_message(messages, paste0(
      "Duplicate variable_name entries found in the data dictionary: ",
      paste(dup_vars$variable_name, collapse = ", ")
    ),
    level = "WARNING"
    )

    # error if duplicates affect report sections
    if (nrow(dup_with_sec) > 0) {
      stop(
        "Duplicate variable_name entries assigned to report sections: ",
        paste(dup_with_sec$variable_name, collapse = ", "),
        call. = FALSE
      )
    }

    # error if duplicates affect grouping variables
    if (nrow(dup_grouping) > 0) {
      stop(
        "Duplicate variable_name entries used as grouping variables: ",
        paste(dup_grouping$variable_name, collapse = ", "),
        call. = FALSE
      )
    }
  }


  # check for variables in dict but not data
  vars_not_in_data <- base::setdiff(
    dict |> dplyr::pull(variable_name),
    names(data)
  )

  if (length(vars_not_in_data) > 0) {
    messages <- add_message(messages, paste0(
      "Variables in the dictionary but not found in the data: ",
      paste(vars_not_in_data, collapse = ", ")
    ),
    level = "NOTE"
    )
  } else {
    messages <- add_message(messages, paste0(
      "All variables specified in the dictionary are present in the data."
    ),
    level = "NOTE"
    )
  }


  # check for variables in data but not dict
  vars_not_in_dict <- base::setdiff(
    names(data),
    dict |> dplyr::pull(variable_name)
  )

  if (length(vars_not_in_dict) > 0) {
    messages <- add_message(messages, paste0(
      "Variables in the data but not found in the dictionary: ",
      paste(vars_not_in_dict, collapse = ", ")
    ),
    level = "NOTE"
    )
  } else {
    messages <- add_message(messages, paste0(
      "All variables in the data are accounted for in the dictionary."
    ),
    level = "NOTE"
    )
  }


  # filter dict to variables in a report section
  dict <-
    dict |>
    dplyr::filter(!is.na(report_sec))


  # Now check for variables IN A SECTION but not in the data
  vars_not_in_data <-
    base::setdiff(dict |> dplyr::pull(variable_name),
                  data |> base::names())

  if (length(vars_not_in_data) != 0) {
    # remove from dictionary
    dict <-
      dict |>
      dplyr::filter(!(variable_name %in% vars_not_in_data))
    # and warn
    messages <- add_message(messages, paste0(
      "The following variables are given a section in the dictionary but cannot be found in the data: ",
      paste(vars_not_in_data, collapse = ", "),
      "They are being ignored for the rest of this report."
    ),
    level = "WARNING"
    )
  } else {
    messages <- add_message(messages,
      "All variables that are assigned to a section in the data dictionary were found in the data.",
      level = "NOTE"
    )
  }


  # Now merge in establishment characteristics if provided
  if (is.null(est_chars_path)){
    messages <- add_message(messages, paste0(
      "No establishment characteristics provided."
    ),
    level = "NOTE"
    )

  }else{

    # first check that the establishment ID variable EST_ID is present in the survey data
    num_EST_ID <- sum("EST_ID" == names(data))
    if (num_EST_ID != 1) {
      stop(paste0("The variable 'EST_ID' needs to appear in the data file once. But I can see it ",num_EST_ID," times."))
    }

    # load establishment characteristics file
    est_chars <-
      OMESurvey::safe_read_excel(
        est_chars_path,
        sheet = est_chars_sheet)

    # v simple checking of other est_chars parameters:
    # that they are of character type, 1-dimensional, all the same length
    stopifnot(is.character(est_char_vars) && is.null(dim(est_char_vars)))
    stopifnot(is.character(est_char_types) && is.null(dim(est_char_types)))
    stopifnot(is.character(est_char_values) && is.null(dim(est_char_values)))
    stopifnot(is.character(est_char_statements) && is.null(dim(est_char_statements)))
    stopifnot(length(est_char_vars) == length(est_char_types) &&
                length(est_char_vars) == length(est_char_values) &&
                length(est_char_vars) == length(est_char_statements))


    # check for variables specified but not in the data
    char_vars_not_in_data <-
      base::setdiff(est_char_vars,
                    est_chars |> base::names())

    if (length(char_vars_not_in_data) != 0) {
      messages <- add_message(messages, paste0(
        "The following establishment characteristics are requested to be included as grouping variables but cannot be found in the establishment characteristics data: ",
        paste(char_vars_not_in_data, collapse = ", ")
      ),
      level = "WARNING"
      )
    }

    # variables to add to the dictionary
    est_char_vars <- base::setdiff(est_char_vars, char_vars_not_in_data)

    # reduce to variables needed for the join
    est_chars <-
      est_chars |>
      dplyr::select(EstablishmentID, all_of(est_char_vars))

    # if there are some variables to work with
    if (length(est_char_vars) > 0){
      # join them to the data
      data <-
        data |>
        dplyr::left_join(est_chars,
                         by=dplyr::join_by(EST_ID == EstablishmentID))

      # add them to the data dictionary and, if numeric, calculate the appropriate x-iles
      for (i in seq_len(length(est_char_vars))){
        if (grepl("^factor-",est_char_types[i])){
          dict <-
            dict |>
            tibble::add_row(variable_name = est_char_vars[i],
                            data_type = est_char_types[i],
                            allowed_values = est_char_values[i],
                            item_statement = est_char_statements[i],
                            report_sec = "Grouping variables",
                            grouping_var = "T",
                            condition = NA)
        } else if (grepl("^numeric-",est_char_types[i])) {

          # first look up x-iles from the establishment characteristics spreadsheet
          if (!exists("est_stats")){
            est_stats <-
              OMESurvey::safe_read_excel(
                est_chars_path,
                sheet = switch (est_chars_sheet,
                                partner_characteristics_sec = "stats_sec",
                                partner_characteristics_pri = "stats_pri")
              )
          }


          get_est_xiles <- function(est_stats, var, type_label, label_max_num, sheet_name) {

            # check variable exists
            if (!var %in% est_stats$variable) {
              stop(
                "Variable '", var,
                "' not found in stats sheet (sheet: ", sheet_name, ").",
                call. = FALSE
              )
            }

            # determine regex pattern
            pattern <- switch(
              type_label,
              quintile = "/5$",
              tertile  = "/3$",
              quartile = "/[24]$",
              stop("Unknown type_label in get_est_xiles().", call. = FALSE)
            )

            # extract and process
            est_xiles <-
              est_stats |>
              dplyr::filter(variable == var, grepl(pattern, statistic)) |>
              dplyr::mutate(statistic = OMESurvey::frac_to_num(statistic)) |>
              dplyr::arrange(statistic) |>
              dplyr::pull(value)

            expected_n <- label_max_num - 1

            # checks
            if (length(est_xiles) == 0) {
              stop(
                "No statistics found for variable '", var,
                "' in stats sheet (sheet: ", sheet_name, "). ",
                "Expected ", expected_n, " cut points for ", type_label, ".",
                call. = FALSE
              )
            }

            if (length(est_xiles) != expected_n) {
              stop(
                "Incorrect number of statistics for variable '", var,
                "' in stats sheet (sheet: ", sheet_name, "). ",
                "Expected ", expected_n, " cut points for ", type_label,
                ", but found ", length(est_xiles), ".",
                call. = FALSE
              )
            }

            return(est_xiles)
          }


          type_suffix <- dplyr::case_when(
            endsWith(est_char_types[i], "-quintiles") ~ "quintile",
            endsWith(est_char_types[i], "-tertiles")  ~ "tertile",
            endsWith(est_char_types[i], "-quartiles") ~ "quartile",
            TRUE ~ NA_character_
          )


          # depending on xxx in numeric-xxx, look up stats and prepare other variables
          if (endsWith(est_char_types[i], "-quintiles")){
            label_prefix <- paste0(tools::toTitleCase(type_suffix), " ")
            label_max_num <- 5
            est_xiles <- get_est_xiles(
              est_stats = est_stats,
              var = est_char_vars[i],
              type_label = "quintile",
              label_max_num = label_max_num,
              sheet_name = est_chars_sheet
            )

          } else if (endsWith(est_char_types[i], "-tertiles")){
            label_prefix <- paste0(tools::toTitleCase(type_suffix), " ")
            label_max_num <- 3
            est_xiles <- get_est_xiles(
              est_stats = est_stats,
              var = est_char_vars[i],
              type_label = "tertile",
              label_max_num = label_max_num,
              sheet_name = est_chars_sheet
            )

          } else if (endsWith(est_char_types[i], "-quartiles")){
            label_prefix <- paste0(tools::toTitleCase(type_suffix), " ")
            label_max_num <- 4
            est_xiles <- get_est_xiles(
              est_stats = est_stats,
              var = est_char_vars[i],
              type_label = "quartile",
              label_max_num = label_max_num,
              sheet_name = est_chars_sheet
            )

          } else {
            stop("Unknown suffix in a `est_char_types` entry of the form numeric-xxx.")
          }





          if (is.na(type_suffix)) {
            stop(
              "Unknown numeric type in est_char_types for variable '",
              est_char_vars[i],
              "'. Expected one of -quintiles, -tertiles, -quartiles.",
              call. = FALSE
            )
          }



          # Construct the factor labels
          final_labels <- paste0(label_prefix, seq_len(label_max_num))

          final_labels[c(1, label_max_num)] <-
            paste0(
              final_labels[c(1, label_max_num)],
              c(" (Lowest)", " (Highest)")
            )

          # Add dictionary row with correct allowed values (semicolon-delimited)
          dict <- dict |>
            tibble::add_row(
              variable_name = est_char_vars[i],
              data_type = "factor-lo-hi",
              allowed_values = paste(final_labels, collapse = ";"),
              item_statement = paste(
                est_char_statements[i],
                type_suffix
              ),
              report_sec = "Grouping variables",
              grouping_var = "T",
              condition = NA
            )

          # Apply the cut() using the same labels
          data <- data |>
            dplyr::mutate("{est_char_vars[i]}_raw" := .data[[est_char_vars[i]]]) |>
            dplyr::mutate("{est_char_vars[i]}" :=
                            cut(.data[[paste0(est_char_vars[i], "_raw")]],
                                breaks = c(-Inf, est_xiles, Inf),
                                labels = final_labels))

        }
      }
    }

  }


  #Compute ordering within each section
  dict <-
    dict |>
    dplyr::mutate(report_sec = ifelse(is.na(report_sec) | stringr::str_trim(report_sec) == "",
                                      NA_integer_,
                                      report_sec)) |>
    dplyr::mutate(report_sec_ord = dplyr::if_else(is.na(report_sec),
                                                  NA_integer_,
                                                  dplyr::row_number()),
                  .by = report_sec)

  #Normalize grouping_var to logical
  if (!is.logical(dict$grouping_var)){
    dict <- dict |>
      dplyr::mutate(grouping_var = dplyr::case_when(
        tolower(as.character(grouping_var)) %in% c("true","t","yes","y","1") ~ TRUE,
        tolower(as.character(grouping_var)) %in% c("false","f","no","n","0") ~ FALSE,
        .default = FALSE
      ))
  }

  return(list(
    data = data,
    dict = dict,
    messages = messages
  ))

}






#' Validate and coerce survey data according to a data dictionary
#'
#' Applies validation and coercion rules to survey data using a supplied
#' data dictionary. This includes enforcing data types, applying allowed
#' values, handling conditional logic, and producing a structured
#' validation log.
#'
#' This function operates on in-memory data and a dictionary, usually the output from
#' \code{\link{survey_read_inputs}}. That function reads files and validates
#' the dictionary, this one uses the dictionary to transform & validate the data.
#'
#' @param data A tibble containing the survey data, typically the \code{data}
#'   component returned by \code{\link{survey_read_inputs}}. This may already
#'   include merged establishment characteristics.
#' @param dict A tibble containing the data dictionary, typically the \code{dict}
#'   component returned by \code{\link{survey_read_inputs}}.
#'
#' @return A list with components:
#' \describe{
#'   \item{data}{A tibble containing the processed survey data, with variables
#'   coerced to their specified types (e.g. factors, numeric) and with
#'   allowed values enforced.}
#'   \item{validation}{A tibble summarising validation checks applied to each
#'   variable, including counts of values meeting or failing specified
#'   conditions.}
#'   \item{messages}{A list of message objects describing any issues
#'   encountered during validation or coercion (e.g. missing variables,
#'   invalid values, failed conditions).}
#' }
#'
#' @seealso \code{\link{survey_read_inputs}}
#'
#' @export
survey_data_prepare <- function(
    data,
    dict
) {
  # Helper: parse allowed values for factors
  parse_allowed <- function(x){
    if(is.na(x) || stringr::str_trim(x) == "") return(NULL)
    vals <- unlist(strsplit(as.character(x), ";", fixed = TRUE))
    vals <- stringr::str_trim(vals)
    vals <- vals[vals != ""]
    if(length(vals) == 0) return(NULL)
    vals
  }



  # Helper: parse allowed ranges for numeric variables
  parse_allowed_range <- function(x) {

    if (is.na(x)) return(NULL)

    s <- trimws(as.character(x))
    if (!nzchar(s)) return(NULL)

    parse_inf <- function(z) {
      z <- trimws(z)
      if (grepl("^(\\+?inf)$", z, ignore.case = TRUE)) return(Inf)
      if (grepl("^(-inf)$", z, ignore.case = TRUE)) return(-Inf)
      suppressWarnings(as.numeric(z))
    }

    ## 1) Bracketed interval: [a,b), (a,Inf], etc.
    if (nchar(s) >= 5 && substr(s, 1, 1) %in% c("[", "(") &&
        substr(s, nchar(s), nchar(s)) %in% c("]", ")")) {

      lower_incl <- substr(s, 1, 1) == "["
      upper_incl <- substr(s, nchar(s), nchar(s)) == "]"

      inner <- substr(s, 2, nchar(s) - 1)
      parts <- strsplit(inner, ",", fixed = TRUE)[[1]]

      if (length(parts) == 2) {
        return(list(
          lower = parse_inf(parts[1]),
          upper = parse_inf(parts[2]),
          lower_incl = lower_incl,
          upper_incl = upper_incl
        ))
      }
    }

    ## 2) Dash form: a-b (inclusive)
    if (grepl("-", s, fixed = TRUE)) {
      parts <- strsplit(s, "-", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        return(list(
          lower = parse_inf(parts[1]),
          upper = parse_inf(parts[2]),
          lower_incl = TRUE,
          upper_incl = TRUE
        ))
      }
    }

    ## 3) Relational forms: >5, >=0, <10, <=100
    if (grepl("^(>=|>)", s)) {
      op <- sub("^\\s*(>=|>).*$", "\\1", s)
      val <- parse_inf(sub("^\\s*(>=|>)", "", s))

      return(list(
        lower = val,
        upper = Inf,
        lower_incl = op == ">=",
        upper_incl = FALSE
      ))
    }

    NULL
  }

  #helper: coerce a column of data
  coerce_col <- function(x, type, levels = NULL) {
    if (grepl("integer", type, ignore.case = TRUE)) {
      return(as.integer(x))
    } else if (grepl("numeric", type, ignore.case = TRUE)) {
      res <- suppress_specific_warning(as.numeric(x), "NAs introduced by coercion")
      return(res$value)      # numeric result with NA
      #res$suppressed # is a character vector of suppressed messages (if any)
    } else if (grepl("binary", type, ignore.case = TRUE)) {
      return(as.factor(as.logical(x)))
    } else if (grepl("date", type, ignore.case = TRUE)) {
      return(as.Date(x))
    } else if (grepl("factor-", type, ignore.case = TRUE)) {
      if (!is.null(levels)) {
        return(factor(x, levels = levels))
      } else {
        return(factor(x))
      }
    } else if (grepl("text", type, ignore.case = TRUE)) {
      return(as.character(x))
    } else {
      return(x)
    }
  }


  # helper: build a single validation row
  make_validation_row <- function(var, type, colo=list(NA), order_values=list(NA),
                                  diag_table = NULL,
                                  allowed_specified, allowed_display, allowed_technical,
                                  n_missing, n_non_allowed, non_allowed_freq,
                                  fail_cond_freq,
                                  report_sec, report_sec_ord, item_label,
                                  condition = NA_character_, n_fail_cond = NA_integer_,
                                  n_meet_cond = NA_integer_,
                                  n_meet_cond_missing = NA_integer_,
                                  n_meet_cond_non_allowed = NA_integer_
  ) {
    list(
      variable = var,
      type = type,
      colo = colo,
      order_values = order_values,
      diag_table = list(diag_table),
      allowed_specified = allowed_specified,
      allowed_display = if (allowed_specified && !is.null(allowed_display)) {
        if (length(allowed_display) > 1) paste(allowed_display, collapse = ";") else allowed_display
      } else NA_character_,
      allowed_technical = list(allowed_technical),
      n_missing = n_missing,
      n_non_allowed = n_non_allowed,
      non_allowed_freq = list(non_allowed_freq),
      fail_cond_freq = list(fail_cond_freq),
      condition = condition,
      n_fail_cond = n_fail_cond,
      n_meet_cond = n_meet_cond,
      n_meet_cond_missing = n_meet_cond_missing,
      n_meet_cond_non_allowed = n_meet_cond_non_allowed,
      report_sec = report_sec,
      report_sec_ord = report_sec_ord,
      item_statement = item_label
    )
  }



  make_diagnostic_table <- function(class_vec, meet_mask = NULL, has_valid_cond = FALSE) {

    # fixed row order
    row_levels <- c("allowed", "-999", "-888", "-777", "other_non_allowed", "missing")

    # build column grouping
    if (has_valid_cond) {
      col_vec <- ifelse(meet_mask, "Condition met", "Condition not met")
      col_levels <- c("Condition met", "Condition not met")
    } else {
      col_vec <- rep("Total", length(class_vec))
      col_levels <- "Total"
    }

    # build contingency table (always 2D)
    tab <- table(
      factor(class_vec, levels = row_levels),
      factor(col_vec, levels = col_levels)
    )

    # convert to data frame
    diag_df <- as.data.frame.matrix(tab)

    # ensure all rows present and ordered
    diag_df <- diag_df[row_levels, , drop = FALSE]

    # replace NA with 0
    diag_df[is.na(diag_df)] <- 0

    # add row totals if we are in a situation with multiple columns
    if (has_valid_cond) {
      diag_df$Total <- rowSums(diag_df)
    }

    # add column totals (bottom row)
    diag_df <- rbind(diag_df, Total = colSums(diag_df))

    # polish row names
    rownames(diag_df) <- c(
      "Allowed",
      "-999 (Missing)",
      "-888 (Not branched)",
      "-777 (Invalid)",
      "Non-allowed & non-'-xxx'",
      "NA (Missing in data)",
      "Total"
    )

    return(diag_df)
  }



  # initialise validation_log and messages list
  validation_log <- vector("list", nrow(dict))
  messages <- list()

  # The main validation loop
  for (i in seq_len(nrow(dict))) {
    var <- dict$variable_name[i]

    # debug step
    # message(">>> Starting variable: ", var)
    # flush.console()

    #tryCatch({

      # extract key info for easier reference in this loop
      dtype <- tolower(dict$data_type[i])
      allowed_raw <- dict$allowed_values[i]
      allowed <- parse_allowed(allowed_raw)           # returns NULL or character vector
      # only consider allowed values meaningful for factor-like types
      is_allowed_type <- grepl("factor|numerical", dtype, ignore.case = TRUE)
      allowed_specified <- is_allowed_type && !is.null(allowed)
      if (!is_allowed_type) allowed <- NULL  # drop allowed for non-factors to avoid confusion
      report_sec <- dict$report_sec[i]
      report_sec_ord <- dict$report_sec_ord[i]
      item_label <- dict$item_statement[i]  # remove line breaks here ?!?

      # make _raw copy of variable & coerce it
      raw <- data[[var]]
      raw_chr <- as.character(raw)
      data[[paste0("raw_", var)]] <- raw_chr

      data[[var]] <- coerce_col(raw, dtype, levels = allowed)


      # initialise some things - check location (& if others should be here too) later!
      non_allowed_mask <- rep(FALSE, nrow(data))
      fail_cond_freq <- list()


      # parse and evaluate condition (if present) and then condition masks and n_xxx variables
      # (about numbers of records the meet/don't the condition, are invalid/missing, etc)

      cond_str <- if ("condition" %in% names(dict)) dict$condition[i] else NA_character_
      cond_expr <- NULL
      cond_mask <- NULL

      has_valid_cond <- FALSE
      meet_mask <- NULL
      fail_mask <- NULL

      n_fail_cond <- NA_integer_
      n_meet_cond <- NA_integer_
      n_meet_cond_missing <- NA_integer_
      n_meet_cond_non_allowed <- NA_integer_

      # parse and evaluate condition
      if (!is.na(cond_str) && nzchar(cond_str)) {

        cond_expr <- tryCatch(
          rlang::parse_expr(cond_str),
          error = function(e) {
            stop("\n**Error parsing condition for variable", var, ":** ", e$message, "\n\n", sep = " ")
            NULL
          }
        )

        if (!is.null(cond_expr)) {
          cond_mask <- tryCatch(
            rlang::eval_tidy(cond_expr, data = data),
            error = function(e) {
              stop("\n**Error evaluating condition for variable", var, ":** ", e$message, "\n\n", sep = " ")
              NULL
            }
          )
        }


        # cat("\nDEBUG: cond_mask class:", paste(class(cond_mask), collapse = ", "), "\n")
        # cat("DEBUG: cond_mask typeof:", typeof(cond_mask), "\n")
        # cat("DEBUG: length(cond_mask):", length(cond_mask), "\n")
        # cat("DEBUG: nrow(data):", nrow(data), "\n")
        # cat("DEBUG: first 10 values of cond_mask:\n")
        # print(head(cond_mask, 10))
        #
        # cat("DEBUG: is.logical(cond_mask):", is.logical(cond_mask), "\n")
        #
        #
        # cat("DEBUG: is.logical(as.logical(cond_mask)):", is.logical(as.logical(cond_mask)), "\n")
        # cat("DEBUG: first 10 values after coercion:\n")
        # print(head(as.logical(cond_mask), 10))
        #
        #
        # cat("DEBUG: table(cond_mask, useNA='always'):\n")
        # print(table(cond_mask, useNA = "always"))


      }

      # validate mask and define meet/fail masks
      if (!is.null(cond_mask) &&
          is.logical(cond_mask) &&
          length(cond_mask) == nrow(data)) {

        has_valid_cond <- TRUE

        # meet_mask TRUE iff cond_mask is TRUE, but use %in% so NA becomes FALSE
        # fail_mask is the complement (TRUE when cond_mask is FALSE or NA)
        meet_mask <- cond_mask %in% TRUE
        fail_mask <- !meet_mask

      } else {

        if (!is.null(cond_mask)) {
          messages <- add_message(messages, paste0(
            "Condition for variable ", var,
            " did not evaluate to a logical vector of length ",
            nrow(data),
            ". Condition will be ignored."
          ),
          level = "NOTE"
          )
        }

        has_valid_cond <- FALSE
        meet_mask <- rep(TRUE, nrow(data))
        fail_mask <- NULL
      }

      # cat("DEBUG: has_valid_cond:", has_valid_cond, "\n")
      #
      # cat("DEBUG: first 10 values of meet_mask:\n")
      # print(head(meet_mask, 10))
      # cat("DEBUG: first 10 values of fail_mask:\n")
      # print(head(fail_mask, 10))


      # compute condition counts
      if (has_valid_cond) {
        n_meet_cond <- sum(meet_mask)
        n_fail_cond <- sum(fail_mask)
      } else {
        n_meet_cond <- NA_integer_
        n_fail_cond <- NA_integer_
      }



      # Build & initialise diagnostic classification
      # --------------------------------------------
      sentinel_999 <- raw_chr %in% c("-999", -999)
      sentinel_888 <- raw_chr %in% c("-888", -888)
      sentinel_777 <- raw_chr %in% c("-777", -777)

      class_vec <- rep(NA_character_, length(raw_chr))

      class_vec[is.na(raw_chr)] <- "missing"



      #  factors processing...
      if (grepl("^factor-", stringr::str_squish(dtype), ignore.case = TRUE)) {

        # they use allowed; compute non_allowed_mask from raw_chr
        if (allowed_specified) {
          non_allowed_mask <- !is.na(raw_chr) & !(raw_chr %in% allowed)
        } else {
          non_allowed_mask <- rep(FALSE, length(raw_chr))
        }


        # classification for factors

        class_vec[!is.na(raw_chr) & !non_allowed_mask &
                    !(sentinel_999 | sentinel_888 | sentinel_777)] <- "allowed"
        class_vec[sentinel_999] <- "-999"
        class_vec[sentinel_888] <- "-888"
        class_vec[sentinel_777] <- "-777"

        # other non-allowed (exclude sentinel codes)
        class_vec[non_allowed_mask & !(sentinel_999 | sentinel_888 | sentinel_777)] <- "other_non_allowed"

        # make diagnostic table
        diag_df <- make_diagnostic_table(class_vec, meet_mask, has_valid_cond)



        if (has_valid_cond) {
          n_meet_cond_missing <-
            sum(meet_mask & is.na(raw))
          n_meet_cond_non_allowed <-
            sum(meet_mask & non_allowed_mask)
        }

        # then make a frequency table of non-allowed responses
        non_allowed_freq <- list()

        bad_vals <- if (has_valid_cond) {
          raw_chr[meet_mask & non_allowed_mask]
        } else {
          raw_chr[non_allowed_mask]
        }

        if (length(bad_vals) > 0) {
          non_allowed_freq$values <-
            sort(table(bad_vals), decreasing = TRUE)
        }

        # frequency table where condition NOT met
        if (!is.null(fail_mask)) {

          vals_fail <- raw_chr[fail_mask]

          if (length(vals_fail) > 0) {
            fail_cond_freq$values <-
              sort(table(vals_fail), decreasing = TRUE)
          }
        }


        # Determine colour scheme
        # and how to order questions in section-level overall plots
        switch(stringr::str_remove(dtype, "^factor-"),
               `neg-pos`={
                 colo <- OMESurvey::get_OME_colours(length(allowed), type="contrast")
                 order_values <- list(allowed[seq(from=ceiling(length(allowed)/2)+1, to=length(allowed))])
               },
               `lo-hi`={
                 colo <- OMESurvey::get_OME_colours(length(allowed), type="complementary")
                 order_values <- list("mean(as.numeric())")
               },
               unordered={
                 colo <- OMESurvey::get_OME_colours(length(allowed), type="distinct")
                 order_values <- list(NA)
               },
               stop("Unknown factor type (in colour scheme determination)")
        )

        names(colo) <- allowed
        colo <- list(colo)

        # cat("\nDEBUG FINAL COUNTS: meet =", n_meet_cond, " fail =", n_fail_cond, "\n")


        names(validation_log)[i] <- var
        validation_log[[i]] <- make_validation_row(
          var = var,
          type = "factor",
          colo = colo,
          order_values = order_values,
          diag_table = diag_df,
          allowed_specified = allowed_specified,
          allowed_display = allowed,
          allowed_technical = allowed,
          n_missing = sum(is.na(raw)),
          n_non_allowed = sum(non_allowed_mask, na.rm = TRUE),
          non_allowed_freq = non_allowed_freq,
          fail_cond_freq = fail_cond_freq,
          report_sec = report_sec,
          report_sec_ord = report_sec_ord,
          item_label = item_label,
          condition = if (!is.na(cond_str) && nzchar(cond_str)) cond_str else NA_character_,
          n_fail_cond = n_fail_cond,
          n_meet_cond = n_meet_cond,
          n_meet_cond_missing = n_meet_cond_missing,
          n_meet_cond_non_allowed = n_meet_cond_non_allowed
        )


        # numeric processing
      } else if (grepl("^numeric$|double", dtype, ignore.case = TRUE)) {
        # detect a range spec like [0,100] or (0,100] or 0-100
        range_spec <- parse_allowed_range(allowed_raw)

        # data[[var]] is the coerced numeric; raw_chr is the original character representation
        coerced <- data[[var]]

        out_of_range <- NULL

        # non-allowed if original non-missing but coercion produced NA (bad numeric text)
        non_allowed_coercion <- !is.na(raw_chr) & is.na(coerced)

        if (!is.null(range_spec)) {
          # compute out-of-range mask for non-NA coerced values
          out_low <-
            if (range_spec$lower_incl){
              coerced < range_spec$lower
            } else {
              coerced <= range_spec$lower
            }
          out_high <-
            if (range_spec$upper_incl){
              coerced > range_spec$upper
            } else {
              coerced >= range_spec$upper
            }
          out_of_range <- (!is.na(coerced)) & (out_low | out_high)


          non_allowed_mask <-   ### duplicated ~30 lines below?
            non_allowed_coercion |
            (!is.null(out_of_range) & out_of_range)

          if (has_valid_cond) {
            n_meet_cond_missing <-
              sum(meet_mask & is.na(raw))
            n_meet_cond_non_allowed <-
              sum(meet_mask & non_allowed_mask)
          }

          # construct allowed_for_report for the validation log
          allowed_specified_num <- TRUE
          allowed_for_report <- paste0(
            if (range_spec$lower_incl) "[" else "(",
            range_spec$lower, ",", range_spec$upper,
            if (range_spec$upper_incl) "]" else ")"
          )

        } else {
          # no numeric range specified: only mark coercion failures as non-allowed
          #non_allowed_mask <- non_allowed_coercion
          allowed_specified_num <- FALSE
          allowed_for_report <- NULL
        }


        non_allowed_mask <-   ### duplicated ~30 lines above?
          non_allowed_coercion |
          (!is.null(out_of_range) & out_of_range)


        non_allowed_freq_numeric <- list()

        ## (1) Raw values that failed numeric coercion
        if (any(non_allowed_coercion, na.rm = TRUE)) {
          non_allowed_freq_numeric$coercion <-
            sort(table(raw_chr[meet_mask & non_allowed_coercion]), decreasing = TRUE)
        }

        ## (2) Numeric values outside the allowed range
        if (!is.null(out_of_range) && any(out_of_range, na.rm = TRUE)) {
          non_allowed_freq_numeric$out_of_range <-
            sort(table(coerced[meet_mask & out_of_range]), decreasing = TRUE)
        }

        ## Normalise empty case
        if (length(non_allowed_freq_numeric) == 0) {
          non_allowed_freq_numeric <- list()
        }


        # frequency table where condition NOT met
        fail_cond_freq <- list()

        if (!is.null(fail_mask)) {

          vals_fail <- raw_chr[fail_mask]

          if (length(vals_fail) > 0) {
            fail_cond_freq$values <-
              sort(table(vals_fail), decreasing = TRUE)
          }
        }



        # classification for numeric
        class_vec[!is.na(coerced) & !non_allowed_mask] <- "allowed"
        class_vec[sentinel_999] <- "-999"
        class_vec[sentinel_888] <- "-888"
        class_vec[sentinel_777] <- "-777"

        # other non-allowed = out_of_range OR coercion
        class_vec[non_allowed_mask & !(sentinel_999 | sentinel_888 | sentinel_777)] <- "other_non_allowed"

        # make diagnostic table
        diag_df <- make_diagnostic_table(class_vec, meet_mask, has_valid_cond)






        names(validation_log)[i] <- var
        validation_log[[i]] <- make_validation_row(
          var = var,
          type = "numeric",
          diag_table = diag_df,
          allowed_specified = allowed_specified_num,
          allowed_display = if (allowed_specified_num) allowed_for_report else NULL,
          allowed_technical = if (!is.null(range_spec)) range_spec else NULL,
          n_missing = sum(is.na(raw)),
          n_non_allowed =
            sum(non_allowed_coercion, na.rm = TRUE) +
            if (!is.null(out_of_range)) sum(out_of_range, na.rm = TRUE) else 0L,
          non_allowed_freq = non_allowed_freq_numeric,
          fail_cond_freq = fail_cond_freq,
          report_sec = report_sec,
          report_sec_ord = report_sec_ord,
          item_label = item_label,
          condition = if (!is.na(cond_str) && nzchar(cond_str)) cond_str else NA_character_,
          n_fail_cond = n_fail_cond,
          n_meet_cond = n_meet_cond,
          n_meet_cond_missing = n_meet_cond_missing,
          n_meet_cond_non_allowed = n_meet_cond_non_allowed
        )


        # } else if (grepl("integer", dtype, ignore.case = TRUE)) {
        #     names(validation_log)[i] <- var
        #     validation_log[[i]] <- make_validation_row(
        #     var = var,
        #     type = "integer",
        #     allowed_specified = FALSE,
        #     allowed_display = NULL,
        #     allowed_technical = NULL,
        #     n_missing = sum(is.na(raw)),
        #     n_non_allowed = NA_integer_, # sum(non_allowed_mask, na.rm = TRUE),
        #     non_allowed_freq = non_allowed_freq,
        #     fail_cond_freq = fail_cond_freq,
        #     report_sec = report_sec,
        #     report_sec_ord = report_sec_ord,
        #     item_label = item_label,
        #     condition = if (!is.na(cond_str) && nzchar(cond_str)) cond_str else NA_character_,
        #     n_fail_cond = n_fail_cond,
        #     n_meet_cond = n_meet_cond,
        #     n_meet_cond_missing = n_meet_cond_missing,
        #     n_meet_cond_non_allowed = n_meet_cond_non_allowed
        #   )

      } else if (grepl("date", dtype, ignore.case = TRUE)) {
        names(validation_log)[i] <- var
        validation_log[[i]] <- make_validation_row(
          var = var,
          type = "date",
          allowed_specified = FALSE,
          allowed_display = NULL,
          allowed_technical = NULL,
          n_missing = sum(is.na(raw)),
          n_non_allowed = NA_integer_, # sum(non_allowed_mask, na.rm = TRUE),
          non_allowed_freq = list(),
          fail_cond_freq = fail_cond_freq,
          report_sec = report_sec,
          report_sec_ord = report_sec_ord,
          item_label = item_label,
          condition = if (!is.na(cond_str) && nzchar(cond_str)) cond_str else NA_character_,
          n_fail_cond = n_fail_cond,
          n_meet_cond = n_meet_cond,
          n_meet_cond_missing = n_meet_cond_missing,
          n_meet_cond_non_allowed = n_meet_cond_non_allowed
        )

      } else {
        # character / fallback



        nonresponse_codes <- c("-999", "-888", "-777")

        non_allowed_mask <-
          !is.na(data[[var]]) &
          data[[var]] %in% nonresponse_codes

        # compute condition stats
        if (has_valid_cond) {
          n_meet_cond_missing <- sum(meet_mask & is.na(raw))
          n_meet_cond_non_allowed <- sum(meet_mask & non_allowed_mask)
        }


        # Build non-allowed frequency table for text variables
        # (depending on whether or not there is a condition)
        non_allowed_freq <- list()

        if (has_valid_cond) {
          bad_vals <- data[[var]][meet_mask & non_allowed_mask]
        } else {
          bad_vals <- data[[var]][non_allowed_mask]
        }

        if (length(bad_vals) > 0) {
          non_allowed_freq$values <-
            sort(table(bad_vals), decreasing = TRUE)
        }


        # frequency table where condition NOT met
        fail_cond_freq <- list()

        if (!is.null(fail_mask)) {

          vals_fail <- data[[var]][fail_mask]

          if (length(vals_fail) > 0) {
            fail_cond_freq$values <-
              sort(table(vals_fail), decreasing = TRUE)
          }
        }



        # generic fallback classification
        class_vec[sentinel_999] <- "-999"
        class_vec[sentinel_888] <- "-888"
        class_vec[sentinel_777] <- "-777"
        class_vec[is.na(class_vec) & !is.na(raw_chr)] <- "allowed"


        # make diagnostic table
        diag_df <- make_diagnostic_table(class_vec, meet_mask, has_valid_cond)



        names(validation_log)[i] <- var
        validation_log[[i]] <- make_validation_row(
          var = var,
          type = "character",
          diag_table = diag_df,
          allowed_specified = FALSE,
          allowed_display = NULL,
          allowed_technical = NULL,
          n_missing = sum(is.na(raw)),
          n_non_allowed = NA_integer_, # sum(non_allowed_mask, na.rm = TRUE),
          non_allowed_freq = non_allowed_freq,
          fail_cond_freq = fail_cond_freq,
          report_sec = report_sec,
          report_sec_ord = report_sec_ord,
          item_label = item_label,
          condition = if (!is.na(cond_str) && nzchar(cond_str)) cond_str else NA_character_,
          n_fail_cond = n_fail_cond,
          n_meet_cond = n_meet_cond,
          n_meet_cond_missing = n_meet_cond_missing,
          n_meet_cond_non_allowed = n_meet_cond_non_allowed
        )
      }

      # checking validation_log entries/construction

      # message(">>> Names in validation_log entry")
      # print(names(validation_log[[var]]))
      #
      # message(">>> Size of validation_log entry")
      # print(object.size(validation_log[[var]]), units = "MB")
      #
      # message(">>> Size of whole validation_log so far")
      # print(object.size(validation_log), units = "MB")

      # message(">>> non_allowed_mask length and summary")
      # mask <- validation_log[[var]]$non_allowed_mask
      # if (is.null(mask)) {
      #   message("non_allowed_mask is NULL")
      # } else {
      #   message("length(mask) = ", length(mask))
      #   message("sum(mask) = ", sum(mask, na.rm = TRUE))
      #   message("sum(!mask) = ", sum(!mask, na.rm = TRUE))
      # }


    # }, error = function(e) {
    #
    #   stop(
    #     paste0(
    #       "\nError while processing variable '", var, "':\n",
    #       e$message
    #     ),
    #     call. = FALSE
    #   )
    #
    # })


  }

  # make a tibble from the validation_log
  validation_df <- dplyr::bind_rows(validation_log)


  return(list(
    data = data,
    validation_log = validation_log,
    validation_df = validation_df,
    messages = messages
  ))

}




#' Read, merge, validate, and prepare survey data in one step
#'
#' Convenience wrapper that performs the full survey data preparation
#' pipeline by combining \code{\link{survey_read_inputs}} and
#' \code{\link{survey_data_prepare}}. This includes reading input files,
#' merging establishment characteristics (if supplied), validating and
#' coercing variables according to the data dictionary, and returning
#' both processed data and diagnostic outputs.
#'
#' @param data_path Character string. Path to the survey data file
#'   (csv, xls, or xlsx).
#' @param dict_path Character string. Path to the data dictionary file
#'   (xls or xlsx).
#' @param dict_sheet Character string. Name of the sheet within the
#'   data dictionary file to use.
#'
#' @param est_chars_path Optional character string. Path to the
#'   establishment characteristics file (xls or xlsx). If \code{NULL}
#'   (default) no additional data are merged.
#' @param est_chars_sheet Optional character string. Name of the sheet
#'   within the establishment characteristics file to use. Required if
#'   \code{est_chars_path} is supplied.
#' @param est_char_vars Optional character vector. Names of establishment
#'   characteristics variables to include.
#' @param est_char_types Optional character vector. Data types for
#'   establishment characteristics variables (same format as dictionary).
#' @param est_char_values Optional character vector. Allowed values for
#'   establishment characteristics variables (as in the dictionary).
#' @param est_char_statements Optional character vector. Descriptive
#'   statements/labels for establishment characteristics variables.
#'
#' @return A list with components:
#' \describe{
#'   \item{data}{A tibble containing the fully prepared survey data,
#'   including merged establishment characteristics (if supplied) and
#'   variables coerced to their specified types.}
#'   \item{validation_log}{A list summarising validation checks applied
#'   to each variable.}
#'   \item{validation_df}{A tibble made as `dplyr::bind_rows(validation_log)`
#'   (easier to access for some purposes).}
#'   \item{read_messages}{A list of message objects generated during
#'   input reading and merging.}
#'   \item{prep_messages}{A list of message objects generated during
#'   validation and coercion.}
#' }
#'
#' @seealso
#' \code{\link{survey_read_inputs}}, \code{\link{survey_data_prepare}}
#'
#' @export
survey_prepare_data <- function(
    data_path,
    dict_path,
    dict_sheet,
    est_chars_path = NULL,
    est_chars_sheet = NULL,
    est_char_vars = NULL,
    est_char_types = NULL,
    est_char_values = NULL,
    est_char_statements = NULL
) {
  # first read & put together inputs
  read_out <- survey_read_inputs(data_path,
                                 dict_path,
                                 dict_sheet,
                                 est_chars_path,
                                 est_chars_sheet,
                                 est_char_vars,
                                 est_char_types,
                                 est_char_values,
                                 est_char_statements)

  # then use those to do data-preparation
  prep_out <- survey_data_prepare(read_out$data,
                                  read_out$dict)

  return(list(
    data = prep_out$data,
    validation_log = prep_out$validation_log,
    validation_df = prep_out$validation_df,
    messages_read = read_out$messages,
    messages_prep = prep_out$messages
  ))

}





#' Append a message to a message list
#'
#' Helper to add a structured message (with level and text) to a list
#' of messages, returning the updated list. Intended for internal use
#' within survey data preparation functions.
#'
#' @param messages A list of message objects.
#' @param text Character string giving the message text.
#' @param level Character string indicating severity (e.g. "NOTE",
#'   "WARNING"). Defaults to "NOTE".
#' @param var Optional character string giving the associated variable name.
#'
#' @return The updated list of messages.
#'
#' @keywords internal
add_message <- function(messages, text, level = "NOTE", var = NULL) {
  messages[[length(messages) + 1]] <- list(
    level = level,
    text  = paste0(text),
    var   = var
  )
  messages
}





#' Narrow kable table with standard styling
#'
#' Convenience wrapper around \code{knitr::kable()} that applies
#' \code{kableExtra::kable_styling()} with sensible defaults for
#' compact, centred tables in reports.
#'
#' @param x An object to be converted to a table by \code{knitr::kable()}.
#' @param ... Additional arguments passed to \code{knitr::kable()}.
#' @param full_width Logical; whether the table should span the full
#'   page width. Defaults to \code{FALSE}.
#' @param position Character string controlling table positioning
#'   (e.g. \code{"center"}, \code{"left"}, \code{"right"}).
#'
#' @return A styled \code{kableExtra} table object.
#'
#' @export
kable_narrow <- function(x, ..., full_width = FALSE, position = "center") {
  knitr::kable(x, ...) |>
    kableExtra::kable_styling(full_width = full_width, position = position)
}




#' Format a NOTE or WARNING message for report output
#'
#' Creates a formatted HTML string representing a message with a
#' severity level (e.g. NOTE or WARNING), suitable for use in
#' R Markdown documents with \code{results = 'asis'}.
#'
#' @param ... Character content of the message (concatenated).
#' @param level Character string indicating message severity.
#'   One of \code{"WARNING"} or \code{"NOTE"} (default).
#'
#' @return A character string containing HTML-formatted message text.
#'
#' @export
report_notice <- function(..., level = c("WARNING", "NOTE")) {
  level <- match.arg(level)

  colour <- switch(level,
                   WARNING = "red",
                   NOTE = "black"
  )

  paste0(
    "\n\n",
    '<span style="color: ', colour, ';"><b>', level, ':</b></span> ',
    paste0(..., collapse = ""),
    "\n\n"
  )
}
