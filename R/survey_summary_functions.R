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
#' @param dict_path,dict_sheet path to the data dictionary (assumed to be xls/xlsx) and name of sheet in that file to use.
#' @param output_file name of output html file (needs to end with ".html", defaults to "<name of data file>_summary.html")
#' @param output_dir (optional) directory to save the output file to. Defaults to current working directory. Ignored if `output_file` is not specified.
#' @param output_title (optional) text string to use as title for html report output
#' @param output_author (optional) text string to use as author for html report output
#' @param output_date (optional) text string to use as date for html report output
#' @param overwrite (optional) whether to overwrite `output_file` if it already exists. Defaults to FALSE for safety but in use probably TRUE will be more typical.
#' @param est_chars_path (optional) path to establishment characteristics data (xls/xlsx).
#' @param est_chars_sheet (optional, needed if `est_chars_path` is used) name of sheet in est't chars to use.
#' @param est_char_vars,est_char_types,est_char_values,est_char_statements (optional, needed if `est_chars_path` is used) specification of establishment characteristics to use, see Details.
#' @param quiet (optional) whether to quieten the Rmd rendering information. Defaults to TRUE; FALSE useful for testing.
#' @param verbose (optional) whether to include extra/verbose detail in the report. Defaults to FALSE; TRUE useful for testing.
#' @param show (optional) whether to show intermediate results in the RStudio Viewer. Defaults to FALSE; unclear when TRUE might be useful.
#' @param rmd (optional) path to .Rmd document to use for report-making. Using anything other than the default should be rare.
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
#'   output_dir = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/Documents/survey-first-analysis/",
#'   output_file = "y12_summary.html",
#'   output_title = "Y12 student survey summary",
#'   output_author = "D Sirl",
#' )
#'
#' # Using establishment characteristics too
#' render_survey_summary(
#'   data_path = "C:/Users/pmzdjs/The University of Nottingham/OME - Higher Cohort - Documents/Advanced_years 12 & 13/4. Analysis/Student survey/Cycle 1 2024-25/20251029-OME-Year-12-Student-Survey-2024-25-pseudonymised.xlsx",
#'   dict_path = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/OME - Cohort Studies - SAG documents/20250507_data dictionary_master copy.xlsx",
#'   dict_sheet = "pupil_survey_Y12",
#'   output_dir = "C:/Users/pmzdjs/OneDrive - The University of Nottingham/Documents/survey-first-analysis/",
#'   output_file = "y12_summary.html",
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
                                  verbose = FALSE,
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


  # check that data & dict are real files
  if (!file.exists(data_path)) {
    stop("File not found: ", data_path)
  }

  if (!file.exists(dict_path)) {
    stop("File not found: ", dict_path)
  }

  # check data file type (if changing see "data_ext <-" in the .Rmd)
  if (!(tolower(tools::file_ext(data_path)) %in% c("csv", "xls", "xlsx"))){
    stop("Unsupported data file type: ", ext)
  }

  is_file_openable <- function(path) {
    con <- try(file(path, open = "rb"), silent = TRUE)
    if (inherits(con, "try-error")) {
      return(FALSE)
    }
    close(con)
    TRUE
  }


  if (!is_file_openable(data_path)) {
    stop("Data file is locked or cannot be opened")
  }
  if (!is_file_openable(dict_path)) {
    stop("Data dictionary file is locked or cannot be opened")
  }


  # same for est_chars_path
  if (!is.null(est_chars_path)) {
    if (!file.exists(est_chars_path)) {
      stop("File not found: ", est_chars_path)
    }

    if(!is_file_openable(est_chars_path)){
      stop("Establishment characteristics file is locked or cannot be opened")
    }
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
    verbose = verbose
  )

  # (optionally) suppress RStudio Viewer during render
  if (!isTRUE(show)) {
    old_viewer <- getOption("viewer")
    on.exit(options(viewer = old_viewer), add = TRUE)
    options(viewer = function(url) invisible())
  }

  # managing working directory for the .Rmd rendering
  old <- setwd(dirname(rmd))
  on.exit(setwd(old), add = TRUE)

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
#' * `(n)` labels showing the number of responses per bar.
#'
#' @author Dave Sirl
#'
#' @note Future/possible extensions include making the `(n)` labels optional, using a
#'   secondary axis for `(n)` labels, improving percentage‑label contrast for
#'   light fill colours, and a 1‑column option for facet layout.
#'
#' @param dat A data frame.
#' @param response_var Bare column name giving the response categories (used for fill).
#' @param group_var Optional bare column name defining separate bars.
#' @param facet_var Optional bare column name used for faceting.
#'   All variables should be factors (characters may work but are not guaranteed).
#'
#' @param percCut Numeric scalar (0–100). Cutoff below which percentages are not
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
#' @param horiz Logical (default `FALSE`). If `TRUE`, flip coordinates so bars
#'   are horizontal and place the legend below the plot.
#'
#' @param text_scale Positive number (default `1`) scaling the size of
#'   percentage and `(n)` labels.
#'
#' @param fillLabText Optional legend title for the fill variable. If `NULL`
#'   (default) the title is removed; if `""` the name of `response_var` is used.
#'
#' @param groupLabText Optional label for the bar‑group axis. If `NULL` the label is
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
#' @param facet_layout Optional character. If `"1row"` the facets are arranged
#'   in a single row; otherwise the default facet layout is used.
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
#' @param group_label_width Optional integer. Width (in characters) used when
#'   wrapping group labels with `stringr::str_wrap()`. Default is `NULL`, to not wrap.
#'
#' @param group_labels Optional named character vector providing alternative
#'   labels for the grouping variable. Should be of the form
#'   `c(level1 = "Label 1", level2 = "Label 2")`. Default is `NULL`, which
#'   uses the factor’s existing levels.
#'
#' @param ... Additional arguments passed to the underlying engine.
#'
#' @returns A `ggplot` object.
#'
#' @details
#' `OME_stacked_bar()` uses tidy‑evaluation;
#' `OME_stacked_bar_()` uses standard evaluation and is safe for loops and
#' programmatic workflows.
#' @examples
#' dat <- tibble::tibble(
#'   Response = factor(c("Yes", "No", "No", NA, "Yes", "Maybe")),
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


OME_stacked_bar_ = function(dat, response_var,
                           group_var=NULL, facet_var=NULL,
                           percCut=NULL, colo=NULL, na.rm=FALSE,
                           show_counts=TRUE,
                           horiz=FALSE, text_scale=1,
                           fillLabText=NULL, groupLabText=NULL,
                           propLabText="Proportion of responses",
                           titleText=NULL,
                           facet_labels=NULL, facet_layout=NULL,
                           separate_at = NULL,
                           fill_label_width=20,
                           group_label_width=NULL,
                           group_labels=NULL){

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
          forcats::fct_expand("Missing") |>
          forcats::fct_na_value_to_level(level = "Missing") |>
          forcats::fct_relevel("Missing", after = 0)
      )
    if (is.null(colo)){
      colo <- (dat2 |> dplyr::pull(var_name) |> nlevels() - 1) |> get_OME_colours(type='distinct')
    }
    colo = c("grey",colo)
  }

  # print("here!")
  # print(str(dat2))


  # --- Build plot -------------------------------------------------------------

  thePlot <-
    #ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, by=var_subdivide, fill=var_name)) +
    #ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, group = interaction(var_subdivide, var_name), fill=var_name)) +
    ggplot2::ggplot(dat2, ggplot2::aes(x=var_subdivide, fill=var_name)) +
    ggplot2::geom_bar(position = "fill") +
    OMESurvey::ROME_ggtheme(base_size = 12) #+
    #ggplot2::theme(
      #axis.ticks = ggplot2::element_blank(),
      #panel.border = ggplot2::element_blank(),
      #plot.title.position = "plot")

  # Further theme()ing
  if (horiz) {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="bottom"#,
        #panel.grid.major.x = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        #panel.grid.major.y = ggplot2::element_blank(),
        #panel.grid.minor = ggplot2::element_blank()
      )
    fill_guide <- ggplot2::guide_legend(direction="horizontal", nrow=1, reverse=TRUE)
  } else {
    thePlot <- thePlot +
      ggplot2::theme(
        legend.position="right"#,
        #panel.grid.major.x = ggplot2::element_blank(),
        #panel.grid.major.y = ggplot2::element_line(linewidth=0.2, colour="grey30"),
        #panel.grid.minor = ggplot2::element_blank()
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
  if (is.null(group_labels)) {
    # No custom labels supplied
    if (is.null(group_label_width)) {
      left_labeller <- ggplot2::waiver()   # use default labels
    } else {
      left_labeller <- function(x) {
        stringr::str_wrap(x, width = group_label_width)
      }
    }
  } else {
    # Custom labels supplied (named vector)
    if (is.null(group_label_width)) {
      left_labeller <- group_labels
    } else {
      left_labeller <- stringr::str_wrap(group_labels, width = group_label_width) |>
        setNames(names(group_labels))
    }
  }

  # Optional override
  if (!is.null(group_labels) && !all(names(group_labels) %in% group_levels)) {
    warning("Names of `group_labels` do not match the levels of the grouping variable.")
  }

  if (exists("omitGroupLabels") && omitGroupLabels) {
    left_labeller <- NULL
  }






  if (exists("show_counts") && show_counts) {
    group_counts <- dat2 |>
      dplyr::summarise(
        non_na = sum(!is.na(.data[[response_name]])),
        total  = dplyr::n(),
        .by = var_subdivide
      ) |>
      dplyr::mutate(label = sprintf("(%d/%d)", non_na, total))

    idx <- match(group_levels, group_counts$var_subdivide)
    right_labels <- stats::setNames(group_counts$label[idx], group_levels)
  }

  if (horiz) {

    # Horizontal: group labels on y-axis
    if (exists("show_counts") && show_counts) {
      thePlot <- thePlot +
        ggplot2::scale_x_discrete(
          labels = left_labeller,
          sec.axis = ggplot2::dup_axis(
            labels = right_labels,
            breaks = group_levels,
            name = NULL
          )
        )
    } else {
      thePlot <- thePlot +
        ggplot2::scale_x_discrete(labels = left_labeller)
    }

  } else {

    # Vertical: group labels on x-axis
    if (exists("show_counts") && show_counts) {
      thePlot <- thePlot +
        ggplot2::scale_x_discrete(
          labels = left_labeller,
          sec.axis = ggplot2::dup_axis(
            labels = right_labels,
            breaks = group_levels,
            name = NULL
          )
        )
    } else {
      thePlot <- thePlot +
        ggplot2::scale_x_discrete(labels = left_labeller)
    }
  }




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







#' Make a stacked bar chart summarising many survey questions
#'
#' Make a horizontal stacked bar chart to summarise several survey questions,
#' with questions ordered according to the proportion of responses that fit a
#' particular pattern.
#'
#' @param dat a tibble/data.frame, each variable corresponding to a question. All variables should be factors and all with the same possible values.
#' @param labels_vec a named vector of labels to use for the questions on the plot. Names are variable names and values are corresponding labels.
#' @param percCut numeric scalar (0-100). Cutoff below which percentages are not shown in bar segments. Default `5`.
#' @param colo (optional but recommended) vector of colours to use for the fill scale (character vector of colours,
#' length the same as the number of levels of the factor that is the first variable of `dat`.)
#' If `NULL` (default) the pallete from `OMESurvey::get_OME_colours(type='distinct')` is used.
#' When `na.rm = FALSE` a grey colour is prepended for the "No response" level.
#' Note that percentage labels are in white, so the pallete needs to work with that.
#'  (... or this function needs upgrading!)
#' @param order_values Optional. Controls how questions are ordered in the plot.
#'   May be a character vector of response levels, or the special value
#'   `"mean(as.numeric())"`. See Details.
#' @param titleText Optional text to use as plot title.
#' @param fill_label_width Optional integer. Width (in characters) used when wrapping
#'   fill labels with `stringr::str_wrap()`. Passed to `OME_stacked_bar()`. Default is 20.
#' @param question_label_width Optional integer. Width (in characters) used when wrapping
#'   axis labels with `stringr::str_wrap()`. Default is 30.
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
#' by `question_label_width`. Legend/fill labels are treated the same using `fill_label_width`.
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
#' labels <- c(Q1 = "Question 1", Q2 = "Question 2", Q3 = "Question 3")
#'
#' plot_many_questions(dat, labels_vec = labels)
#'
plot_many_questions <- function(dat, labels_vec=NULL, percCut=5,
                                colo=NULL, order_values = NULL,
                                titleText=NULL,
                                fill_label_width=20,
                                question_label_width=30){

  # In the context of using this for the survey_summary_report(.Rmd) this should be unnecessary, but maybe leave it for other use?
  colo <- convert_colo(colo)

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
                    titleText = titleText,
                    fill_label_width = fill_label_width,
                    group_label_width = question_label_width,
                    group_labels = labels_vec)
  return(thePlot)
}



#' Boxplot (horizontal), in an OME style.
#'
#' `OME_boxplot()` builds a boxplot from survey-style data using **bare column names**.
#' `OME_boxplot_()` is the programmatic interface and underlying engine,
#' designed for use when column names are supplied as **strings** or **symbols**.
#'
#' Features include:
#' * optional subdivision into separate boxes,
#' * optional `(n/m)` style labels showing the number of non-NA responses per box,
#' * optional dashed line to visually separate some of the boxes.

#' @author Dave Sirl
#'
#' @note Future/possible extensions include support for dodging/colour
#' (and maybe vertical-ness and/or faceting).
#'
#' @param data A data frame.
#' @param value_var Numeric variable to plot.
#' @param group_var Optional grouping variable (factor).
#' @param show_counts Logical; whether to display (valid/total) counts on the
#'   right-hand axis. Defaults to TRUE.
#' @param valueLabText Optional title for the value variable axis. If `NULL`
#'   (default) the title is removed; if `""` the name of `value_var` is used.
#' @param groupLabText Optional title for the group variable axis. If `NULL`
#'   (default) the title is removed; if `""` the name of `group_var` is used.
#' @param omitGroupLabels Optional logical controlling whether to omit group labels.
#'   Helpful when group_var=NULL. Default FALSE.

#' @param titleText Optional plot title.
#' @param colour Boxplot outline colour (any valid R colour). Defaults to [get_OME_colours](1).
#' @param separate_at Optional integer specifying where to draw a horizontal
#'   separation line between groups defined by `group_var`.
#'   * If positive n the separation line is after the first n categories
#'   * If negative n the separation line is before the n-th last category.
#'  Default `NULL` or 0 omits the line.
#' @param group_label_width Optional integer. Width (in characters) used when
#'   wrapping group labels with `stringr::str_wrap()`. Default is `NULL`, to not wrap.

#' @param ... Additional arguments passed to the underlying engine.
#'
#' @details
#' `OME_boxplot()` uses tidy‑evaluation;
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
#' # Without right‑hand counts
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


OME_boxplot_ <- function(data,
                         value_var,
                         group_var = NULL,
                         show_counts = TRUE,
                         valueLabText = NULL,
                         groupLabText = NULL,
                         omitGroupLabels = FALSE,
                         titleText = NULL,
                         colour = OMESurvey::get_OME_colours(1),
                         separate_at = NULL,
                         group_label_width = 20) {

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

  # Ensure factor levels follow order of appearance
  data[[group_var]] <- factor(
    data[[group_var]],
    levels = unique(data[[group_var]])
  )

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
      dplyr::mutate(label = sprintf("(%d/%d)", non_na, total))

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
      axis.ticks = ggplot2::element_blank()
    )



  # ggplot2::theme_bw() +
  # ggplot2::theme(
  #   panel.grid.major = ggplot2::element_line(colour = "grey90"),
  #   panel.grid.minor = ggplot2::element_line(colour = "grey95")
  # )

  # --- Label wrapping + optional counts ---------------------------------------
  if (omitGroupLabels) {
    y_labeller <- NULL
  } else {
    y_labeller <- function(x) stringr::str_wrap(x, width = group_label_width)
  }

  if (show_counts) {
    p <- p +
      ggplot2::scale_y_discrete(
        labels = y_labeller,
        sec.axis = ggplot2::dup_axis(
          labels = right_labels,
          breaks = group_levels,
          name = NULL
        )
      )
  } else {
    p <- p +
      ggplot2::scale_y_discrete(
        labels = y_labeller
      )
  }


  # --- Optional dashed reference line -----------------------------------------
  if (!is.null(separate_at) && separate_at != 0) {

    # first interpret separate_at
    if (separate_at > 0) {
      intercept <- separate_at + 0.5
    } else {
      intercept <- length(levels(data[[group_var]])) - abs(separate_at) + 0.5
    }

    length(levels(data[[group_var]])) |> print()
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
#'   * `value` — the result of evaluating `expr`
#'   * `suppressed` — a character vector of suppressed warning messages
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
