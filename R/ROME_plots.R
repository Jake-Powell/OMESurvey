#' #' Plot stacked Likert-style survey questions (ROME style)
#' #'
#' #' Builds a horizontal 100% stacked bar chart for one or more survey questions,
#' #' adding in-segment percent labels (hidden below a cutoff), a right-edge \code{N}
#' #' label for each question, and a bottom legend that is left-aligned and wraps
#' #' across rows when needed. Bar thickness and text size remain constant; the
#' #' function computes and attaches recommended figure dimensions (in inches) that
#' #' you can pass to \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}.
#' #'
#' #' @section Important:
#' #' The **top-to-bottom order of questions follows the order of `columns`** you
#' #' pass in. Internally the function fixes factor levels (with \code{coord_flip()})
#' #' so the visual order matches the supplied \code{columns}.
#' #'
#' #' @param data A data frame where each selected column is a survey question and
#' #'   values are categorical responses (character or factor). \code{NA}s are treated
#' #'   as non-responses and excluded from the per-question \code{N}.
#' #' @param columns Character or integer vector selecting the question columns;
#' #'   **order matters** (bars appear top→bottom in this order). Default \code{NA}
#' #'   uses all columns in \code{data}.
#' #' @param ordering Optional character vector giving the desired response order
#' #'   from most-positive to most-negative. Controls bar stacking and legend order
#' #'   (legend display is reversed so the first level appears on the left of the bar).
#' #' @param percentage_cutoff Numeric in \code{[0, 100]}; in-bar percentage labels
#' #'   smaller than this are suppressed (0-dp, with a \code{\%} sign).
#' #' @param label_size Numeric text size (ggplot2 size units) for both the in-bar
#' #'   percentage labels and the right-edge \code{N} labels.
#' #' @param n_label_pad_text Numeric passed to \code{hjust} for the \code{N} labels;
#' #'   negative values place the label slightly outside the 100\% mark; positive
#' #'   values pull it inside the bar.
#' #' @param per_question_in,base_height_in,legend_allowance_in,width_in Sizing
#' #'   controls (in inches) used to compute recommended figure dimensions stored
#' #'   in the returned object (see “Value”).
#' #' @param y_label_wrap_width Integer; approximate wrap width (characters) for
#' #'   question text on the y axis.
#' #' @param legend_wrap_chars Rough character budget per legend row; used to choose
#' #'   \code{guide_legend(nrow = ...)} so long legends wrap cleanly.
#' #' @param right_margin_min_pt Minimum right plot margin (points) to reserve space
#' #'   for the right-edge \code{N} labels.
#' #'
#' #' @return A \code{ggplot} object. In addition to the usual plot methods, the
#' #'   object carries these attributes for saving at a consistent size:
#' #'   \itemize{
#' #'     \item \code{attr(p, "width_in")} — recommended width in inches;
#' #'     \item \code{attr(p, "height_in")} — recommended height in inches;
#' #'     \item \code{attr(p, "rome_dims")} — list with \code{width}, \code{height},
#' #'           and \code{units = "in"}.
#' #'   }
#' #'
#' #' @details
#' #' The percent axis spans 0–100 with major gridlines every 5\% and labels only at
#' #' 0/25/50/75/100. Axis ticks and axis lines are suppressed. The legend is placed
#' #' at the bottom, left-aligned, and can wrap into multiple rows according to
#' #' \code{legend_wrap_chars}. Per-question \code{N} (excluding \code{NA}) is printed
#' #' at the right edge of each bar. Colours are drawn from \code{get_OME_colours()}
#' #' for the number of observed response levels.
#' #'
#' #' Note: this function uses theme settings such as \code{legend.location = "plot"}
#' #' (honored by \pkg{ggplot2} \code{>= 3.5.0}); on earlier versions that setting is
#' #' ignored, but the plot will still render.
#' #'
#' #' @seealso \code{\link{ROME_ggtheme}}, \code{\link{get_OME_colours}},
#' #'   \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}},
#' #'   \code{\link[cowplot:plot_grid]{cowplot::plot_grid}},
#' #'   \code{\link[patchwork:plot_layout]{patchwork::plot_layout}}
#' #'
#' #' @examples
#' #' # Minimal toy example (creates a small survey-like data frame)
#' #' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' #'   set.seed(1)
#' #'   lvls <- c("Agree a lot","Agree a little","Neither",
#' #'             "Disagree a little","Disagree a lot")
#' #'   make_col <- function(n = 400) {
#' #'     sample(c(lvls, NA), n, replace = TRUE,
#' #'            prob = c(0.34, 0.29, 0.20, 0.12, 0.05, 0.00))
#' #'   }
#' #'   df <- data.frame(
#' #'     `Is Jake lovely?`  = make_col(),
#' #'     `Is Jake wonderful?`  = make_col(),
#' #'     `Is Jake just the best?`  = make_col(),
#' #'     `Is Jake the person who most inspires you?` = make_col()
#' #'   )
#' #'
#' #'   # The order of 'columns' controls the top->bottom order in the chart:
#' #'   cols <- names(df)  # e.g., c("Is Jake lovely?", "Is Jake wonderful?", ...)
#' #'
#' #'   p <- plot_ROME_survey_questions(
#' #'     df,
#' #'     columns = cols,
#' #'     ordering = lvls,
#' #'     percentage_cutoff = 10,
#' #'     y_label_wrap_width = 40
#' #'   )
#' #'   print(p)
#' #'
#' #'   # Save with the recommended size:
#' #'   ggplot2::ggsave(
#' #'     "rome_demo.png", p,
#' #'     width  = attr(p, "width_in"),
#' #'     height = attr(p, "height_in"),
#' #'     units  = "in", dpi = 300
#' #'   )
#' #' }
#' #'
#' #' @export
#'
#' plot_ROME_survey_questions <- function(data,
#'                                        columns = NA,
#'                                        ordering = NA,
#'                                        percentage_cutoff = 10,       # % points (0-100)
#'                                        label_size = 4.5,             # % labels + N labels
#'                                        n_label_pad_text = -0.1,      # negative nudges N right of 100%
#'                                        # --- sizing controls (inches) ---
#'                                        per_question_in = 0.5,        # height per question row
#'                                        base_height_in = 0.8,         # top/bottom allowance
#'                                        legend_allowance_in = 0.6,    # extra space for bottom legend
#'                                        width_in = 8,                  # suggested width
#'                                        y_label_wrap_width = 40,
#'                                        # --- legend controls ---
#'                                        legend_wrap_chars = 80,        # ~chars per row before wrapping
#'                                        right_margin_min_pt = 24       # minimum right margin for N labels
#' ) {
#'   data <- data[, columns]
#'
#'   # 1) Extract question names
#'   questions <- names(data)
#'   n_questions <- length(questions)
#'
#'   # 2) Format data
#'   freq_tables <- lapply(seq_len(ncol(data)), function(index) {
#'     d <- data[, index] |> table() |> data.frame()
#'     d <- data.frame(a = rep(questions[index], nrow(d)), d)
#'     d$percent <- d$Freq / sum(d$Freq) * 100
#'     names(d) <- c(letters[1:4])
#'     d
#'   })
#'   data_format <- do.call(rbind, freq_tables)
#'   names(data_format) <- c("question", "answer", "count", "percent")
#'
#'   # --- ORDERING FIX: lock top-to-bottom y-axis order to the 'columns' order ---
#'   # coord_flip() draws factor levels from bottom->top, so use rev(questions)
#'   # to make the visual order top->bottom match the given 'columns' order.
#'   data_format$question <- factor(data_format$question, levels = rev(questions))
#'
#'   if (!is.na(ordering[1])) {
#'     data_format$answer <- factor(as.character(data_format$answer),
#'                                  levels = rev(ordering))
#'   }
#'
#'   # In-bar labels (only if >= cutoff)
#'   data_format$label <- ifelse(
#'     data_format$percent >= percentage_cutoff,
#'     paste0(round(data_format$percent, 0), "%"),
#'     ""
#'   )
#'
#'   # N per question (exclude NA)
#'   N_per_question <- sapply(data, function(col) sum(!is.na(col)))
#'   n_df <- data.frame(
#'     question = names(N_per_question),
#'     N = as.integer(N_per_question),
#'     y = 100,  # anchor at 100 so axis doesn't extend
#'     n_label = paste0("(", scales::comma(N_per_question), ")"),
#'     stringsAsFactors = FALSE
#'   )
#'   # Keep N labels on the same discrete scale/order as bars
#'   n_df$question <- factor(n_df$question, levels = levels(data_format$question))
#'
#'   # Axis: labels only at 0/25/50/75/100, gridlines every 5%
#'   quartiles <- c(0, 25, 50, 75, 100)
#'   lbl_fun <- function(x) ifelse(x %in% quartiles, paste0(x, "%"), "")
#'
#'   # Palette length from actually-used levels
#'   current_levels <- if (is.factor(data_format$answer)) levels(data_format$answer) else unique(as.character(data_format$answer))
#'   colo <- get_OME_colours(n = length(current_levels), type = "contrast")
#'   xaxis_label <- ""
#'
#'   # --- Legend wrapping + left alignment ---
#'   # crude width proxy: total label chars; wrap into ~legend_wrap_chars per row
#'   legend_labels <- current_levels
#'   total_chars <- sum(nchar(legend_labels)) + 3 * length(legend_labels)  # +key/spacing allowance
#'   legend_rows <- max(1, ceiling(total_chars / legend_wrap_chars))
#'
#'   # --- Dynamic right margin (pts) based on longest "(12,345)" label ---
#'   max_n_chars <- max(nchar(n_df$n_label))
#'   # ~5.5pt per char as a simple heuristic + a little padding
#'   right_margin_pt <- max(right_margin_min_pt, ceiling(5.5 * max_n_chars) + 6)
#'
#'   p <- ggplot2::ggplot(
#'     data = data_format,
#'     ggplot2::aes(x = question, y = percent, fill = factor(answer))
#'   ) +
#'     ggplot2::geom_bar(stat = "identity") +
#'     ggplot2::geom_text(
#'       ggplot2::aes(label = label),
#'       position = ggplot2::position_stack(vjust = 0.5),
#'       color = "white",
#'       size = label_size
#'     ) +
#'     ROME_ggtheme(18) +
#'     ggplot2::scale_fill_manual(values = colo, drop = FALSE) +
#'     ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = y_label_wrap_width), drop = FALSE) +
#'     ggplot2::scale_y_continuous(
#'       breaks = seq(0, 100, by = 5),     # gridlines every 5%
#'       labels = lbl_fun,                  # labels only at quartiles
#'       limits = c(0, 100),
#'       expand = c(0, 0)
#'     ) +
#'     ggplot2::xlab("") +
#'     ggplot2::ylab(xaxis_label) +
#'     ggplot2::coord_flip(clip = "off") +
#'     ggplot2::theme(
#'       # Legend: left-justify + minimize wasted space
#'       legend.title        = ggplot2::element_blank(),
#'       legend.position     = "bottom",
#'       legend.direction    = "horizontal",
#'       legend.justification = 'center',
#'       legend.location = 'plot',
#'       legend.box.just     = "left",
#'       legend.margin       = ggplot2::margin(0, 0, 0, 0),
#'       legend.box.margin   = ggplot2::margin(0, 0, 0, 0),
#'       legend.box.spacing  = grid::unit(2, "pt"),
#'       legend.spacing.x    = grid::unit(6, "pt"),
#'       legend.spacing.y    = grid::unit(2, "pt"),
#'       # Axes: no ticks/lines; keep only gridlines
#'       axis.line.x         = ggplot2::element_blank(),
#'       axis.line.y         = ggplot2::element_blank(),
#'       axis.ticks.x        = ggplot2::element_blank(),
#'       axis.ticks.y        = ggplot2::element_blank(),
#'       axis.ticks.length   = grid::unit(0, "pt"),
#'       # Plot margins: slim left, dynamic right for N labels
#'       plot.margin         = ggplot2::margin(5.5, right_margin_pt, 5.5, 5.5, "pt")
#'     ) +
#'     ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = TRUE)) +
#'     ggplot2::geom_text(
#'       data = n_df,
#'       ggplot2::aes(x = question, y = y, label = n_label),
#'       inherit.aes = FALSE,
#'       size = label_size,
#'       hjust = n_label_pad_text
#'     )
#'
#'   #  Figure size recommendation (inches)
#'   height_in <- base_height_in + n_questions * per_question_in + legend_allowance_in
#'
#'   attr(p, "width_in")  <- width_in
#'   attr(p, "height_in") <- height_in
#'   attr(p, "rome_dims") <- list(width = width_in, height = height_in, units = "in")
#'
#'   return(p)
#' }
#'
#'
#'
#' #' Plot survey questions by demographic, faceted by question (ROME style)
#' #'
#' #' Creates a horizontal 100% stacked bar chart of multiple survey questions,
#' #' faceted (one panel per question). Within each question, one stacked bar is
#' #' drawn **for each level of a demographic variable** (e.g., Male, Female).
#' #' The plot includes in-segment percentage labels (0-dp, suppressed below a
#' #' cutoff), a right-edge \code{N} label per (question, demographic) bar, and a
#' #' bottom legend that wraps when needed. Bars and text retain constant size, and
#' #' the function attaches recommended figure dimensions suitable for
#' #' \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}.
#' #'
#' #' @section Important:
#' #' \itemize{
#' #' \item The **top-to-bottom order of question facets follows the order of `columns`**.
#' #' \item Within each facet, the **top-to-bottom order of demographic bars**
#' #'       follows \code{demographic_order} if supplied; otherwise the factor levels
#' #'       of the demographic column (or order of first appearance).
#' #' }
#' #'
#' #' @param data A data frame containing survey questions as columns and at least
#' #'   one demographic column.
#' #' @param columns Character or integer vector selecting which question columns to
#' #'   plot. **Order matters**: facets appear top→bottom in this order.
#' #' @param demographic Character scalar naming the demographic column in \code{data}
#' #'   (e.g., \code{"Gender"}). Rows with \code{NA} in either the question or the
#' #'   demographic are excluded for that bar.
#' #' @param ordering Optional character vector giving the desired **response**
#' #'   category order (e.g., most-positive → most-negative). Controls the stack
#' #'   order and legend order (legend display is reversed to match stacks).
#' #' @param demographic_order Optional character vector giving the desired
#' #'   order of demographic levels (top→bottom inside each facet).
#' #' @param percentage_cutoff Numeric in \code{[0, 100]}. In-bar % labels smaller
#' #'   than this value are hidden.
#' #' @param label_size Numeric text size (ggplot2 units) for both in-bar % labels
#' #'   and right-edge \code{N} labels.
#' #' @param n_label_pad_text Numeric passed to \code{hjust} for the \code{N} labels;
#' #'   negative values place the label slightly outside the 100\% line; positive
#' #'   values pull it inside the bar.
#' #' @param per_question_in,base_height_in,legend_allowance_in,width_in Sizing
#' #'   controls (in inches). Height scales with the total number of demographic
#' #'   bars so bar thickness and text size remain visually constant.
#' #' @param y_label_wrap_width Integer; approximate wrap width (characters) for
#' #'   demographic labels (become y-axis labels after flipping).
#' #' @param legend_wrap_chars Rough character budget per legend row; used to pick
#' #'   \code{guide_legend(nrow = ...)} so long legends wrap cleanly.
#' #' @param right_margin_min_pt Minimum right plot margin (points) reserved so
#' #'   right-edge \code{N} labels are visible without extending the 0–100\% axis.
#' #'
#' #' @return A \code{ggplot} object. The object also carries size attributes you
#' #'   can pass directly to \code{ggplot2::ggsave()}:
#' #'   \itemize{
#' #'     \item \code{attr(p, "width_in")} — recommended width (inches)
#' #'     \item \code{attr(p, "height_in")} — recommended height (inches)
#' #'     \item \code{attr(p, "rome_dims")} — list with \code{width}, \code{height},
#' #'           and \code{units = "in"}
#' #'   }
#' #'
#' #' @details
#' #' \itemize{
#' #' \item Axis formatting: percent axis fixed to 0–100 with **major gridlines every
#' #'       5\%** and **labels only at 0/25/50/75/100**; axis ticks and lines removed.
#' #' \item In-bar percentage labels are rounded to 0-dp with a \code{\%} suffix and
#' #'       shown only when \code{percent >= percentage_cutoff}.
#' #' \item Per-(question, demographic) \code{N} excludes \code{NA} responses and is
#' #'       printed at the right edge of each bar as \code{"(12,345)"}.
#' #' \item Colours come from \code{get_OME_colours()} matching the number of
#' #'       observed response levels.
#' #' \item The legend is placed at the bottom, left-leaning, and wraps into multiple
#' #'       rows using a simple character-count heuristic.
#' #' }
#' #'
#' #' @note Uses modern \pkg{ggplot2} theme elements. The argument
#' #'   \code{legend.location = "plot"} is honored in \pkg{ggplot2} (≥ 3.5.0); on
#' #'   earlier versions it is ignored but the plot still renders.
#' #'
#' #' @seealso \code{\link{plot_ROME_survey_questions}},
#' #'   \code{\link{ROME_ggtheme}}, \code{\link{get_OME_colours}},
#' #'   \code{\link[ggplot2:ggsave]{ggplot2::ggsave}},
#' #'   \code{\link[patchwork:plot_layout]{patchwork::plot_layout}},
#' #'   \code{\link[cowplot:get_legend]{cowplot::get_legend}}
#' #'
#' #' @examples
#' #' # Minimal toy example with Jake questions and a Gender demographic
#' #' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' #'   set.seed(1)
#' #'   lvls <- c("Agree a lot","Agree a little","Neither",
#' #'             "Disagree a little","Disagree a lot")
#' #'   make_col <- function(n = 400) {
#' #'     sample(c(lvls, NA), n, replace = TRUE,
#' #'            prob = c(0.34, 0.29, 0.20, 0.12, 0.05, 0.00))
#' #'   }
#' #'
#' #'   df <- data.frame(
#' #'     `Is Jake lovely?`  = make_col(),
#' #'     `Is Jake wonderful?`  = make_col(),
#' #'     `Is Jake just the best?`  = make_col(),
#' #'     `Is Jake the person who most inspires you?` = make_col(),
#' #'     Gender = sample(c("Male","Female"), 400, replace = TRUE, prob = c(0.47, 0.53))
#' #'   )
#' #'
#' #'   # Facets (top->bottom) follow the order of 'columns'
#' #'   cols <- c("Is Jake lovely?",
#' #'             "Is Jake wonderful?",
#' #'             "Is Jake just the best?",
#' #'             "Is Jake the person who most inspires you?")
#' #'
#' #'   p <- plot_ROME_survey_by_demographic(
#' #'     data = df,
#' #'     columns = cols,
#' #'     demographic = "Gender",
#' #'     demographic_order = c("Male","Female"),
#' #'     ordering = lvls,
#' #'     percentage_cutoff = 10,
#' #'     y_label_wrap_width = 30
#' #'   )
#' #'   print(p)
#' #'
#' #'   # Save with the recommended size
#' #'   ggplot2::ggsave(
#' #'     "rome_questions_by_gender.png", p,
#' #'     width  = attr(p, "width_in"),
#' #'     height = attr(p, "height_in"),
#' #'     units  = "in", dpi = 300
#' #'   )
#' #' }
#' #'
#' #' @export
#' # Plot survey questions by demographic, faceted by question
#' # Plot survey questions by demographic, faceted by question
#' plot_ROME_survey_by_demographic <- function(data,
#'                                             columns = NA,
#'                                             demographic,                  # <- name of the demographic column in `data`
#'                                             ordering = NA,
#'                                             demographic_order = NA,       # optional explicit order for demographic levels
#'                                             percentage_cutoff = 10,       # % points (0-100)
#'                                             label_size = 4.5,             # % labels + N labels
#'                                             n_label_pad_text = -0.1,      # negative nudges N right of 100%
#'                                             # --- sizing controls (inches) ---
#'                                             per_question_in = 0.5,        # height per *bar* row (one per demographic)
#'                                             base_height_in = 0.8,         # top/bottom allowance
#'                                             legend_allowance_in = 0.6,    # extra space for bottom legend
#'                                             width_in = 8,                  # suggested width
#'                                             y_label_wrap_width = 40,
#'                                             # --- legend controls ---
#'                                             legend_wrap_chars = 80,        # ~chars per row before wrapping
#'                                             right_margin_min_pt = 24       # minimum right margin for N labels
#' ) {
#'   # --- sanity checks
#'   if (missing(demographic) || !(demographic %in% names(data))) {
#'     stop("`demographic` must be the name of a column in `data`.")
#'   }
#'
#'   # Use selected columns as questions
#'   data <- data[, c(columns, demographic), drop = FALSE]
#'
#'   # 1) Extract question names (in the order 'columns' provided)
#'   questions <- setdiff(names(data), demographic)
#'   n_questions <- length(questions)
#'
#'   # 2) Build long data: for each question, stack responses by demographic
#'   df_list <- vector("list", n_questions)
#'   n_list  <- vector("list", n_questions)
#'
#'   # establish demographic level order
#'   dem_col <- data[[demographic]]
#'   dem_levels <- if (!is.na(demographic_order[1])) {
#'     demographic_order
#'   } else if (is.factor(dem_col)) {
#'     levels(dem_col)
#'   } else {
#'     unique(as.character(dem_col[!is.na(dem_col)]))  # order of appearance
#'   }
#'
#'   for (i in seq_along(questions)) {
#'     q <- questions[i]
#'     ans <- data[[q]]
#'     dem <- data[[demographic]]
#'
#'     keep <- !is.na(ans) & !is.na(dem)
#'     if (!any(keep)) next
#'
#'     # counts by demographic x answer
#'     tab <- as.data.frame(table(dem = dem[keep], ans = ans[keep]), stringsAsFactors = FALSE)
#'     names(tab) <- c("demographic", "answer", "count")
#'
#'     # percent within each demographic
#'     totals_by_dem <- tapply(tab$count, tab$demographic, sum)
#'     tab$percent <- tab$count / totals_by_dem[tab$demographic] * 100
#'
#'     # attach question
#'     df_list[[i]] <- data.frame(
#'       question    = q,
#'       demographic = as.character(tab$demographic),
#'       answer      = as.character(tab$answer),
#'       count       = tab$count,
#'       percent     = tab$percent,
#'       stringsAsFactors = FALSE
#'     )
#'
#'     # N per (question, demographic) excluding NA answers
#'     n_counts <- tapply(ans[keep], dem[keep], function(v) sum(!is.na(v)))
#'     n_tmp <- data.frame(
#'       question    = q,
#'       demographic = names(n_counts),
#'       N           = as.integer(n_counts),
#'       y           = 100,
#'       n_label     = paste0("(", scales::comma(as.integer(n_counts)), ")"),
#'       stringsAsFactors = FALSE
#'     )
#'     n_list[[i]] <- n_tmp
#'   }
#'
#'   data_format <- do.call(rbind, df_list)
#'   n_df        <- do.call(rbind, n_list)
#'
#'   if (is.null(data_format) || nrow(data_format) == 0) {
#'     stop("No non-missing data to plot for the selected questions and demographic.")
#'   }
#'
#'   names(data_format) <- c("question", "demographic", "answer", "count", "percent")
#'
#'   # --- ORDERING: facets (rows) follow 'columns' order top->bottom
#'   data_format$question <- factor(data_format$question, levels = questions)
#'   n_df$question        <- factor(n_df$question,        levels = questions)
#'
#'   # --- DEMOGRAPHIC order: top->bottom inside each facet follows 'dem_levels'
#'   # coord_flip() draws x levels bottom->top, so use rev() to make visual top->bottom = dem_levels
#'   data_format$demographic <- factor(data_format$demographic, levels = rev(dem_levels))
#'   n_df$demographic        <- factor(n_df$demographic,        levels = levels(data_format$demographic))
#'
#'   # --- ANSWER order (stack and legend)
#'   if (!is.na(ordering[1])) {
#'     data_format$answer <- factor(as.character(data_format$answer), levels = rev(ordering))
#'   }
#'
#'   # In-bar labels (only if >= cutoff)
#'   data_format$label <- ifelse(
#'     data_format$percent >= percentage_cutoff,
#'     paste0(round(data_format$percent, 0), "%"),
#'     ""
#'   )
#'
#'   # Axis: labels only at 0/25/50/75/100, gridlines every 5%
#'   quartiles <- c(0, 25, 50, 75, 100)
#'   lbl_fun <- function(x) ifelse(x %in% quartiles, paste0(x, "%"), "")
#'
#'   # Palette length from actually-used levels
#'   current_levels <- if (is.factor(data_format$answer)) levels(data_format$answer) else unique(as.character(data_format$answer))
#'   colo <- get_OME_colours(n = length(current_levels), type = "contrast")
#'   xaxis_label <- ""
#'
#'   # --- Legend wrapping + left alignment (heuristic rows)
#'   legend_labels <- current_levels
#'   total_chars <- sum(nchar(legend_labels)) + 3 * length(legend_labels)
#'   legend_rows <- max(1, ceiling(total_chars / legend_wrap_chars))
#'
#'   # --- Dynamic right margin (pts) based on longest "(12,345)" label ---
#'   max_n_chars <- max(nchar(n_df$n_label))
#'   right_margin_pt <- max(right_margin_min_pt, ceiling(5.5 * max_n_chars) + 6)
#'
#'   p <- ggplot2::ggplot(
#'     data = data_format,
#'     ggplot2::aes(x = demographic, y = percent, fill = factor(answer))
#'   ) +
#'     ggplot2::geom_bar(stat = "identity") +
#'     # white in-bar % labels
#'     ggplot2::geom_text(
#'       ggplot2::aes(label = label),
#'       position = ggplot2::position_stack(vjust = 0.5),
#'       color = "white",
#'       size = label_size
#'     ) +
#'     ROME_ggtheme(18) +
#'     ggplot2::scale_fill_manual(values = colo, drop = FALSE) +
#'     # wrap demographic labels (become y labels after flip)
#'     ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = y_label_wrap_width), drop = FALSE) +
#'     ggplot2::scale_y_continuous(
#'       breaks = seq(0, 100, by = 5),     # gridlines every 5%
#'       labels = lbl_fun,                  # labels only at quartiles
#'       limits = c(0, 100),
#'       expand = c(0, 0)
#'     ) +
#'     ggplot2::xlab("") +
#'     ggplot2::ylab(xaxis_label) +
#'     ggplot2::coord_flip(clip = "off") +
#'     # --- FACET TITLES: one panel per question, title above bars, left-aligned
#'     ggplot2::facet_wrap(~ question, ncol = 1, scales = "free_y", strip.position = "top") +
#'     ggplot2::theme(
#'       # Legend: left-leaning and wrapped
#'       legend.title        = ggplot2::element_blank(),
#'       legend.position     = "bottom",
#'       legend.direction    = "horizontal",
#'       legend.justification = 'center',
#'       legend.location     = 'plot',
#'       legend.box.just     = "left",
#'       legend.margin       = ggplot2::margin(0, 0, 0, 0),
#'       legend.box.margin   = ggplot2::margin(0, 0, 0, 0),
#'       legend.box.spacing  = grid::unit(2, "pt"),
#'       legend.spacing.x    = grid::unit(6, "pt"),
#'       legend.spacing.y    = grid::unit(2, "pt"),
#'       # Axes: no ticks/lines; keep only gridlines
#'       axis.line.x         = ggplot2::element_blank(),
#'       axis.line.y         = ggplot2::element_blank(),
#'       axis.ticks.x        = ggplot2::element_blank(),
#'       axis.ticks.y        = ggplot2::element_blank(),
#'       axis.ticks.length   = grid::unit(0, "pt"),
#'       # Facet spacing and margins
#'       panel.spacing       = grid::unit(6, "pt"),
#'       plot.margin         = ggplot2::margin(5.5, right_margin_pt, 5.5, 5.5, "pt"),
#'       # Left-align facet strip titles (keep your strip styling from ROME_ggtheme)
#'       strip.background   = ggplot2::element_blank(),     # no box
#'       strip.background.x = ggplot2::element_blank(),
#'       strip.text.x     = ggplot2::element_text(
#'         hjust  = 0,
#'         colour = ggplot2::theme_get()$axis.text$colour
#'       ),
#'       strip.placement    = "outside",                    # place strips outside panels (needs ggplot2 >= 3.5)
#'       strip.clip         = "off"                         # let long titles breathe
#'     ) +
#'     ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = TRUE)) +
#'     # N labels at the right edge of each demographic bar
#'     ggplot2::geom_text(
#'       data = n_df,
#'       ggplot2::aes(x = demographic, y = y, label = n_label),
#'       inherit.aes = FALSE,
#'       size = label_size,
#'       hjust = n_label_pad_text
#'     )
#'
#'   # --- Figure size recommendation (inches):
#'   # with facet_wrap (ncol=1), panel heights are equal; use the max bars per panel
#'   bars_per_question <- sapply(questions, function(q)
#'     length(unique(n_df$demographic[n_df$question == q])))
#'   max_bars <- max(bars_per_question)
#'   height_in <- base_height_in + n_questions * (max_bars * per_question_in) + legend_allowance_in
#'
#'   attr(p, "width_in")  <- width_in
#'   attr(p, "height_in") <- height_in
#'   attr(p, "rome_dims") <- list(width = width_in, height = height_in, units = "in")
#'
#'   return(p)
#' }
#'
#'
