#' Internal core for ROME survey plotting
#'
#' Shared implementation used by \code{plot_ROME_survey_questions()} and
#' \code{plot_ROME_survey_by_demographic()} to keep styling and behavior in one place.
#'
#' @keywords internal
#' @noRd
.plot_ROME_survey_core <- function(data,
                                   columns = NA,
                                   demographic = NULL,          # NULL => no facet; string => facet by question
                                   ordering = NA,
                                   percentage_cutoff = 10,       # % points (0-100)
                                   label_size = 4.5,             # % labels + N labels
                                   n_label_pad_text = -0.1,      # negative nudges N right of 100%
                                   # --- sizing controls (inches) ---
                                   per_question_in = 0.5,        # height per *bar* row (one per demographic or 1 if none)
                                   base_height_in = 0.8,         # top/bottom allowance
                                   legend_allowance_in = 0.6,    # extra space for bottom legend
                                   width_in = 8,                  # suggested width
                                   y_label_wrap_width = 40,
                                   # --- legend controls ---
                                   legend_wrap_chars = 80,        # ~chars per row before wrapping
                                   right_margin_min_pt = 24       # minimum right margin for N labels
) {
  # --- columns defaulting
  if (is.na(columns[1])) {
    if (!is.null(demographic)) {
      columns <- setdiff(names(data), demographic)
    } else {
      columns <- names(data)
    }
  }

  # keep only requested columns (+ demo if present)
  keep_cols <- unique(c(columns, demographic))
  data <- data[, keep_cols, drop = FALSE]

  # 1) Extract question names (order = columns)
  questions   <- setdiff(names(data), demographic %||% character(0))
  n_questions <- length(questions)

  # 2) Build long data
  df_list <- vector("list", n_questions)
  n_list  <- vector("list", n_questions)

  # demographic levels (if any)
  if (!is.null(demographic)) {
    dem_col    <- data[[demographic]]
    dem_levels <- if (is.factor(dem_col)) levels(dem_col) else unique(as.character(dem_col[!is.na(dem_col)]))
  }

  for (i in seq_along(questions)) {
    q   <- questions[i]
    ans <- data[[q]]

    if (is.null(demographic)) {
      keep <- !is.na(ans)
      if (!any(keep)) next

      # counts by answer
      tab <- as.data.frame(table(ans = ans[keep]), stringsAsFactors = FALSE)
      names(tab) <- c("answer", "count")
      tab$percent <- tab$count / sum(tab$count) * 100

      df_list[[i]] <- data.frame(
        question = q,
        x        = q,                     # single bar per question (placeholder)
        answer   = as.character(tab$answer),
        count    = tab$count,
        percent  = tab$percent,
        stringsAsFactors = FALSE
      )

      n_list[[i]] <- data.frame(
        question = q,
        x        = q,
        N        = sum(!is.na(ans)),
        y        = 100,
        n_label  = paste0("(", scales::comma(sum(!is.na(ans))), ")"),
        stringsAsFactors = FALSE
      )

    } else {
      dem <- data[[demographic]]
      keep <- !is.na(ans) & !is.na(dem)
      if (!any(keep)) next

      # counts by demographic x answer
      tab <- as.data.frame(table(dem = dem[keep], ans = ans[keep]), stringsAsFactors = FALSE)
      names(tab) <- c("demographic", "answer", "count")
      totals_by_dem <- tapply(tab$count, tab$demographic, sum)
      tab$percent   <- tab$count / totals_by_dem[tab$demographic] * 100

      df_list[[i]] <- data.frame(
        question    = q,
        x           = as.character(tab$demographic),   # one bar per demographic
        answer      = as.character(tab$answer),
        count       = tab$count,
        percent     = tab$percent,
        stringsAsFactors = FALSE
      )

      # N per (question, demographic)
      n_counts <- tapply(ans[keep], dem[keep], function(v) sum(!is.na(v)))
      n_list[[i]] <- data.frame(
        question    = q,
        x           = names(n_counts),
        N           = as.integer(n_counts),
        y           = 100,
        n_label     = paste0("(", scales::comma(as.integer(n_counts)), ")"),
        stringsAsFactors = FALSE
      )
    }
  }

  data_format <- do.call(rbind, df_list)
  n_df        <- do.call(rbind, n_list)
  if (is.null(data_format) || nrow(data_format) == 0) stop("No non-missing data to plot.")

  # 3) Factor ordering (questions top->bottom = 'columns' order)
  data_format$question <- factor(data_format$question, levels = questions)
  n_df$question        <- factor(n_df$question,        levels = questions)

  # x-scale order:
  if (is.null(demographic)) {
    # coord_flip draws bottom->top, so reverse to make top->bottom match columns
    data_format$x <- factor(data_format$x, levels = rev(questions))
    n_df$x        <- factor(n_df$x,        levels = levels(data_format$x))
  } else {
    # top->bottom inside each facet = dem_levels
    data_format$x <- factor(data_format$x, levels = rev(dem_levels))
    n_df$x        <- factor(n_df$x,        levels = levels(data_format$x))
  }

  # answer order (stack & legend)
  if (!is.na(ordering[1])) {
    data_format$answer <- factor(as.character(data_format$answer), levels = rev(ordering))
  }

  # In-bar labels (only if >= cutoff)
  data_format$label <- ifelse(
    data_format$percent >= percentage_cutoff,
    paste0(round(data_format$percent, 0), "%"),
    ""
  )

  # Axis labels (gridlines every 5%, labels at quartiles)
  quartiles <- c(0, 25, 50, 75, 100)
  lbl_fun   <- function(x) ifelse(x %in% quartiles, paste0(x, "%"), "")

  # Palette length from actually-used levels
  current_levels <- if (is.factor(data_format$answer)) levels(data_format$answer) else unique(as.character(data_format$answer))
  colo <- get_OME_colours(n = length(current_levels), type = "contrast")
  xaxis_label <- ""

  # Legend wrapping rows (heuristic)
  total_chars  <- sum(nchar(current_levels)) + 3 * length(current_levels)
  legend_rows  <- max(1, ceiling(total_chars / legend_wrap_chars))

  # Dynamic right margin for N labels
  max_n_chars     <- max(nchar(n_df$n_label))
  right_margin_pt <- max(right_margin_min_pt, ceiling(5.5 * max_n_chars) + 6)

  # --- BUILD PLOT (shared) ---
  p <- ggplot2::ggplot(
    data_format,
    ggplot2::aes(x = x, y = percent, fill = factor(answer))
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size  = label_size
    ) +
    ROME_ggtheme(18) +
    ggplot2::scale_fill_manual(values = colo, drop = FALSE) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = y_label_wrap_width), drop = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, by = 5),
      labels = lbl_fun,
      limits = c(0, 100.5),
      expand = c(0, 0)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(xaxis_label) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme(
      legend.title        = ggplot2::element_blank(),
      legend.position     = "bottom",
      legend.direction    = "horizontal",
      legend.justification= "center",
      legend.location     = "plot",
      legend.box.just     = "left",
      legend.margin       = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin   = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing  = grid::unit(2, "pt"),
      legend.spacing.x    = grid::unit(6, "pt"),
      legend.spacing.y    = grid::unit(2, "pt"),
      axis.line.x         = ggplot2::element_blank(),
      axis.line.y         = ggplot2::element_blank(),
      axis.ticks.x        = ggplot2::element_blank(),
      axis.ticks.y        = ggplot2::element_blank(),
      axis.ticks.length   = grid::unit(0, "pt"),
      plot.margin         = ggplot2::margin(5.5, right_margin_pt, 5.5, 5.5, "pt")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = TRUE)) +
    ggplot2::geom_text(
      data = n_df,
      ggplot2::aes(x = x, y = y, label = n_label),
      inherit.aes = FALSE,
      size = label_size,
      hjust = n_label_pad_text
    )

  # Facet decoration (only when demographic supplied)
  if (!is.null(demographic)) {
    p <- p +
      ggplot2::facet_wrap(~ question, ncol = 1, scales = "free_y", strip.position = "top") +
      ggplot2::theme(
        panel.spacing    = grid::unit(6, "pt"),
        strip.background = ggplot2::element_blank(),
        strip.placement  = "outside",
        strip.clip       = "off",
        strip.text.x     = ggplot2::element_text(hjust = 0, colour = ggplot2::theme_get()$axis.text$colour)
      )
  }

  # --- SIZE recommendation (inches) ---
  if (is.null(demographic)) {
    bars_per_panel <- 1
    height_in <- base_height_in + n_questions * (bars_per_panel * per_question_in) + legend_allowance_in
  } else {
    # equal-height facet panels -> scale by max #demographic rows in any facet
    bars_per_question <- sapply(questions, function(q)
      length(unique(n_df$x[n_df$question == q])))
    max_bars <- max(bars_per_question)
    height_in <- base_height_in + n_questions * (max_bars * per_question_in) + legend_allowance_in
  }

  attr(p, "width_in")  <- width_in
  attr(p, "height_in") <- height_in
  attr(p, "rome_dims") <- list(width = width_in, height = height_in, units = "in")
  return(p)
}

#' Plot stacked Likert-style survey questions (ROME style)
#'
#' Builds a horizontal 100% stacked bar chart for one or more survey questions,
#' adding in-segment percent labels (hidden below a cutoff), a right-edge \code{N}
#' label for each question, and a bottom legend that is left-aligned and wraps
#' across rows when needed. Bar thickness and text size remain constant; the
#' function computes and attaches recommended figure dimensions (in inches) that
#' you can pass to \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}.
#'
#' @section Important:
#' The **top-to-bottom order of questions follows the order of `columns`** you
#' pass in. Internally the function fixes factor levels (with \code{coord_flip()})
#' so the visual order matches the supplied \code{columns}.
#'
#' @name plot_ROME_survey
#' @aliases plot_ROME_survey plot_ROME_survey_questions plot_ROME_survey_by_demographic
#'
#' @param data A data frame where each selected column is a survey question and
#'   values are categorical responses (character or factor). \code{NA}s are treated
#'   as non-responses and excluded from the per-question \code{N}.
#' @param columns Character or integer vector selecting the question columns;
#'   **order matters** (bars/facets appear top→bottom in this order). Default
#'   \code{NA} uses all columns in \code{data}.
#' @param ordering Optional character vector giving the desired response order
#'   from most-positive to most-negative. Controls bar stacking and legend order
#'   (legend display is reversed so the first level appears on the left of the bar).
#' @param percentage_cutoff Numeric in \code{[0, 100]}; in-bar percentage labels
#'   smaller than this are suppressed (0-dp, with a \code{\%} sign).
#' @param label_size Numeric text size (ggplot2 size units) for both the in-bar
#'   percentage labels and the right-edge \code{N} labels.
#' @param n_label_pad_text Numeric passed to \code{hjust} for the \code{N} labels;
#'   negative values place the label slightly outside the 100\% mark; positive
#'   values pull it inside the bar.
#' @param per_question_in,base_height_in,legend_allowance_in,width_in Sizing
#'   controls (in inches) used to compute recommended figure dimensions stored
#'   in the returned object (see “Value”).
#' @param y_label_wrap_width Integer; approximate wrap width (characters) for
#'   question (or demographic) labels after flipping.
#' @param legend_wrap_chars Rough character budget per legend row; used to choose
#'   \code{guide_legend(nrow = ...)} so long legends wrap cleanly.
#' @param right_margin_min_pt Minimum right plot margin (points) to reserve space
#'   for the right-edge \code{N} labels.
#'
#' @return A \code{ggplot} object. In addition to the usual plot methods, the
#'   object carries these attributes for saving at a consistent size:
#'   \itemize{
#'     \item \code{attr(p, "width_in")} — recommended width in inches;
#'     \item \code{attr(p, "height_in")} — recommended height in inches;
#'     \item \code{attr(p, "rome_dims")} — list with \code{width}, \code{height},
#'           and \code{units = "in"}.
#'   }
#'
#' @details
#' The percent axis spans 0–100 with major gridlines every 5\% and labels only at
#' 0/25/50/75/100. Axis ticks and axis lines are suppressed. The legend is placed
#' at the bottom, left-aligned, and can wrap into multiple rows according to
#' \code{legend_wrap_chars}. Per-question \code{N} (excluding \code{NA}) is printed
#' at the right edge of each bar. Colours are drawn from \code{get_OME_colours()}
#' for the number of observed response levels.
#'
#' Note: this function uses theme settings such as \code{legend.location = "plot"}
#' (honored by \pkg{ggplot2} \code{>= 3.5.0}); on earlier versions that setting is
#' ignored, but the plot will still render.
#'
#' @seealso \code{\link{ROME_ggtheme}}, \code{\link{get_OME_colours}},
#'   \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}},
#'   \code{\link[cowplot:plot_grid]{cowplot::plot_grid}},
#'   \code{\link[patchwork:plot_layout]{patchwork::plot_layout}}
#'
#' @examples
#' # Minimal toy example (creates a small survey-like data frame)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(1)
#'   lvls <- c("Agree a lot","Agree a little","Neither",
#'             "Disagree a little","Disagree a lot")
#'   make_col <- function(n = 400) {
#'     sample(c(lvls, NA), n, replace = TRUE,
#'            prob = c(0.34, 0.29, 0.20, 0.12, 0.05, 0.00))
#'   }
#'   df <- data.frame(
#'     `Is Jake lovely?`  = make_col(),
#'     `Is Jake wonderful?`  = make_col(),
#'     `Is Jake just the best?`  = make_col(),
#'     `Is Jake the person who most inspires you?` = make_col()
#'   )
#'
#'   # The order of 'columns' controls the top->bottom order in the chart:
#'   cols <- names(df)
#'
#'   p <- plot_ROME_survey_questions(
#'     df,
#'     columns = cols,
#'     ordering = lvls,
#'     percentage_cutoff = 10,
#'     y_label_wrap_width = 40
#'   )
#'   print(p)
#'
#'   # Save with the recommended size:
#'   ggplot2::ggsave(
#'     "rome_demo.png", p,
#'     width  = attr(p, "width_in"),
#'     height = attr(p, "height_in"),
#'     units  = "in", dpi = 300
#'   )
#' }
#'
#' @export
plot_ROME_survey_questions <- function(data,
                                       columns = NA,
                                       ordering = NA,
                                       percentage_cutoff = 10,
                                       label_size = 4.5,
                                       n_label_pad_text = -0.1,
                                       per_question_in = 0.5,
                                       base_height_in = 0.8,
                                       legend_allowance_in = 0.6,
                                       width_in = 8,
                                       y_label_wrap_width = 40,
                                       legend_wrap_chars = 80,
                                       right_margin_min_pt = 24) {
  .plot_ROME_survey_core(
    data, columns, demographic = NULL, ordering,
    percentage_cutoff, label_size, n_label_pad_text,
    per_question_in, base_height_in, legend_allowance_in, width_in,
    y_label_wrap_width, legend_wrap_chars, right_margin_min_pt
  )
}

#' @rdname plot_ROME_survey
#' @inheritParams plot_ROME_survey_questions
#'
#' @param demographic Optional character scalar naming the demographic column in
#'   \code{data} (e.g., \code{"Gender"}). When supplied, the plot facets by
#'   question (top→bottom in the order of \code{columns}) and draws one stacked
#'   bar per demographic level **within each question**. When \code{NULL} or
#'   \code{NA}, this function **falls back** to \code{plot_ROME_survey_questions()}.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(1)
#'   lvls <- c("Agree a lot","Agree a little","Neither",
#'             "Disagree a little","Disagree a lot")
#'   make_col <- function(n = 400) {
#'     sample(c(lvls, NA), n, replace = TRUE,
#'            prob = c(0.34, 0.29, 0.20, 0.12, 0.05, 0.00))
#'   }
#'
#'   df <- data.frame(
#'     `Is Jake lovely?`  = make_col(),
#'     `Is Jake wonderful?`  = make_col(),
#'     `Is Jake just the best?`  = make_col(),
#'     `Is Jake the person who most inspires you?` = make_col(),
#'     Gender = sample(c("Male","Female"), 400, replace = TRUE, prob = c(0.47, 0.53))
#'   )
#'
#'   cols <- names(df)[names(df) != "Gender"]
#'
#'   p_dem <- plot_ROME_survey_by_demographic(
#'     data = df,
#'     columns = cols,
#'     demographic = "Gender",
#'     ordering = lvls,
#'     percentage_cutoff = 10,
#'     y_label_wrap_width = 30
#'   )
#'   print(p_dem)
#'
#'   ggplot2::ggsave(
#'     "rome_questions_by_gender.png", p_dem,
#'     width  = attr(p_dem, "width_in"),
#'     height = attr(p_dem, "height_in"),
#'     units  = "in", dpi = 300
#'   )
#' }
#'
#' @export
plot_ROME_survey_by_demographic <- function(data,
                                            columns = NA,
                                            demographic = NULL,
                                            ordering = NA,
                                            percentage_cutoff = 10,
                                            label_size = 4.5,
                                            n_label_pad_text = -0.1,
                                            per_question_in = 0.5,
                                            base_height_in = 0.8,
                                            legend_allowance_in = 0.6,
                                            width_in = 8,
                                            y_label_wrap_width = 40,
                                            legend_wrap_chars = 80,
                                            right_margin_min_pt = 24) {
  if (is.null(demographic) || (length(demographic) == 1 && is.na(demographic))) {
    return(plot_ROME_survey_questions(
      data, columns, ordering, percentage_cutoff, label_size, n_label_pad_text,
      per_question_in, base_height_in, legend_allowance_in, width_in,
      y_label_wrap_width, legend_wrap_chars, right_margin_min_pt
    ))
  }
  if (!(demographic %in% names(data))) {
    stop("`demographic` must be the name of a column in `data`, or NULL/NA.")
  }
  .plot_ROME_survey_core(
    data, columns, demographic, ordering,
    percentage_cutoff, label_size, n_label_pad_text,
    per_question_in, base_height_in, legend_allowance_in, width_in,
    y_label_wrap_width, legend_wrap_chars, right_margin_min_pt
  )
}
