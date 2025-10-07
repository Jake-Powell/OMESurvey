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
#' @param data A data frame where each selected column is a survey question and
#'   values are categorical responses (character or factor). \code{NA}s are treated
#'   as non-responses and excluded from the per-question \code{N}.
#' @param columns Character or integer vector selecting the question columns;
#'   **order matters** (bars appear top→bottom in this order). Default \code{NA}
#'   uses all columns in \code{data}.
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
#'   question text on the y axis.
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
#'   cols <- names(df)  # e.g., c("Is Jake lovely?", "Is Jake wonderful?", ...)
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
                                       percentage_cutoff = 10,       # % points (0-100)
                                       label_size = 4.5,             # % labels + N labels
                                       n_label_pad_text = -0.1,      # negative nudges N right of 100%
                                       # --- sizing controls (inches) ---
                                       per_question_in = 0.5,        # height per question row
                                       base_height_in = 0.8,         # top/bottom allowance
                                       legend_allowance_in = 0.6,    # extra space for bottom legend
                                       width_in = 8,                  # suggested width
                                       y_label_wrap_width = 40,
                                       # --- legend controls ---
                                       legend_wrap_chars = 80,        # ~chars per row before wrapping
                                       right_margin_min_pt = 24       # minimum right margin for N labels
) {
  data <- data[, columns]

  # 1) Extract question names
  questions <- names(data)
  n_questions <- length(questions)

  # 2) Format data
  freq_tables <- lapply(seq_len(ncol(data)), function(index) {
    d <- data[, index] |> table() |> data.frame()
    d <- data.frame(a = rep(questions[index], nrow(d)), d)
    d$percent <- d$Freq / sum(d$Freq) * 100
    names(d) <- c(letters[1:4])
    d
  })
  data_format <- do.call(rbind, freq_tables)
  names(data_format) <- c("question", "answer", "count", "percent")

  # --- ORDERING FIX: lock top-to-bottom y-axis order to the 'columns' order ---
  # coord_flip() draws factor levels from bottom->top, so use rev(questions)
  # to make the visual order top->bottom match the given 'columns' order.
  data_format$question <- factor(data_format$question, levels = rev(questions))

  if (!is.na(ordering[1])) {
    data_format$answer <- factor(as.character(data_format$answer),
                                 levels = rev(ordering))
  }

  # In-bar labels (only if >= cutoff)
  data_format$label <- ifelse(
    data_format$percent >= percentage_cutoff,
    paste0(round(data_format$percent, 0), "%"),
    ""
  )

  # N per question (exclude NA)
  N_per_question <- sapply(data, function(col) sum(!is.na(col)))
  n_df <- data.frame(
    question = names(N_per_question),
    N = as.integer(N_per_question),
    y = 100,  # anchor at 100 so axis doesn't extend
    n_label = paste0("(", scales::comma(N_per_question), ")"),
    stringsAsFactors = FALSE
  )
  # Keep N labels on the same discrete scale/order as bars
  n_df$question <- factor(n_df$question, levels = levels(data_format$question))

  # Axis: labels only at 0/25/50/75/100, gridlines every 5%
  quartiles <- c(0, 25, 50, 75, 100)
  lbl_fun <- function(x) ifelse(x %in% quartiles, paste0(x, "%"), "")

  # Palette length from actually-used levels
  current_levels <- if (is.factor(data_format$answer)) levels(data_format$answer) else unique(as.character(data_format$answer))
  colo <- get_OME_colours(n = length(current_levels), type = "contrast")
  xaxis_label <- ""

  # --- Legend wrapping + left alignment ---
  # crude width proxy: total label chars; wrap into ~legend_wrap_chars per row
  legend_labels <- current_levels
  total_chars <- sum(nchar(legend_labels)) + 3 * length(legend_labels)  # +key/spacing allowance
  legend_rows <- max(1, ceiling(total_chars / legend_wrap_chars))

  # --- Dynamic right margin (pts) based on longest "(12,345)" label ---
  max_n_chars <- max(nchar(n_df$n_label))
  # ~5.5pt per char as a simple heuristic + a little padding
  right_margin_pt <- max(right_margin_min_pt, ceiling(5.5 * max_n_chars) + 6)

  p <- ggplot2::ggplot(
    data = data_format,
    ggplot2::aes(x = question, y = percent, fill = factor(answer))
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size = label_size
    ) +
    ROME_ggtheme(18) +
    ggplot2::scale_fill_manual(values = colo, drop = FALSE) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = y_label_wrap_width), drop = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, by = 5),     # gridlines every 5%
      labels = lbl_fun,                  # labels only at quartiles
      limits = c(0, 100),
      expand = c(0, 0)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(xaxis_label) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme(
      # Legend: left-justify + minimize wasted space
      legend.title        = ggplot2::element_blank(),
      legend.position     = "bottom",
      legend.direction    = "horizontal",
      legend.justification = 'center',
      legend.location = 'plot',
      legend.box.just     = "left",
      legend.margin       = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin   = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing  = grid::unit(2, "pt"),
      legend.spacing.x    = grid::unit(6, "pt"),
      legend.spacing.y    = grid::unit(2, "pt"),
      # Axes: no ticks/lines; keep only gridlines
      axis.line.x         = ggplot2::element_blank(),
      axis.line.y         = ggplot2::element_blank(),
      axis.ticks.x        = ggplot2::element_blank(),
      axis.ticks.y        = ggplot2::element_blank(),
      axis.ticks.length   = grid::unit(0, "pt"),
      # Plot margins: slim left, dynamic right for N labels
      plot.margin         = ggplot2::margin(5.5, right_margin_pt, 5.5, 5.5, "pt")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, nrow = legend_rows, byrow = TRUE)) +
    ggplot2::geom_text(
      data = n_df,
      ggplot2::aes(x = question, y = y, label = n_label),
      inherit.aes = FALSE,
      size = label_size,
      hjust = n_label_pad_text
    )

  #  Figure size recommendation (inches)
  height_in <- base_height_in + n_questions * per_question_in + legend_allowance_in

  attr(p, "width_in")  <- width_in
  attr(p, "height_in") <- height_in
  attr(p, "rome_dims") <- list(width = width_in, height = height_in, units = "in")

  return(p)
}


