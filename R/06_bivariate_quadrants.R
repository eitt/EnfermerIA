# Two-dimensional attitude quadrant analysis ---------------------------------
# The quadrant figure uses the two GAAIS dimensions:
#   x = Positive attitudes
#   y = Negative concerns in their original wording
#
# No global drop_na() is used. Rows are excluded only when one of these two
# participant-level scores is unavailable.

quadrant_required_variables <- c(
  "attitude_positive",
  "attitude_negative_concern"
)

quadrant_missing_log <- analysis_data |>
  dplyr::filter(
    !stats::complete.cases(
      dplyr::pick(
        dplyr::all_of(
          quadrant_required_variables
        )
      )
    )
  ) |>
  dplyr::transmute(
    source_row,
    id = if ("id" %in% names(analysis_data)) {
      as.character(id)
    } else {
      NA_character_
    },
    positive_score_missing =
      is.na(attitude_positive),
    negative_concern_score_missing =
      is.na(attitude_negative_concern),
    exclusion_reason = dplyr::case_when(
      is.na(attitude_positive) &
        is.na(attitude_negative_concern) ~
        "Both attitude-factor scores are missing",
      is.na(attitude_positive) ~
        "Positive-attitude score is missing",
      is.na(attitude_negative_concern) ~
        "Negative-concern score is missing",
      TRUE ~
        "Included"
    )
  )

plot_data <- analysis_data |>
  dplyr::filter(
    stats::complete.cases(
      dplyr::pick(
        dplyr::all_of(
          quadrant_required_variables
        )
      )
    ),
    is.finite(attitude_positive),
    is.finite(attitude_negative_concern)
  )

if (nrow(plot_data) < 3) {
  stop(
    "Fewer than three observations have valid Positive and Negative attitude scores."
  )
}

# Use the theoretical midpoint of the 1--5 response scale for both axes.
# These cut points are intentionally fixed and are not calculated from the data.
x_cut <- 3
y_cut <- 3

plot_data <- plot_data |>
  dplyr::mutate(
    quadrant = dplyr::case_when(
      attitude_positive >= x_cut &
        attitude_negative_concern < y_cut ~
        "Enthusiastic and low-concern",
      
      attitude_positive >= x_cut &
        attitude_negative_concern >= y_cut ~
        "Positive but concerned",
      
      attitude_positive < x_cut &
        attitude_negative_concern >= y_cut ~
        "Cautious or skeptical",
      
      TRUE ~
        "Low-engagement and low-concern"
    )
  )

quadrant_summary <- plot_data |>
  dplyr::count(
    quadrant,
    name = "n"
  ) |>
  dplyr::mutate(
    percent = n / sum(n),
    positive_cut = x_cut,
    negative_concern_cut = y_cut,
    cut_method = "theoretical mean (3)"
  )

pearson_test <- stats::cor.test(
  plot_data$attitude_positive,
  plot_data$attitude_negative_concern,
  method = "pearson"
)

spearman_test <- stats::cor.test(
  plot_data$attitude_positive,
  plot_data$attitude_negative_concern,
  method = "spearman",
  exact = FALSE
)

correlation_summary <- tibble::tibble(
  method = c(
    "Pearson",
    "Spearman"
  ),
  estimate = c(
    unname(
      pearson_test$estimate
    ),
    unname(
      spearman_test$estimate
    )
  ),
  p_value = c(
    pearson_test$p.value,
    spearman_test$p.value
  ),
  conf_low = c(
    pearson_test$conf.int[1],
    NA_real_
  ),
  conf_high = c(
    pearson_test$conf.int[2],
    NA_real_
  ),
  n = nrow(plot_data)
)

quadrant_labels <- tibble::tibble(
  x = c(
    4.55,
    4.55,
    1.45,
    1.45
  ),
  y = c(
    1.45,
    4.55,
    4.55,
    1.45
  ),
  quadrant = c(
    "Enthusiastic and low-concern",
    "Positive but concerned",
    "Cautious or skeptical",
    "Low-engagement and low-concern"
  ),
  label = c(
    "Enthusiastic\nand low-concern",
    "Positive but\nconcerned",
    "Cautious or\nskeptical",
    "Low-engagement\nand low-concern"
  )
)

quadrant_labels <- quadrant_labels |>
  dplyr::left_join(
    quadrant_summary |>
      dplyr::select(quadrant, n),
    by = "quadrant"
  ) |>
  dplyr::mutate(
    label = paste0(label, "\n(n = ", dplyr::coalesce(n, 0L), ")")
  )

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = attitude_positive,
    y = attitude_negative_concern
  )
) +
  ggplot2::annotate(
    "rect",
    xmin = x_cut,
    xmax = Inf,
    ymin = -Inf,
    ymax = y_cut,
    fill = PALETTE_OKABE_ITO[["bluish_green"]],
    alpha = 0.08
  ) +
  ggplot2::annotate(
    "rect",
    xmin = x_cut,
    xmax = Inf,
    ymin = y_cut,
    ymax = Inf,
    fill = PALETTE_OKABE_ITO[["orange"]],
    alpha = 0.08
  ) +
  ggplot2::annotate(
    "rect",
    xmin = -Inf,
    xmax = x_cut,
    ymin = y_cut,
    ymax = Inf,
    fill = PALETTE_OKABE_ITO[["vermillion"]],
    alpha = 0.08
  ) +
  ggplot2::annotate(
    "rect",
    xmin = -Inf,
    xmax = x_cut,
    ymin = -Inf,
    ymax = y_cut,
    fill = PALETTE_OKABE_ITO[["sky_blue"]],
    alpha = 0.08
  ) +
  ggplot2::geom_point(
    alpha = 0.45,
    size = 1.9,
    color = PALETTE_OKABE_ITO[["blue"]],
    position = ggplot2::position_jitter(
      width = 0.025,
      height = 0.025
    )
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE,
    color = PALETTE_OKABE_ITO[["black"]],
    linewidth = 0.8
  ) +
  ggplot2::geom_vline(
    xintercept = x_cut,
    linetype = 2,
    linewidth = 0.8
  ) +
  ggplot2::geom_hline(
    yintercept = y_cut,
    linetype = 2,
    linewidth = 0.8
  ) +
  ggplot2::geom_text(
    data = quadrant_labels,
    ggplot2::aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 3.7
  ) +
  ggplot2::coord_cartesian(
    xlim = c(1, 5),
    ylim = c(1, 5),
    clip = "off"
  ) +
  ggplot2::labs(
    title = "Positive attitudes and negative concerns toward AI",
    subtitle = paste0(
      "Quadrants split at the theoretical midpoint (3 on both axes); Pearson r = ",
      sprintf(
        "%.2f",
        pearson_test$estimate
      ),
      ", p ",
      ifelse(
        pearson_test$p.value < 0.001,
        "< .001",
        paste0(
          "= ",
          sprintf(
            "%.3f",
            pearson_test$p.value
          )
        )
      )
    ),
    x = "Positive attitudes toward AI",
    y = "Negative concerns about AI",
    caption = paste0(
      "The two axes correspond to the Positive and Negative GAAIS dimensions. ",
      "Higher y-values indicate stronger negative concerns."
    )
  ) +
  theme_accessible()

save_plot_300(
  p,
  file.path(
    FIGURE_DIR,
    "positive_negative_attitude_quadrants.png"
  ),
  width = 9,
  height = 7
)

write_workbook_safely(
  list(
    Quadrant_summary = quadrant_summary,
    Correlations = correlation_summary,
    Participant_scores = plot_data,
    Excluded_rows = quadrant_missing_log
  ),
  file.path(
    TABLE_DIR,
    "bivariate_positive_negative_attitudes.xlsx"
  )
)

append_audit_log(
  action = "Quadrant analysis",
  object = "Positive and negative attitude dimensions",
  detail = paste0(
    "The quadrant analysis used ",
    nrow(plot_data),
    " rows. ",
    nrow(quadrant_missing_log),
    " rows were excluded only because Positive or Negative attitude scores ",
    "were unavailable. No unrelated variables were used for drop_na."
  ),
  rows_affected = nrow(quadrant_missing_log),
  columns_affected = 2
)

message("\nQuadrant analysis")
message(
  "Rows included: ",
  nrow(plot_data)
)
message(
  "Rows excluded because one of the two attitude scores was missing: ",
  nrow(quadrant_missing_log)
)
print(
  quadrant_summary,
  n = Inf
)
print(
  correlation_summary,
  n = Inf
)
