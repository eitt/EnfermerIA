# Bivariate analysis and quadrant scatterplot --------------------------------

plot_data <- analysis_data |>
  dplyr::filter(is.finite(attitude_favorability_index), is.finite(competence_total))

x_cut <- if (QUADRANT_CUT == "median") stats::median(plot_data$attitude_favorability_index) else mean(plot_data$attitude_favorability_index)
y_cut <- if (QUADRANT_CUT == "median") stats::median(plot_data$competence_total) else mean(plot_data$competence_total)

plot_data <- plot_data |>
  dplyr::mutate(
    quadrant = dplyr::case_when(
      attitude_favorability_index >= x_cut & competence_total >= y_cut ~ "AI-ready advocates",
      attitude_favorability_index <  x_cut & competence_total >= y_cut ~ "Competent but cautious",
      attitude_favorability_index <  x_cut & competence_total <  y_cut ~ "Skeptical and underprepared",
      TRUE ~ "Positive but capability-limited"
    )
  )

quadrant_summary <- plot_data |>
  dplyr::count(quadrant, name = "n") |>
  dplyr::mutate(percent = n / sum(n), x_cut = x_cut, y_cut = y_cut, cut_method = QUADRANT_CUT)

cor_pearson <- stats::cor.test(plot_data$attitude_favorability_index, plot_data$competence_total, method = "pearson")
cor_spearman <- stats::cor.test(plot_data$attitude_favorability_index, plot_data$competence_total, method = "spearman", exact = FALSE)
cor_summary <- tibble::tibble(
  method = c("Pearson", "Spearman"),
  estimate = c(unname(cor_pearson$estimate), unname(cor_spearman$estimate)),
  p_value = c(cor_pearson$p.value, cor_spearman$p.value),
  conf_low = c(cor_pearson$conf.int[1], NA_real_),
  conf_high = c(cor_pearson$conf.int[2], NA_real_),
  n = nrow(plot_data)
)

labels <- tibble::tibble(
  x = c(4.75, 1.25, 1.25, 4.75), y = c(4.75, 4.75, 1.25, 1.25),
  label = c("AI-ready\nadvocates", "Competent but\ncautious",
            "Skeptical and\nunderprepared", "Positive but\ncapability-limited")
)

p <- ggplot2::ggplot(plot_data,
                     ggplot2::aes(x = attitude_favorability_index, y = competence_total)) +
  ggplot2::annotate("rect", xmin = x_cut, xmax = Inf, ymin = y_cut, ymax = Inf,
                    fill = PALETTE_OKABE_ITO[["bluish_green"]], alpha = .08) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = x_cut, ymin = y_cut, ymax = Inf,
                    fill = PALETTE_OKABE_ITO[["sky_blue"]], alpha = .08) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = x_cut, ymin = -Inf, ymax = y_cut,
                    fill = PALETTE_OKABE_ITO[["vermillion"]], alpha = .08) +
  ggplot2::annotate("rect", xmin = x_cut, xmax = Inf, ymin = -Inf, ymax = y_cut,
                    fill = PALETTE_OKABE_ITO[["orange"]], alpha = .08) +
  ggplot2::geom_point(alpha = .45, size = 1.9, color = PALETTE_OKABE_ITO[["blue"]],
                      position = ggplot2::position_jitter(width = .025, height = .025)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = PALETTE_OKABE_ITO[["black"]], linewidth = .8) +
  ggplot2::geom_vline(xintercept = x_cut, linetype = 2, linewidth = .8) +
  ggplot2::geom_hline(yintercept = y_cut, linetype = 2, linewidth = .8) +
  ggplot2::geom_text(data = labels, ggplot2::aes(x = x, y = y, label = label),
                     inherit.aes = FALSE, fontface = "bold", size = 3.8) +
  ggplot2::coord_cartesian(xlim = c(1, 5), ylim = c(1, 5), clip = "off") +
  ggplot2::labs(
    title = "AI attitudes and competence",
    subtitle = paste0("Quadrants split at the ", QUADRANT_CUT,
                      "; Pearson r = ", sprintf("%.2f", cor_pearson$estimate),
                      ", p ", ifelse(cor_pearson$p.value < .001, "< .001", paste0("= ", sprintf("%.3f", cor_pearson$p.value)))),
    x = "Favorable attitudes toward AI (visualization index)",
    y = "Competence in using AI",
    caption = "The GAAIS source recommends reporting its Positive and Negative subscales separately. The x-axis combines them only for this quadrant visualization."
  ) + theme_accessible()

save_plot_300(p, file.path(FIGURE_DIR, "attitudes_competence_quadrants.png"), width = 9, height = 7)

write_workbook_safely(
  list(Quadrant_summary = quadrant_summary, Correlations = cor_summary, Participant_scores = plot_data),
  file.path(TABLE_DIR, "bivariate_attitudes_competence.xlsx")
)
