# Descriptive statistics and plots -------------------------------------------

# Variable classification based on declared role, not merely storage type.
all_names <- names(raw_data)
scale_items <- c(ATT_POS_ITEMS, ATT_NEG_ITEMS, COMP_ALL_ITEMS,
                 "ac_1", "ac_2", "ac_3", "ac_4")
continuous_vars <- intersect(c("age_years"), all_names)
categorical_vars <- intersect(CATEGORICAL_VARS, all_names)
text_vars <- intersect(OPEN_TEXT_VARS, all_names)
identifier_vars <- intersect(ID_VARS, all_names)
other_numeric <- setdiff(
  names(raw_data)[vapply(raw_data, is.numeric, logical(1))],
  c(continuous_vars, categorical_vars, scale_items, identifier_vars)
)

variable_inventory <- tibble::tibble(variable = all_names) |>
  dplyr::mutate(type = dplyr::case_when(
    variable %in% identifier_vars ~ "Identifier",
    variable %in% continuous_vars ~ "Continuous numeric",
    variable %in% categorical_vars ~ "Categorical / ordinal",
    variable %in% scale_items ~ "Likert item",
    variable %in% text_vars ~ "Open text",
    variable %in% other_numeric ~ "Other numeric",
    TRUE ~ "Other"
  ))

numeric_summary <- raw_data |>
  dplyr::select(dplyr::where(is.numeric)) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    n = sum(!is.na(value)), missing = sum(is.na(value)), missing_pct = mean(is.na(value)),
    mean = safe_mean(value), sd = safe_sd(value), median = stats::median(value, na.rm = TRUE),
    q1 = stats::quantile(value, .25, na.rm = TRUE), q3 = stats::quantile(value, .75, na.rm = TRUE),
    min = suppressWarnings(min(value, na.rm = TRUE)), max = suppressWarnings(max(value, na.rm = TRUE)),
    skewness = psych::skew(value, na.rm = TRUE), kurtosis = psych::kurtosi(value, na.rm = TRUE),
    unique_values = dplyr::n_distinct(value, na.rm = TRUE), .groups = "drop"
  )

categorical_summary <- purrr::map_dfr(categorical_vars, function(v) {
  raw_data |>
    dplyr::count(category = .data[[v]], name = "n", .drop = FALSE) |>
    dplyr::mutate(variable = v, percent = n / sum(n)) |>
    dplyr::select(variable, category, n, percent)
})

text_summary <- purrr::map_dfr(text_vars, function(v) {
  x <- raw_data[[v]]
  tibble::tibble(
    variable = v, n_nonmissing = sum(!is.na(x) & stringr::str_trim(x) != ""),
    n_missing_or_blank = sum(is.na(x) | stringr::str_trim(x) == ""),
    median_characters = stats::median(nchar(x[!is.na(x)]), na.rm = TRUE),
    mean_characters = mean(nchar(x[!is.na(x)]), na.rm = TRUE)
  )
})

scale_descriptives <- dplyr::bind_rows(
  item_summary(raw_data, ATT_POS_ITEMS, "GAAIS Positive"),
  item_summary(raw_data, ATT_NEG_ITEMS, "GAAIS Negative (raw concern wording)"),
  item_summary(raw_data, COMP_AWARENESS, "AI Literacy: Awareness"),
  item_summary(raw_data, COMP_USAGE, "AI Literacy: Usage"),
  item_summary(raw_data, COMP_EVALUATION, "AI Literacy: Evaluation"),
  item_summary(raw_data, COMP_ETHICS, "AI Literacy: Ethics")
)

write_workbook_safely(
  list(
    Variable_inventory = variable_inventory,
    Numeric_summary = numeric_summary,
    Categorical_summary = categorical_summary,
    Scale_items = scale_descriptives,
    Open_text_summary = text_summary,
    Sample_flow = sample_flow
  ),
  file.path(TABLE_DIR, "descriptive_statistics_all_variables.xlsx")
)

# Continuous-variable figures.
for (v in continuous_vars) {
  p <- ggplot2::ggplot(raw_data, ggplot2::aes(x = .data[[v]])) +
    ggplot2::geom_histogram(bins = 30, fill = PALETTE_OKABE_ITO[["blue"]], color = "white") +
    ggplot2::geom_vline(xintercept = mean(raw_data[[v]], na.rm = TRUE),
                       linetype = 2, linewidth = 0.8) +
    ggplot2::labs(title = paste("Distribution of", stringr::str_replace_all(v, "_", " ")),
                  x = stringr::str_to_title(stringr::str_replace_all(v, "_", " ")), y = "Participants") +
    theme_accessible()
  save_plot_300(p, file.path(FIGURE_DIR, paste0("continuous_", v, ".png")))
}

# Categorical-variable figures.
for (v in categorical_vars) {
  plot_df <- raw_data |> dplyr::count(category = factor(.data[[v]]), name = "n") |>
    dplyr::mutate(percent = n / sum(n))
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = forcats::fct_reorder(category, n), y = percent)) +
    ggplot2::geom_col(fill = PALETTE_OKABE_ITO[["sky_blue"]]) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(percent, accuracy = 0.1)), hjust = -0.05, size = 3.5) +
    ggplot2::labs(title = stringr::str_to_title(stringr::str_replace_all(v, "_", " ")),
                  x = NULL, y = "Percentage") + theme_accessible()
  save_plot_300(p, file.path(FIGURE_DIR, paste0("categorical_", v, ".png")))
}

# Likert item distributions, one file per scale.
plot_likert_scale <- function(data, items, title, filename) {
  long <- data |> dplyr::select(dplyr::all_of(items)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "response") |>
    dplyr::filter(!is.na(response)) |>
    dplyr::count(item, response, name = "n") |>
    dplyr::group_by(item) |> dplyr::mutate(percent = n / sum(n)) |> dplyr::ungroup()
  p <- ggplot2::ggplot(long, ggplot2::aes(x = item, y = percent, fill = factor(response))) +
    ggplot2::geom_col(position = "stack") + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c(
      "1" = PALETTE_OKABE_ITO[["vermillion"]], "2" = PALETTE_OKABE_ITO[["orange"]],
      "3" = "#BDBDBD", "4" = PALETTE_OKABE_ITO[["sky_blue"]], "5" = PALETTE_OKABE_ITO[["blue"]]
    ), name = "Response") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(title = title, x = NULL, y = "Percentage") + theme_accessible()
  save_plot_300(p, file.path(FIGURE_DIR, filename), width = 10, height = max(6, length(items) * .34))
}

plot_likert_scale(raw_data, ATT_POS_ITEMS, "Positive attitudes toward AI: item distributions", "likert_attitude_positive.png")
plot_likert_scale(raw_data, ATT_NEG_ITEMS, "Negative attitudes toward AI: raw item distributions", "likert_attitude_negative_raw.png")
plot_likert_scale(raw_data, COMP_ALL_ITEMS, "AI competence: item distributions", "likert_ai_competence.png")
