# Descriptive statistics and plots -------------------------------------------

# Variable classification based on declared role, not merely storage type.
all_names <- names(raw_data)

scale_items <- c(
  ATT_POS_ITEMS,
  ATT_NEG_ITEMS,
  COMP_ALL_ITEMS,
  "ac_1",
  "ac_2",
  "ac_3",
  "ac_4"
)

continuous_vars <- intersect(
  c("age_years"),
  all_names
)

categorical_vars <- intersect(
  CATEGORICAL_VARS,
  all_names
)

text_vars <- intersect(
  OPEN_TEXT_VARS,
  all_names
)

identifier_vars <- intersect(
  ID_VARS,
  all_names
)

other_numeric <- setdiff(
  names(raw_data)[
    vapply(
      raw_data,
      is.numeric,
      logical(1)
    )
  ],
  c(
    continuous_vars,
    categorical_vars,
    scale_items,
    identifier_vars
  )
)

# ---------------------------------------------------------------------------
# Variable inventory
# ---------------------------------------------------------------------------

variable_inventory <- tibble::tibble(
  variable = all_names
) |>
  dplyr::mutate(
    type = dplyr::case_when(
      variable %in% identifier_vars ~ "Identifier",
      variable %in% continuous_vars ~ "Continuous numeric",
      variable %in% categorical_vars ~ "Categorical / ordinal",
      variable %in% scale_items ~ "Likert item",
      variable %in% text_vars ~ "Open text",
      variable %in% other_numeric ~ "Other numeric",
      TRUE ~ "Other"
    )
  )

# ---------------------------------------------------------------------------
# Numeric-variable summary
# ---------------------------------------------------------------------------

numeric_summary <- raw_data |>
  dplyr::select(
    dplyr::where(is.numeric)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::group_by(
    variable
  ) |>
  dplyr::summarise(
    n = sum(!is.na(value)),
    missing = sum(is.na(value)),
    missing_pct = mean(is.na(value)),
    mean = safe_mean(value),
    sd = safe_sd(value),
    median = if (
      all(is.na(value))
    ) {
      NA_real_
    } else {
      stats::median(
        value,
        na.rm = TRUE
      )
    },
    q1 = if (
      all(is.na(value))
    ) {
      NA_real_
    } else {
      as.numeric(
        stats::quantile(
          value,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        )
      )
    },
    q3 = if (
      all(is.na(value))
    ) {
      NA_real_
    } else {
      as.numeric(
        stats::quantile(
          value,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        )
      )
    },
    min = if (
      all(is.na(value))
    ) {
      NA_real_
    } else {
      suppressWarnings(
        min(
          value,
          na.rm = TRUE
        )
      )
    },
    max = if (
      all(is.na(value))
    ) {
      NA_real_
    } else {
      suppressWarnings(
        max(
          value,
          na.rm = TRUE
        )
      )
    },
    skewness = if (
      sum(!is.na(value)) < 3
    ) {
      NA_real_
    } else {
      psych::skew(
        value,
        na.rm = TRUE
      )
    },
    kurtosis = if (
      sum(!is.na(value)) < 4
    ) {
      NA_real_
    } else {
      psych::kurtosi(
        value,
        na.rm = TRUE
      )
    },
    unique_values = dplyr::n_distinct(
      value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Categorical-variable summary
# ---------------------------------------------------------------------------

categorical_summary <- purrr::map_dfr(
  categorical_vars,
  function(v) {
    
    raw_data |>
      dplyr::count(
        category = .data[[v]],
        name = "n",
        .drop = FALSE
      ) |>
      dplyr::mutate(
        variable = v,
        percent = n / sum(n)
      ) |>
      dplyr::select(
        variable,
        category,
        n,
        percent
      )
  }
)

# ---------------------------------------------------------------------------
# Open-text-variable summary
# ---------------------------------------------------------------------------

text_summary <- purrr::map_dfr(
  text_vars,
  function(v) {
    
    x <- raw_data[[v]]
    
    nonblank <- !is.na(x) &
      stringr::str_trim(
        as.character(x)
      ) != ""
    
    character_lengths <- nchar(
      as.character(
        x[nonblank]
      )
    )
    
    tibble::tibble(
      variable = v,
      n_nonmissing = sum(nonblank),
      n_missing_or_blank = sum(!nonblank),
      median_characters = if (
        length(character_lengths) == 0
      ) {
        NA_real_
      } else {
        stats::median(
          character_lengths,
          na.rm = TRUE
        )
      },
      mean_characters = if (
        length(character_lengths) == 0
      ) {
        NA_real_
      } else {
        mean(
          character_lengths,
          na.rm = TRUE
        )
      }
    )
  }
)

# ---------------------------------------------------------------------------
# Scale-item descriptives
# ---------------------------------------------------------------------------

scale_descriptives <- dplyr::bind_rows(
  item_summary(
    raw_data,
    ATT_POS_ITEMS,
    "GAAIS Positive"
  ),
  item_summary(
    raw_data,
    ATT_NEG_ITEMS,
    "GAAIS Negative (raw concern wording)"
  ),
  item_summary(
    raw_data,
    COMP_AWARENESS,
    "AI Literacy: Awareness"
  ),
  item_summary(
    raw_data,
    COMP_USAGE,
    "AI Literacy: Usage"
  ),
  item_summary(
    raw_data,
    COMP_EVALUATION,
    "AI Literacy: Evaluation"
  ),
  item_summary(
    raw_data,
    COMP_ETHICS,
    "AI Literacy: Ethics"
  )
)

# ---------------------------------------------------------------------------
# Export descriptive tables
# ---------------------------------------------------------------------------

write_workbook_safely(
  list(
    Variable_inventory = variable_inventory,
    Numeric_summary = numeric_summary,
    Categorical_summary = categorical_summary,
    Scale_items = scale_descriptives,
    Open_text_summary = text_summary,
    Attention_subsamples = subsample_flow,
    Model_missingness = factor_missingness_log,
    Score_availability = score_availability
  ),
  file.path(
    TABLE_DIR,
    "descriptive_statistics_all_variables.xlsx"
  )
)
# ---------------------------------------------------------------------------
# Continuous-variable figures
# ---------------------------------------------------------------------------

for (v in continuous_vars) {
  
  variable_label <- stringr::str_to_title(
    stringr::str_replace_all(
      v,
      "_",
      " "
    )
  )
  
  variable_mean <- safe_mean(
    raw_data[[v]]
  )
  
  p <- ggplot2::ggplot(
    raw_data,
    ggplot2::aes(
      x = .data[[v]]
    )
  ) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = PALETTE_OKABE_ITO[["blue"]],
      color = "white"
    )
  
  if (!is.na(variable_mean)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = variable_mean,
        linetype = 2,
        linewidth = 0.8
      )
  }
  
  p <- p +
    ggplot2::labs(
      title = paste(
        "Distribution of",
        variable_label
      ),
      x = variable_label,
      y = "Participants"
    ) +
    theme_accessible()
  
  save_plot_300(
    p,
    file.path(
      FIGURE_DIR,
      paste0(
        "continuous_",
        v,
        ".png"
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Categorical-variable figures
# ---------------------------------------------------------------------------
# Categories are ordered according to their numeric value whenever possible.
# With coord_flip(), factor levels must be reversed so the smallest value
# appears at the top and the largest value appears at the bottom.

for (v in categorical_vars) {
  
  plot_df <- raw_data |>
    dplyr::count(
      category = .data[[v]],
      name = "n",
      .drop = FALSE
    ) |>
    dplyr::mutate(
      percent = n / sum(n),
      category_character = dplyr::if_else(
        is.na(category),
        "Missing",
        as.character(category)
      )
    )
  
  category_numeric <- suppressWarnings(
    as.numeric(
      plot_df$category_character
    )
  )
  
  nonmissing_categories <- plot_df$category_character != "Missing"
  
  categories_are_numeric <- (
    any(nonmissing_categories) &&
      all(
        !is.na(
          category_numeric[nonmissing_categories]
        )
      )
  )
  
  if (categories_are_numeric) {
    
    # Numeric categories are sorted by their actual value.
    # Missing values, when present, are placed after the valid categories.
    plot_df <- plot_df |>
      dplyr::mutate(
        category_order = dplyr::if_else(
          category_character == "Missing",
          Inf,
          category_numeric
        )
      ) |>
      dplyr::arrange(
        category_order
      )
    
  } else {
    
    # Non-numeric categories are sorted alphabetically.
    # Missing values are placed after valid categories.
    plot_df <- plot_df |>
      dplyr::mutate(
        missing_order = category_character == "Missing"
      ) |>
      dplyr::arrange(
        missing_order,
        stringr::str_to_lower(
          category_character
        )
      )
  }
  
  ordered_categories <- unique(
    plot_df$category_character
  )
  
  # coord_flip() displays the first factor level at the bottom.
  # Reversing the levels places the smallest value at the top.
  plot_df <- plot_df |>
    dplyr::mutate(
      category_label = factor(
        category_character,
        levels = rev(
          ordered_categories
        )
      )
    )
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = category_label,
      y = percent
    )
  ) +
    ggplot2::geom_col(
      fill = PALETTE_OKABE_ITO[["sky_blue"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(
          percent,
          accuracy = 0.1
        )
      ),
      hjust = -0.05,
      size = 3.5
    ) +
    ggplot2::coord_flip(
      clip = "off"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(
        accuracy = 1
      ),
      expand = ggplot2::expansion(
        mult = c(
          0,
          0.10
        )
      )
    ) +
    ggplot2::labs(
      title = stringr::str_to_title(
        stringr::str_replace_all(
          v,
          "_",
          " "
        )
      ),
      x = NULL,
      y = "Percentage"
    ) +
    theme_accessible()
  
  save_plot_300(
    p,
    file.path(
      FIGURE_DIR,
      paste0(
        "categorical_",
        v,
        ".png"
      )
    )
  )
}

append_audit_log(
  action = "Plot ordering",
  object = "Categorical bar plots",
  detail = paste(
    "Categories were ordered by numeric value when possible,",
    "or alphabetically otherwise.",
    "The smallest category is displayed at the top",
    "and the largest category at the bottom."
  )
)

# ---------------------------------------------------------------------------
# Likert-item ordering helper
# ---------------------------------------------------------------------------

item_number <- function(x) {
  
  extracted_number <- stringr::str_extract(
    x,
    "[0-9]+$"
  )
  
  suppressWarnings(
    as.integer(
      extracted_number
    )
  )
}

# ---------------------------------------------------------------------------
# Likert item distributions
# ---------------------------------------------------------------------------
# One figure is generated for each scale.
# Items are shown from the lowest item number at the top to the highest at the
# bottom. Response categories are arranged from 1 on the left to 5 on the right.

plot_likert_scale <- function(
    data,
    items,
    title,
    filename
) {
  
  item_numbers <- item_number(
    items
  )
  
  item_order_index <- order(
    is.na(item_numbers),
    item_numbers,
    items
  )
  
  item_levels_top_to_bottom <- items[
    item_order_index
  ]
  
  # coord_flip() places the first level at the bottom, so reverse factor levels.
  item_levels_for_plot <- rev(
    item_levels_top_to_bottom
  )
  
  long <- data |>
    dplyr::select(
      dplyr::all_of(items)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "item",
      values_to = "response"
    ) |>
    dplyr::filter(
      !is.na(response)
    ) |>
    dplyr::mutate(
      response = suppressWarnings(
        as.integer(
          as.character(response)
        )
      )
    ) |>
    dplyr::filter(
      response %in% 1:5
    ) |>
    dplyr::mutate(
      item = factor(
        item,
        levels = item_levels_for_plot
      ),
      response = factor(
        response,
        levels = 1:5,
        ordered = TRUE
      )
    ) |>
    tidyr::complete(
      item,
      response,
      fill = list(
        n = 0
      )
    )
  
  # Count responses before completing all item-response combinations.
  long <- data |>
    dplyr::select(
      dplyr::all_of(items)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "item",
      values_to = "response"
    ) |>
    dplyr::filter(
      !is.na(response)
    ) |>
    dplyr::mutate(
      response = suppressWarnings(
        as.integer(
          as.character(response)
        )
      )
    ) |>
    dplyr::filter(
      response %in% 1:5
    ) |>
    dplyr::mutate(
      item = factor(
        item,
        levels = item_levels_for_plot
      ),
      response = factor(
        response,
        levels = 1:5,
        ordered = TRUE
      )
    ) |>
    dplyr::count(
      item,
      response,
      name = "n",
      .drop = FALSE
    ) |>
    dplyr::group_by(
      item
    ) |>
    dplyr::mutate(
      item_total = sum(n),
      percent = dplyr::if_else(
        item_total > 0,
        n / item_total,
        0
      )
    ) |>
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = item,
      y = percent,
      fill = response
    )
  ) +
    # reverse = TRUE ensures response 1 is on the left and response 5 on the right.
    ggplot2::geom_col(
      position = ggplot2::position_stack(
        reverse = TRUE
      ),
      width = 0.90
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        "1" = PALETTE_OKABE_ITO[["vermillion"]],
        "2" = PALETTE_OKABE_ITO[["orange"]],
        "3" = "#BDBDBD",
        "4" = PALETTE_OKABE_ITO[["sky_blue"]],
        "5" = PALETTE_OKABE_ITO[["blue"]]
      ),
      breaks = as.character(
        1:5
      ),
      labels = as.character(
        1:5
      ),
      name = "Response",
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(
        accuracy = 1
      ),
      limits = c(
        0,
        1
      ),
      expand = c(
        0,
        0
      )
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Percentage"
    ) +
    theme_accessible()
  
  save_plot_300(
    p,
    file.path(
      FIGURE_DIR,
      filename
    ),
    width = 10,
    height = max(
      6,
      length(items) * 0.42
    )
  )
}

# ---------------------------------------------------------------------------
# Generate Likert figures
# ---------------------------------------------------------------------------

plot_likert_scale(
  data = raw_data,
  items = ATT_POS_ITEMS,
  title = "Positive attitudes toward AI: item distributions",
  filename = "likert_attitude_positive.png"
)

plot_likert_scale(
  data = raw_data,
  items = ATT_NEG_ITEMS,
  title = "Negative attitudes toward AI: raw item distributions",
  filename = "likert_attitude_negative_raw.png"
)

plot_likert_scale(
  data = raw_data,
  items = COMP_ALL_ITEMS,
  title = "AI competence: item distributions",
  filename = "likert_ai_competence.png"
)

append_audit_log(
  action = "Plot ordering",
  object = "Likert plots",
  detail = paste(
    "Items were ordered by ascending item number from top to bottom.",
    "Response categories and colors were arranged from 1 on the left",
    "to 5 on the right."
  )
)