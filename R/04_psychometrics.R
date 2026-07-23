# Reliability, factorability, and correlation diagnostics ---------------------

# ---------------------------------------------------------------------------
# Item sets
# ---------------------------------------------------------------------------

att_neg_scored <- ATT_NEG_SCORED

att_all_scored <- c(
  ATT_POS_ITEMS,
  att_neg_scored
)

check_required_columns(
  analysis_data,
  c(
    att_all_scored,
    COMP_ALL_ITEMS
  ),
  "psychometric diagnostics"
)

# ---------------------------------------------------------------------------
# Reliability
# ---------------------------------------------------------------------------

reliability_summary <- dplyr::bind_rows(
  reliability_row(
    analysis_data,
    ATT_POS_ITEMS,
    "GAAIS Positive"
  ),
  reliability_row(
    analysis_data,
    att_neg_scored,
    "GAAIS Negative, reverse-scored/favorable"
  ),
  reliability_row(
    analysis_data,
    COMP_AWARENESS,
    "AI Literacy: Awareness"
  ),
  reliability_row(
    analysis_data,
    COMP_USAGE,
    "AI Literacy: Usage"
  ),
  reliability_row(
    analysis_data,
    COMP_EVALUATION,
    "AI Literacy: Evaluation"
  ),
  reliability_row(
    analysis_data,
    COMP_ETHICS,
    "AI Literacy: Ethics"
  ),
  reliability_row(
    analysis_data,
    COMP_ALL_ITEMS,
    "AI Literacy: Total"
  )
)

message(
  "\nReliability summary"
)

print(
  reliability_summary,
  n = Inf
)

# ---------------------------------------------------------------------------
# Factorability
# ---------------------------------------------------------------------------

factorability <- function(
    data,
    items,
    scale_name
) {
  
  missing_items <- setdiff(
    items,
    names(data)
  )
  
  if (length(missing_items) > 0) {
    
    return(
      tibble::tibble(
        scale = scale_name,
        n_complete = NA_integer_,
        items = length(items),
        usable_items = 0L,
        removed_zero_variance_items = NA_character_,
        missing_items = paste(
          missing_items,
          collapse = ", "
        ),
        kmo_overall = NA_real_,
        bartlett_chisq = NA_real_,
        bartlett_df = NA_real_,
        bartlett_p = NA_real_,
        min_item_variance = NA_real_,
        max_absolute_item_correlation = NA_real_,
        status = "Required variables missing"
      )
    )
  }
  
  x <- data |>
    dplyr::select(
      dplyr::all_of(items)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(
          as.numeric(.x)
        )
      )
    )
  
  complete_n <- sum(
    stats::complete.cases(x)
  )
  
  item_variances <- vapply(
    x,
    function(variable) {
      
      nonmissing_n <- sum(
        !is.na(variable)
      )
      
      if (nonmissing_n < 2) {
        return(
          NA_real_
        )
      }
      
      stats::var(
        variable,
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  
  poly_result <- tryCatch(
    safe_polychoric(
      data,
      items
    ),
    error = function(e) {
      
      list(
        rho = matrix(
          numeric(0),
          nrow = 0,
          ncol = 0
        ),
        usable = character(0),
        removed = items,
        error = conditionMessage(e)
      )
    }
  )
  
  poly <- poly_result$rho
  
  usable_items <- if (
    is.null(poly_result$usable)
  ) {
    character(0)
  } else {
    poly_result$usable
  }
  
  removed_items <- if (
    is.null(poly_result$removed)
  ) {
    character(0)
  } else {
    poly_result$removed
  }
  
  if (
    is.null(poly) ||
    length(poly) == 0 ||
    nrow(poly) < 2
  ) {
    
    return(
      tibble::tibble(
        scale = scale_name,
        n_complete = complete_n,
        items = length(items),
        usable_items = length(
          usable_items
        ),
        removed_zero_variance_items = paste(
          removed_items,
          collapse = ", "
        ),
        missing_items = "",
        kmo_overall = NA_real_,
        bartlett_chisq = NA_real_,
        bartlett_df = NA_real_,
        bartlett_p = NA_real_,
        min_item_variance = if (
          all(is.na(item_variances))
        ) {
          NA_real_
        } else {
          min(
            item_variances,
            na.rm = TRUE
          )
        },
        max_absolute_item_correlation = NA_real_,
        status = if (
          !is.null(poly_result$error)
        ) {
          paste0(
            "Polychoric estimation failed: ",
            poly_result$error
          )
        } else {
          "Fewer than two usable items"
        }
      )
    )
  }
  
  kmo_result <- tryCatch(
    psych::KMO(
      poly
    ),
    error = function(e) NULL
  )
  
  bartlett_result <- tryCatch(
    psych::cortest.bartlett(
      poly,
      n = complete_n
    ),
    error = function(e) NULL
  )
  
  upper_correlations <- abs(
    poly[
      upper.tri(poly)
    ]
  )
  
  max_absolute_item_correlation <- if (
    length(upper_correlations) == 0 ||
    all(is.na(upper_correlations))
  ) {
    NA_real_
  } else {
    max(
      upper_correlations,
      na.rm = TRUE
    )
  }
  
  tibble::tibble(
    scale = scale_name,
    n_complete = complete_n,
    items = length(items),
    usable_items = length(
      usable_items
    ),
    removed_zero_variance_items = paste(
      removed_items,
      collapse = ", "
    ),
    missing_items = "",
    kmo_overall = if (
      is.null(kmo_result)
    ) {
      NA_real_
    } else {
      unname(
        kmo_result$MSA
      )
    },
    bartlett_chisq = if (
      is.null(bartlett_result)
    ) {
      NA_real_
    } else {
      unname(
        bartlett_result$chisq
      )
    },
    bartlett_df = if (
      is.null(bartlett_result)
    ) {
      NA_real_
    } else {
      unname(
        bartlett_result$df
      )
    },
    bartlett_p = if (
      is.null(bartlett_result)
    ) {
      NA_real_
    } else {
      unname(
        bartlett_result$p.value
      )
    },
    min_item_variance = if (
      all(is.na(item_variances))
    ) {
      NA_real_
    } else {
      min(
        item_variances,
        na.rm = TRUE
      )
    },
    max_absolute_item_correlation =
      max_absolute_item_correlation,
    status = dplyr::case_when(
      is.null(kmo_result) &&
        is.null(bartlett_result) ~
        "KMO and Bartlett tests unavailable",
      is.null(kmo_result) ~
        "KMO unavailable",
      is.null(bartlett_result) ~
        "Bartlett test unavailable",
      TRUE ~
        "Estimated"
    )
  )
}

factorability_summary <- dplyr::bind_rows(
  factorability(
    analysis_data,
    att_all_scored,
    "GAAIS two-factor item set"
  ),
  factorability(
    analysis_data,
    COMP_ALL_ITEMS,
    "AI Literacy four-factor item set"
  )
)

message(
  "\nFactorability summary"
)

print(
  factorability_summary,
  n = Inf
)

# ---------------------------------------------------------------------------
# Parallel analysis
# ---------------------------------------------------------------------------
# Parallel analysis is exploratory. It does not replace the theoretically
# specified confirmatory models.

run_parallel_analysis <- function(
    data,
    items,
    title,
    filename,
    scale_name
) {
  
  available_items <- intersect(
    items,
    names(data)
  )
  
  if (length(available_items) < 2) {
    
    append_audit_log(
      action = "Parallel analysis skipped",
      object = scale_name,
      detail = paste0(
        "Fewer than two requested items were available. Available items: ",
        paste(
          available_items,
          collapse = ", "
        )
      ),
      columns_affected = length(
        available_items
      )
    )
    
    return(
      tibble::tibble(
        scale = scale_name,
        suggested_factors = NA_integer_,
        suggested_components = NA_integer_,
        status = "Skipped: fewer than two available items"
      )
    )
  }
  
  analysis_items <- data |>
    dplyr::select(
      dplyr::all_of(
        available_items
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(
          as.numeric(.x)
        )
      )
    )
  
  usable_items <- names(
    analysis_items
  )[
    vapply(
      analysis_items,
      function(x) {
        
        nonmissing <- x[
          !is.na(x)
        ]
        
        length(
          unique(nonmissing)
        ) >= 2
      },
      logical(1)
    )
  ]
  
  removed_items <- setdiff(
    available_items,
    usable_items
  )
  
  if (length(usable_items) < 2) {
    
    append_audit_log(
      action = "Parallel analysis skipped",
      object = scale_name,
      detail = paste0(
        "Fewer than two usable nonconstant items remained. Removed: ",
        paste(
          removed_items,
          collapse = ", "
        )
      ),
      columns_affected = length(
        removed_items
      )
    )
    
    return(
      tibble::tibble(
        scale = scale_name,
        suggested_factors = NA_integer_,
        suggested_components = NA_integer_,
        status = "Skipped: fewer than two usable items"
      )
    )
  }
  
  figure_path <- file.path(
    FIGURE_DIR,
    filename
  )
  
  grDevices::png(
    filename = figure_path,
    width = 10,
    height = 7,
    units = "in",
    res = FIG_DPI
  )
  
  result <- tryCatch(
    suppressMessages(
      psych::fa.parallel(
        analysis_items[
          ,
          usable_items,
          drop = FALSE
        ],
        fa = "fa",
        cor = "poly",
        plot = TRUE,
        main = title
      )
    ),
    error = function(e) e
  )
  
  grDevices::dev.off()
  
  if (inherits(result, "error")) {
    
    append_audit_log(
      action = "Parallel analysis failed",
      object = scale_name,
      detail = conditionMessage(
        result
      ),
      columns_affected = length(
        usable_items
      )
    )
    
    return(
      tibble::tibble(
        scale = scale_name,
        suggested_factors = NA_integer_,
        suggested_components = NA_integer_,
        status = paste0(
          "Failed: ",
          conditionMessage(result)
        )
      )
    )
  }
  
  suggested_factors <- if (
    is.null(result$nfact)
  ) {
    NA_integer_
  } else {
    as.integer(
      result$nfact
    )
  }
  
  suggested_components <- if (
    is.null(result$ncomp)
  ) {
    NA_integer_
  } else {
    as.integer(
      result$ncomp
    )
  }
  
  append_audit_log(
    action = "Parallel analysis",
    object = scale_name,
    detail = paste0(
      "Suggested factors = ",
      ifelse(
        is.na(suggested_factors),
        "NA",
        suggested_factors
      ),
      "; suggested components = ",
      ifelse(
        is.na(suggested_components),
        "NA",
        suggested_components
      ),
      ". Removed nonconstant items: ",
      ifelse(
        length(removed_items) == 0,
        "none",
        paste(
          removed_items,
          collapse = ", "
        )
      ),
      "."
    ),
    columns_affected = length(
      usable_items
    )
  )
  
  tibble::tibble(
    scale = scale_name,
    suggested_factors = suggested_factors,
    suggested_components = suggested_components,
    status = "Estimated"
  )
}

parallel_attitudes <- run_parallel_analysis(
  data = analysis_data,
  items = att_all_scored,
  title = "Parallel analysis: GAAIS items",
  filename = "parallel_analysis_attitudes.png",
  scale_name = "GAAIS attitudes"
)

parallel_competence <- run_parallel_analysis(
  data = analysis_data,
  items = COMP_ALL_ITEMS,
  title = "Parallel analysis: AI Literacy items",
  filename = "parallel_analysis_competence.png",
  scale_name = "AI Literacy"
)

parallel_analysis_summary <- dplyr::bind_rows(
  parallel_attitudes,
  parallel_competence
)

message(
  "\nParallel-analysis summary"
)

print(
  parallel_analysis_summary,
  n = Inf
)

# ---------------------------------------------------------------------------
# Polychoric correlations among observed items
# ---------------------------------------------------------------------------

att_item_cor_result <- safe_polychoric(
  analysis_data,
  att_all_scored
)

comp_item_cor_result <- safe_polychoric(
  analysis_data,
  COMP_ALL_ITEMS
)

att_item_cor <- att_item_cor_result$rho
comp_item_cor <- comp_item_cor_result$rho

if (
  length(
    att_item_cor_result$removed
  ) > 0
) {
  
  append_audit_log(
    action = "Correlation exclusion",
    object = "GAAIS observed items",
    detail = paste0(
      "Excluded zero-variance or unusable items: ",
      paste(
        att_item_cor_result$removed,
        collapse = ", "
      )
    ),
    columns_affected = length(
      att_item_cor_result$removed
    )
  )
}

if (
  length(
    comp_item_cor_result$removed
  ) > 0
) {
  
  append_audit_log(
    action = "Correlation exclusion",
    object = "AI competence observed items",
    detail = paste0(
      "Excluded zero-variance or unusable items: ",
      paste(
        comp_item_cor_result$removed,
        collapse = ", "
      )
    ),
    columns_affected = length(
      comp_item_cor_result$removed
    )
  )
}

# ---------------------------------------------------------------------------
# Correlations among factor and scale scores
# ---------------------------------------------------------------------------
# attitude_favorability_index is intentionally excluded because the revised
# analysis retains Positive attitudes and Negative concerns as distinct
# dimensions.

factor_score_vars_requested <- c(
  "attitude_positive",
  "attitude_negative_concern",
  "attitude_negative_favorable",
  "competence_awareness",
  "competence_usage",
  "competence_evaluation",
  "competence_ethics",
  "competence_total"
)

factor_score_vars_available <- intersect(
  factor_score_vars_requested,
  names(analysis_data)
)

factor_score_vars_missing <- setdiff(
  factor_score_vars_requested,
  factor_score_vars_available
)

if (length(factor_score_vars_missing) > 0) {
  
  append_audit_log(
    action = "Correlation variable availability",
    object = "Factor and scale scores",
    detail = paste0(
      "Requested variables not found and therefore omitted: ",
      paste(
        factor_score_vars_missing,
        collapse = ", "
      )
    ),
    columns_affected = length(
      factor_score_vars_missing
    )
  )
  
  message(
    "\nUnavailable factor-score variables: ",
    paste(
      factor_score_vars_missing,
      collapse = ", "
    )
  )
}

if (length(factor_score_vars_available) < 2) {
  
  stop(
    paste0(
      "Fewer than two factor or scale scores are available. Available: ",
      paste(
        factor_score_vars_available,
        collapse = ", "
      )
    )
  )
}

factor_score_data <- analysis_data |>
  dplyr::select(
    dplyr::all_of(
      factor_score_vars_available
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ suppressWarnings(
        as.numeric(.x)
      )
    )
  )

# ---------------------------------------------------------------------------
# Diagnose factor-score missingness and variance
# ---------------------------------------------------------------------------

factor_score_diagnostics <- purrr::map_dfr(
  names(factor_score_data),
  function(variable) {
    
    x <- factor_score_data[[variable]]
    
    n_nonmissing <- sum(
      !is.na(x)
    )
    
    unique_nonmissing <- dplyr::n_distinct(
      x,
      na.rm = TRUE
    )
    
    variance_value <- if (
      n_nonmissing < 2
    ) {
      NA_real_
    } else {
      stats::var(
        x,
        na.rm = TRUE
      )
    }
    
    usable <- (
      n_nonmissing >= 2 &&
        unique_nonmissing >= 2 &&
        !is.na(variance_value) &&
        is.finite(variance_value) &&
        variance_value > 0
    )
    
    tibble::tibble(
      variable = variable,
      n_total = length(x),
      n_nonmissing = n_nonmissing,
      n_missing = sum(
        is.na(x)
      ),
      missing_pct = mean(
        is.na(x)
      ),
      unique_nonmissing = unique_nonmissing,
      mean = if (
        n_nonmissing == 0
      ) {
        NA_real_
      } else {
        mean(
          x,
          na.rm = TRUE
        )
      },
      sd = if (
        n_nonmissing < 2
      ) {
        NA_real_
      } else {
        stats::sd(
          x,
          na.rm = TRUE
        )
      },
      variance = variance_value,
      usable_for_correlation = usable,
      exclusion_reason = dplyr::case_when(
        n_nonmissing == 0 ~
          "All values are missing",
        n_nonmissing < 2 ~
          "Fewer than two non-missing observations",
        unique_nonmissing < 2 ~
          "Only one unique non-missing value",
        is.na(variance_value) ~
          "Variance could not be estimated",
        !is.finite(variance_value) ~
          "Variance is not finite",
        variance_value <= 0 ~
          "Zero variance",
        TRUE ~
          NA_character_
      )
    )
  }
)

usable_factor_scores <- factor_score_diagnostics |>
  dplyr::filter(
    usable_for_correlation
  ) |>
  dplyr::pull(
    variable
  )

excluded_factor_scores <- factor_score_diagnostics |>
  dplyr::filter(
    !usable_for_correlation
  )

message(
  "\nFactor-score diagnostics"
)

print(
  factor_score_diagnostics,
  n = Inf
)

if (nrow(excluded_factor_scores) > 0) {
  
  append_audit_log(
    action = "Correlation exclusion",
    object = "Factor and scale scores",
    detail = paste0(
      "Excluded from factor-score correlation matrix: ",
      paste(
        paste0(
          excluded_factor_scores$variable,
          " [",
          excluded_factor_scores$exclusion_reason,
          "]"
        ),
        collapse = "; "
      )
    ),
    columns_affected = nrow(
      excluded_factor_scores
    )
  )
}

if (length(usable_factor_scores) < 2) {
  
  stop(
    paste0(
      "Fewer than two usable factor scores remain after checking missingness ",
      "and variance. Inspect factor_score_diagnostics."
    )
  )
}

# ---------------------------------------------------------------------------
# Pairwise sample sizes
# ---------------------------------------------------------------------------

factor_score_pairwise_n <- outer(
  usable_factor_scores,
  usable_factor_scores,
  Vectorize(
    function(variable_1, variable_2) {
      
      sum(
        stats::complete.cases(
          factor_score_data[
            ,
            c(
              variable_1,
              variable_2
            ),
            drop = FALSE
          ]
        )
      )
    }
  )
)

dimnames(
  factor_score_pairwise_n
) <- list(
  usable_factor_scores,
  usable_factor_scores
)

# ---------------------------------------------------------------------------
# Pearson factor-score correlations
# ---------------------------------------------------------------------------

score_cor <- psych::corr.test(
  factor_score_data[
    ,
    usable_factor_scores,
    drop = FALSE
  ],
  use = "pairwise",
  method = "pearson",
  adjust = "holm",
  ci = TRUE
)

factor_cor_matrix <- score_cor$r
factor_p_matrix <- score_cor$p

# Prefer the explicitly calculated pairwise-n matrix because it has guaranteed
# dimensions and names matching the correlation matrix.
factor_n_matrix <- factor_score_pairwise_n

# ---------------------------------------------------------------------------
# Long-format correlation table
# ---------------------------------------------------------------------------

correlation_table <- as.data.frame(
  as.table(
    factor_cor_matrix
  ),
  stringsAsFactors = FALSE
) |>
  dplyr::rename(
    variable_1 = Var1,
    variable_2 = Var2,
    r = Freq
  )

correlation_table$p_holm <- as.vector(
  factor_p_matrix
)

correlation_table$n_pairwise <- as.vector(
  factor_n_matrix
)

correlation_table <- correlation_table |>
  dplyr::mutate(
    absolute_r = abs(r),
    direction = dplyr::case_when(
      is.na(r) ~
        NA_character_,
      r > 0 ~
        "Positive",
      r < 0 ~
        "Negative",
      TRUE ~
        "Zero"
    ),
    magnitude = dplyr::case_when(
      is.na(absolute_r) ~
        NA_character_,
      absolute_r < 0.10 ~
        "Negligible",
      absolute_r < 0.30 ~
        "Small",
      absolute_r < 0.50 ~
        "Moderate",
      TRUE ~
        "Large"
    ),
    significant_holm_05 = (
      !is.na(p_holm) &
        p_holm < 0.05
    )
  )

append_audit_log(
  action = "Correlation analysis",
  object = "Observed items and factor scores",
  detail = paste0(
    "Generated polychoric observed-item matrices and a Pearson factor-score ",
    "matrix using pairwise available observations. Included scores: ",
    paste(
      usable_factor_scores,
      collapse = ", "
    ),
    "."
  ),
  rows_affected = nrow(
    factor_score_data
  ),
  columns_affected = length(
    usable_factor_scores
  )
)

# ---------------------------------------------------------------------------
# Correlation heatmaps
# ---------------------------------------------------------------------------

plot_corr_matrix <- function(
    mat,
    title,
    filename,
    label_size = 0.60
) {
  
  if (
    is.null(mat) ||
    length(mat) == 0 ||
    nrow(mat) < 2 ||
    ncol(mat) < 2
  ) {
    
    append_audit_log(
      action = "Correlation plot skipped",
      object = filename,
      detail = "The matrix contained fewer than two rows or columns."
    )
    
    return(
      invisible(NULL)
    )
  }
  
  finite_values <- mat[
    is.finite(mat)
  ]
  
  if (length(finite_values) == 0) {
    
    append_audit_log(
      action = "Correlation plot skipped",
      object = filename,
      detail = "The matrix contained no finite correlation values."
    )
    
    return(
      invisible(NULL)
    )
  }
  
  figure_path <- file.path(
    FIGURE_DIR,
    filename
  )
  
  grDevices::png(
    filename = figure_path,
    width = 10,
    height = 9,
    units = "in",
    res = FIG_DPI
  )
  
  tryCatch(
    {
      corrplot::corrplot(
        mat,
        method = "color",
        type = "upper",
        order = "original",
        diag = TRUE,
        addCoef.col = "black",
        number.cex = label_size,
        tl.col = "black",
        tl.srt = 45,
        na.label = "NA",
        na.label.col = "grey50",
        col = grDevices::colorRampPalette(
          c(
            PALETTE_OKABE_ITO[["vermillion"]],
            "white",
            PALETTE_OKABE_ITO[["blue"]]
          )
        )(
          200
        ),
        mar = c(
          0,
          0,
          3,
          0
        ),
        title = title
      )
    },
    error = function(e) {
      
      append_audit_log(
        action = "Correlation plot failed",
        object = filename,
        detail = conditionMessage(
          e
        )
      )
      
      message(
        "Correlation plot failed for ",
        filename,
        ": ",
        conditionMessage(e)
      )
    },
    finally = {
      grDevices::dev.off()
    }
  )
  
  invisible(
    figure_path
  )
}

plot_corr_matrix(
  mat = att_item_cor,
  title = "Observed-item correlations: attitudes toward AI",
  filename = "correlation_observed_attitude_items.png",
  label_size = 0.45
)

plot_corr_matrix(
  mat = comp_item_cor,
  title = "Observed-item correlations: AI competence",
  filename = "correlation_observed_competence_items.png",
  label_size = 0.55
)

plot_corr_matrix(
  mat = factor_cor_matrix,
  title = "Correlations among scale and factor scores",
  filename = "correlation_factor_scores.png",
  label_size = 0.70
)

# ---------------------------------------------------------------------------
# Additional correlation-strength plot
# ---------------------------------------------------------------------------

correlation_pairs_plot_data <- correlation_table |>
  dplyr::filter(
    variable_1 != variable_2
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    pair_key = paste(
      sort(
        c(
          variable_1,
          variable_2
        )
      ),
      collapse = " | "
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::distinct(
    pair_key,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    pair_label = paste0(
      variable_1,
      "\n",
      variable_2
    )
  ) |>
  dplyr::arrange(
    r
  )

if (nrow(correlation_pairs_plot_data) > 0) {
  
  p_correlation_pairs <- ggplot2::ggplot(
    correlation_pairs_plot_data,
    ggplot2::aes(
      x = r,
      y = forcats::fct_reorder(
        pair_label,
        r
      )
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = significant_holm_05
      ),
      size = 2.8
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        -1,
        1
      ),
      breaks = seq(
        -1,
        1,
        by = 0.25
      )
    ) +
    ggplot2::labs(
      title = "Pairwise correlations among factor and scale scores",
      subtitle = "Shape indicates Holm-adjusted statistical significance",
      x = "Pearson correlation",
      y = NULL,
      shape = "Holm p < .05"
    ) +
    theme_accessible()
  
  save_plot_300(
    p_correlation_pairs,
    file.path(
      FIGURE_DIR,
      "correlation_factor_score_pairs.png"
    ),
    width = 10,
    height = max(
      7,
      nrow(
        correlation_pairs_plot_data
      ) * 0.28
    )
  )
}

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

write_workbook_safely(
  list(
    Reliability = reliability_summary,
    Factorability = factorability_summary,
    Parallel_analysis = parallel_analysis_summary,
    Factor_score_diagnostics = factor_score_diagnostics,
    Excluded_factor_scores = excluded_factor_scores,
    Observed_attitude_items = matrix_to_table(
      att_item_cor
    ),
    Observed_competence_items = matrix_to_table(
      comp_item_cor
    ),
    Factor_score_correlations = matrix_to_table(
      factor_cor_matrix
    ),
    Factor_score_p_Holm = matrix_to_table(
      factor_p_matrix
    ),
    Factor_score_pairwise_n = matrix_to_table(
      factor_n_matrix
    ),
    Factor_correlations_long = correlation_table
  ),
  file.path(
    TABLE_DIR,
    "psychometric_diagnostics.xlsx"
  )
)

message(
  "\nPsychometric diagnostics completed."
)

message(
  "Workbook: ",
  file.path(
    TABLE_DIR,
    "psychometric_diagnostics.xlsx"
  )
)