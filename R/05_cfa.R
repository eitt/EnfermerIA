# Confirmatory factor analysis across attention-check subsamples --------------

# ---------------------------------------------------------------------------
# Model definitions
# ---------------------------------------------------------------------------

attitude_model_2f <- paste0(
  "Positive =~ ",
  paste(
    ATT_POS_ITEMS,
    collapse = " + "
  ),
  "\n",
  "Negative =~ ",
  paste(
    ATT_NEG_SCORED,
    collapse = " + "
  )
)

competence_model_4f <- paste0(
  "Awareness =~ ",
  paste(
    COMP_AWARENESS,
    collapse = " + "
  ),
  "\n",
  "Usage =~ ",
  paste(
    COMP_USAGE,
    collapse = " + "
  ),
  "\n",
  "Evaluation =~ ",
  paste(
    COMP_EVALUATION,
    collapse = " + "
  ),
  "\n",
  "Ethics =~ ",
  paste(
    COMP_ETHICS,
    collapse = " + "
  )
)

# Pre-specified alternatives for structural diagnosis. These models are not
# selected automatically by fit; they are reported as exploratory comparisons.
competence_model_3f_awareness_evaluation <- paste0(
  "AwarenessEvaluation =~ ",
  paste(
    c(
      COMP_AWARENESS,
      COMP_EVALUATION
    ),
    collapse = " + "
  ),
  "\n",
  "Usage =~ ",
  paste(
    COMP_USAGE,
    collapse = " + "
  ),
  "\n",
  "Ethics =~ ",
  paste(
    COMP_ETHICS,
    collapse = " + "
  )
)

competence_model_1f <- paste0(
  "AI_Literacy =~ ",
  paste(
    COMP_ALL_ITEMS,
    collapse = " + "
  )
)

model_definitions <- list(
  attitude_2f = list(
    label = "GAAIS two-factor model",
    syntax = attitude_model_2f,
    indicators = ATTITUDE_MODEL_ITEMS,
    analysis_role = "Primary theoretical model"
  ),
  competence_4f = list(
    label = "AI Literacy four-factor model",
    syntax = competence_model_4f,
    indicators = COMP_ALL_ITEMS,
    analysis_role = "Primary theoretical model; admissibility required"
  ),
  competence_3f_awareness_evaluation = list(
    label = "AI Literacy exploratory 3-factor model (Awareness/Evaluation combined)",
    syntax = competence_model_3f_awareness_evaluation,
    indicators = COMP_ALL_ITEMS,
    analysis_role = "Pre-specified exploratory alternative"
  ),
  competence_1f = list(
    label = "AI Literacy exploratory general-factor model",
    syntax = competence_model_1f,
    indicators = COMP_ALL_ITEMS,
    analysis_role = "Pre-specified exploratory alternative"
  )
)

# ---------------------------------------------------------------------------
# Create attention-check subsamples
# ---------------------------------------------------------------------------

create_attention_subsample <- function(
    data,
    minimum_correct
) {
  
  if (minimum_correct <= 0) {
    return(data)
  }
  
  data |>
    dplyr::filter(
      !is.na(ac_total_recalculated),
      ac_total_recalculated >= minimum_correct
    )
}

analysis_subsamples <- purrr::imap(
  ATTENTION_SUBSAMPLES,
  function(specification, sample_key) {
    
    sample_data <- create_attention_subsample(
      analysis_data,
      specification$minimum_correct
    )
    
    attr(
      sample_data,
      "sample_key"
    ) <- sample_key
    
    attr(
      sample_data,
      "sample_label"
    ) <- specification$label

    attr(
      sample_data,
      "sample_role"
    ) <- specification$role
    
    sample_data
  }
)

# ---------------------------------------------------------------------------
# Prepare ordinal model data
# ---------------------------------------------------------------------------
# drop_na is applied only to indicators belonging to the current model.

prepare_cfa_data <- function(
    data,
    indicators,
    model_label,
    sample_label
) {
  
  missing_indicators <- setdiff(
    indicators,
    names(data)
  )
  
  if (length(missing_indicators) > 0) {
    stop(
      model_label,
      " is missing variables: ",
      paste(
        missing_indicators,
        collapse = ", "
      )
    )
  }
  
  original_n <- nrow(data)
  
  indicator_data <- data |>
    dplyr::select(
      dplyr::all_of(indicators)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(
          as.numeric(.x)
        )
      )
    )
  
  incomplete_rows <- !stats::complete.cases(
    indicator_data
  )
  
  excluded_detail <- tibble::tibble(
    sample = sample_label,
    model = model_label,
    source_row = data$source_row[
      incomplete_rows
    ],
    id = if ("id" %in% names(data)) {
      as.character(
        data$id[incomplete_rows]
      )
    } else {
      NA_character_
    },
    missing_indicators = apply(
      is.na(
        indicator_data[incomplete_rows, , drop = FALSE]
      ),
      1,
      function(x) {
        paste(
          indicators[x],
          collapse = ", "
        )
      }
    )
  )
  
  complete_data <- indicator_data[
    !incomplete_rows,
    ,
    drop = FALSE
  ]
  
  for (variable in indicators) {
    
    observed_levels <- sort(
      unique(
        complete_data[[variable]]
      )
    )
    
    complete_data[[variable]] <- ordered(
      complete_data[[variable]],
      levels = observed_levels
    )
  }
  
  list(
    data = complete_data,
    original_n = original_n,
    complete_n = nrow(complete_data),
    excluded_n = sum(incomplete_rows),
    excluded_detail = excluded_detail
  )
}

# ---------------------------------------------------------------------------
# Safe CFA estimator
# ---------------------------------------------------------------------------

fit_cfa_safely <- function(
    model_syntax,
    prepared_data,
    indicators,
    model_key,
    model_label,
    sample_key,
    sample_label
) {
  
  warnings_captured <- character(0)
  
  fit <- withCallingHandlers(
    tryCatch(
      lavaan::cfa(
        model = model_syntax,
        data = prepared_data,
        ordered = indicators,
        estimator = "WLSMV",
        parameterization = "theta",
        std.lv = TRUE,
        missing = "listwise",
        control = list(
          iter.max = 20000
        )
      ),
      error = function(e) {
        structure(
          list(
            message = conditionMessage(e)
          ),
          class = "cfa_error"
        )
      }
    ),
    warning = function(w) {
      
      warnings_captured <<- c(
        warnings_captured,
        conditionMessage(w)
      )
      
      invokeRestart(
        "muffleWarning"
      )
    }
  )
  
  if (inherits(fit, "cfa_error")) {
    
    return(
      list(
        model_key = model_key,
        model_label = model_label,
        sample_key = sample_key,
        sample_label = sample_label,
        fit = NULL,
        converged = FALSE,
        post_check = FALSE,
        admissible = FALSE,
        min_eigen_latent_covariance = NA_real_,
        error = fit$message,
        warnings = unique(warnings_captured)
      )
    )
  }
  
  converged <- tryCatch(
    isTRUE(
      lavaan::lavInspect(
        fit,
        "converged"
      )
    ),
    error = function(e) FALSE
  )
  
  post_check <- tryCatch(
    isTRUE(
      lavaan::lavInspect(
        fit,
        "post.check"
      )
    ),
    error = function(e) FALSE
  )
  
  latent_covariance <- tryCatch(
    lavaan::lavInspect(
      fit,
      "cov.lv"
    ),
    error = function(e) NULL
  )
  
  min_eigen <- if (
    is.null(latent_covariance)
  ) {
    NA_real_
  } else {
    tryCatch(
      min(
        eigen(
          latent_covariance,
          symmetric = TRUE,
          only.values = TRUE
        )$values
      ),
      error = function(e) NA_real_
    )
  }
  
  admissible <- (
    converged &&
      post_check &&
      (
        is.na(min_eigen) ||
          min_eigen > -1e-6
      )
  )
  
  list(
    model_key = model_key,
    model_label = model_label,
    sample_key = sample_key,
    sample_label = sample_label,
    fit = fit,
    converged = converged,
    post_check = post_check,
    admissible = admissible,
    min_eigen_latent_covariance = min_eigen,
    error = NA_character_,
    warnings = unique(warnings_captured)
  )
}

# ---------------------------------------------------------------------------
# Extract model outputs
# ---------------------------------------------------------------------------

requested_fit_indices <- c(
  "chisq.scaled",
  "df.scaled",
  "pvalue.scaled",
  "cfi.scaled",
  "tli.scaled",
  "rmsea.scaled",
  "rmsea.ci.lower.scaled",
  "rmsea.ci.upper.scaled",
  "srmr"
)

extract_fit_indices <- function(result) {
  
  if (
    is.null(result$fit) ||
    !result$converged
  ) {
    
    return(
      tibble::tibble(
        model_key = result$model_key,
        model = result$model_label,
        sample_key = result$sample_key,
        sample = result$sample_label,
        fit_index = requested_fit_indices,
        value = NA_real_
      )
    )
  }
  
  fit_values <- tryCatch(
    lavaan::fitMeasures(
      result$fit
    ),
    error = function(e) numeric(0)
  )
  
  tibble::tibble(
    model_key = result$model_key,
    model = result$model_label,
    sample_key = result$sample_key,
    sample = result$sample_label,
    fit_index = requested_fit_indices,
    value = as.numeric(
      fit_values[
        requested_fit_indices
      ]
    )
  )
}

extract_loadings <- function(result) {
  
  if (
    is.null(result$fit) ||
    !result$converged
  ) {
    return(
      tibble::tibble()
    )
  }
  
  tryCatch(
    lavaan::standardizedSolution(
      result$fit
    ) |>
      dplyr::filter(
        op == "=~"
      ) |>
      dplyr::transmute(
        model_key = result$model_key,
        model = result$model_label,
        sample_key = result$sample_key,
        sample = result$sample_label,
        factor = lhs,
        item = rhs,
        loading = est.std,
        se = se,
        z = z,
        p_value = pvalue,
        na_reason = dplyr::case_when(
          is.na(se) & !result$converged ~
            "Model did not converge",
          is.na(se) & !result$admissible ~
            "Model solution was inadmissible",
          is.na(se) ~
            "Standard error unavailable; inspect identification or boundary estimates",
          TRUE ~
            NA_character_
        )
      ),
    error = function(e) {
      tibble::tibble()
    }
  )
}

extract_latent_correlations <- function(result) {
  
  if (
    is.null(result$fit) ||
    !result$converged
  ) {
    return(
      tibble::tibble()
    )
  }
  
  latent_correlations <- tryCatch(
    lavaan::lavInspect(
      result$fit,
      "cor.lv"
    ),
    error = function(e) NULL
  )
  
  if (is.null(latent_correlations)) {
    return(
      tibble::tibble()
    )
  }
  
  as.data.frame(
    as.table(
      latent_correlations
    )
  ) |>
    dplyr::rename(
      factor_1 = Var1,
      factor_2 = Var2,
      correlation = Freq
    ) |>
    dplyr::mutate(
      model_key = result$model_key,
      model = result$model_label,
      sample_key = result$sample_key,
      sample = result$sample_label,
      .before = 1
    )
}

# ---------------------------------------------------------------------------
# Estimate every model in every attention-check sample
# ---------------------------------------------------------------------------

all_results <- list()
model_sample_log <- list()
excluded_row_logs <- list()

for (sample_key in names(analysis_subsamples)) {
  
  sample_data <- analysis_subsamples[[sample_key]]
  
  sample_label <- attr(
    sample_data,
    "sample_label"
  )

  sample_role <- attr(
    sample_data,
    "sample_role"
  )
  
  message(
    "\n============================================================"
  )
  
  message(
    "CFA sample: ",
    sample_label,
    " | n before model-specific missingness = ",
    nrow(sample_data)
  )
  
  message(
    "============================================================"
  )
  
  for (model_key in names(model_definitions)) {
    
    definition <- model_definitions[[model_key]]
    
    prepared <- prepare_cfa_data(
      data = sample_data,
      indicators = definition$indicators,
      model_label = definition$label,
      sample_label = sample_label
    )
    
    message(
      "\nModel: ",
      definition$label
    )
    
    message(
      "Rows before model-specific complete-case filtering: ",
      prepared$original_n
    )
    
    message(
      "Rows retained for this model: ",
      prepared$complete_n
    )
    
    message(
      "Rows excluded because one or more model indicators were missing: ",
      prepared$excluded_n
    )
    
    result <- fit_cfa_safely(
      model_syntax = definition$syntax,
      prepared_data = prepared$data,
      indicators = definition$indicators,
      model_key = model_key,
      model_label = definition$label,
      sample_key = sample_key,
      sample_label = sample_label
    )
    
    result_name <- paste(
      model_key,
      sample_key,
      sep = "__"
    )
    
    all_results[[result_name]] <- result
    
    model_sample_log[[result_name]] <- tibble::tibble(
      model_key = model_key,
      model = definition$label,
      analysis_role = definition$analysis_role,
      sample_key = sample_key,
      sample = sample_label,
      sample_role = sample_role,
      attention_key_status = attention_key_status,
      n_before_model_missingness = prepared$original_n,
      n_used = prepared$complete_n,
      n_excluded_for_model_missingness = prepared$excluded_n,
      converged = result$converged,
      post_check = result$post_check,
      admissible = result$admissible,
      min_eigen_latent_covariance =
        result$min_eigen_latent_covariance,
      error = result$error,
      warnings = ifelse(
        length(result$warnings) == 0,
        NA_character_,
        paste(
          result$warnings,
          collapse = " | "
        )
      )
    )
    
    if (nrow(prepared$excluded_detail) > 0) {
      excluded_row_logs[[result_name]] <-
        prepared$excluded_detail
    }
    
    # Print model fit to the console.
    if (is.null(result$fit)) {
      
      message(
        "Model estimation failed: ",
        result$error
      )
      
    } else {
      
      print(
        lavaan::fitMeasures(
          result$fit,
          requested_fit_indices
        )
      )
      
      message(
        "Converged: ",
        result$converged,
        " | admissible: ",
        result$admissible
      )
    }
    
    # Save complete textual print.
    summary_file <- file.path(
      MODEL_DIR,
      paste0(
        "cfa_",
        model_key,
        "__",
        sample_key,
        "_summary.txt"
      )
    )
    
    if (is.null(result$fit)) {
      
      writeLines(
        c(
          paste(
            "Model:",
            definition$label
          ),
          paste(
            "Sample:",
            sample_label
          ),
          paste(
            "Error:",
            result$error
          )
        ),
        summary_file
      )
      
    } else {
      
      capture.output(
        {
          cat(
            "Model:",
            definition$label,
            "\n"
          )
          
          cat(
            "Sample:",
            sample_label,
            "\n"
          )

          cat(
            "Sample role:",
            sample_role,
            "\n"
          )

          cat(
            "Analysis role:",
            definition$analysis_role,
            "\n"
          )

          cat(
            "Attention-key status:",
            attention_key_status,
            "\n"
          )
          
          cat(
            "Rows used:",
            prepared$complete_n,
            "\n\n"
          )
          
          print(
            summary(
              result$fit,
              fit.measures = TRUE,
              standardized = TRUE,
              rsquare = TRUE
            )
          )
        },
        file = summary_file
      )
    }
  }
}

# ---------------------------------------------------------------------------
# Combine outputs
# ---------------------------------------------------------------------------

model_status <- dplyr::bind_rows(
  model_sample_log
)

fit_indices <- purrr::map_dfr(
  all_results,
  extract_fit_indices
)

loadings <- purrr::map_dfr(
  all_results,
  extract_loadings
)

latent_correlations <- purrr::map_dfr(
  all_results,
  extract_latent_correlations
)

excluded_for_model_missingness <- dplyr::bind_rows(
  excluded_row_logs
)

# ---------------------------------------------------------------------------
# Wide fit comparison
# ---------------------------------------------------------------------------

fit_comparison <- fit_indices |>
  tidyr::pivot_wider(
    names_from = fit_index,
    values_from = value
  )

print(
  fit_comparison,
  n = Inf
)

# ---------------------------------------------------------------------------
# Fit-index figures
# ---------------------------------------------------------------------------

fit_plot_data <- fit_indices |>
  dplyr::filter(
    fit_index %in% c(
      "cfi.scaled",
      "tli.scaled",
      "rmsea.scaled",
      "srmr"
    ),
    !is.na(value)
  ) |>
  dplyr::mutate(
    fit_index = factor(
      fit_index,
      levels = c(
        "cfi.scaled",
        "tli.scaled",
        "rmsea.scaled",
        "srmr"
      ),
      labels = c(
        "CFI",
        "TLI",
        "RMSEA",
        "SRMR"
      )
    )
  )

for (current_model in unique(fit_plot_data$model_key)) {
  
  current_data <- fit_plot_data |>
    dplyr::filter(
      model_key == current_model
    )
  
  if (nrow(current_data) == 0) {
    next
  }
  
  p <- ggplot2::ggplot(
    current_data,
    ggplot2::aes(
      x = sample,
      y = value,
      group = fit_index,
      linetype = fit_index,
      shape = fit_index
    )
  ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      size = 2.8
    ) +
    ggplot2::facet_wrap(
      ~ fit_index,
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = paste0(
        "CFA fit across attention-check subsamples: ",
        unique(current_data$model)
      ),
      x = NULL,
      y = "Fit-index value",
      linetype = "Fit index",
      shape = "Fit index"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 25,
        hjust = 1
      )
    ) +
    theme_accessible()
  
  save_plot_300(
    p,
    file.path(
      FIGURE_DIR,
      paste0(
        "cfa_fit_",
        current_model,
        "_across_attention_samples.png"
      )
    ),
    width = 11,
    height = 7
  )
}

# ---------------------------------------------------------------------------
# Loading plots
# ---------------------------------------------------------------------------

if (nrow(loadings) > 0) {
  
  loading_plot <- loadings |>
    dplyr::filter(
      !is.na(loading)
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = loading,
        y = forcats::fct_reorder(
          item,
          loading
        ),
        shape = sample
      )
    ) +
    ggplot2::geom_vline(
      xintercept = 0.50,
      linetype = 2
    ) +
    ggplot2::geom_point(
      size = 2.4,
      alpha = 0.75
    ) +
    ggplot2::facet_grid(
      model ~ factor,
      scales = "free_y",
      space = "free_y"
    ) +
    ggplot2::labs(
      title = "Standardized CFA loadings across attention-check samples",
      x = "Standardized loading",
      y = NULL,
      shape = "Sample"
    ) +
    theme_accessible()
  
  save_plot_300(
    loading_plot,
    file.path(
      FIGURE_DIR,
      "cfa_loadings_across_attention_samples.png"
    ),
    width = 12,
    height = 12
  )
}

# ---------------------------------------------------------------------------
# Export
# ---------------------------------------------------------------------------

write_workbook_safely(
  list(
    Model_status = model_status,
    Fit_comparison = fit_comparison,
    Fit_long = fit_indices,
    Standardized_loadings = loadings,
    Latent_correlations = latent_correlations,
    Model_missing_rows = excluded_for_model_missingness
  ),
  file.path(
    TABLE_DIR,
    "cfa_results_all_attention_subsamples.xlsx"
  )
)

saveRDS(
  all_results,
  file.path(
    MODEL_DIR,
    "cfa_fitted_models_all_attention_subsamples.rds"
  )
)

append_audit_log(
  action = "CFA estimation",
  object = "Attitude and competence measurement models",
  detail = paste0(
    "Estimated the two-factor attitude model, four-factor competence model, ",
    "and two pre-specified competence alternatives in all core observations ",
    "and in three attention-check sensitivity samples. ",
    "Complete-case filtering was applied separately using only each model's ",
    "indicator variables. Text summaries, fit tables, loadings, correlations, ",
    "and plots were saved."
  ),
  rows_affected = nrow(analysis_data),
  columns_affected = length(
    unique(
      c(
        ATTITUDE_MODEL_ITEMS,
        COMP_ALL_ITEMS
      )
    )
  )
)
