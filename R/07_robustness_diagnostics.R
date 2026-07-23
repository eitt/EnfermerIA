# Robustness, decision-register, and all-sample psychometric diagnostics -----
# This script reports pre-specified alternatives and sensitivity analyses.
# It never selects a model solely because it has the best fit index.

sample_roles <- tibble::tibble(
  sample_key = names(ATTENTION_SUBSAMPLES),
  sample = vapply(
    ATTENTION_SUBSAMPLES,
    function(x) x$label,
    character(1)
  ),
  sample_role = vapply(
    ATTENTION_SUBSAMPLES,
    function(x) x$role,
    character(1)
  ),
  minimum_correct = vapply(
    ATTENTION_SUBSAMPLES,
    function(x) x$minimum_correct,
    numeric(1)
  ),
  primary_sample = names(ATTENTION_SUBSAMPLES) == PRIMARY_ANALYSIS_SAMPLE,
  attention_key_status = attention_key_status
)

scale_definitions <- list(
  GAAIS_Positive = ATT_POS_ITEMS,
  GAAIS_Negative_reversed = ATT_NEG_SCORED,
  AI_Literacy_Awareness = COMP_AWARENESS,
  AI_Literacy_Usage = COMP_USAGE,
  AI_Literacy_Evaluation = COMP_EVALUATION,
  AI_Literacy_Ethics = COMP_ETHICS,
  AI_Literacy_Total = COMP_ALL_ITEMS
)

safe_reliability_row <- function(data, items, scale_name, sample_key, sample_label) {
  if (nrow(data) < 2) {
    return(
      tibble::tibble(
        sample_key = sample_key,
        sample = sample_label,
        scale = scale_name,
        items = length(items),
        n_complete = 0L,
        cronbach_alpha = NA_real_,
        standardized_alpha = NA_real_,
        omega_total = NA_real_,
        mean_interitem_r = NA_real_,
        status = "Not estimable: fewer than two rows"
      )
    )
  }

  result <- tryCatch(
    reliability_row(data, items, scale_name),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(
      tibble::tibble(
        sample_key = sample_key,
        sample = sample_label,
        scale = scale_name,
        items = length(items),
        n_complete = sum(stats::complete.cases(data[, items, drop = FALSE])),
        cronbach_alpha = NA_real_,
        standardized_alpha = NA_real_,
        omega_total = NA_real_,
        mean_interitem_r = NA_real_,
        status = "Estimation failed; inspect warnings"
      )
    )
  }

  result |>
    dplyr::mutate(
      sample_key = sample_key,
      sample = sample_label,
      status = "Estimated",
      .before = 1
    )
}

reliability_all_samples <- purrr::map_dfr(
  names(analysis_subsamples),
  function(sample_key) {
    sample_data <- analysis_subsamples[[sample_key]]
    sample_label <- attr(sample_data, "sample_label")
    purrr::map_dfr(
      names(scale_definitions),
      function(scale_name) {
        safe_reliability_row(
          sample_data,
          scale_definitions[[scale_name]],
          scale_name,
          sample_key,
          sample_label
        )
      }
    )
  }
)

factorability_all_samples <- purrr::map_dfr(
  names(analysis_subsamples),
  function(sample_key) {
    sample_data <- analysis_subsamples[[sample_key]]
    sample_label <- attr(sample_data, "sample_label")
    purrr::map_dfr(
      list(
        list(items = att_all_scored, scale = "GAAIS two-factor item set"),
        list(items = COMP_ALL_ITEMS, scale = "AI Literacy four-factor item set")
      ),
      function(specification) {
        result <- tryCatch(
          factorability(
            sample_data,
            specification$items,
            specification$scale
          ),
          error = function(e) {
            tibble::tibble(
              scale = specification$scale,
              n_complete = NA_integer_,
              items = length(specification$items),
              usable_items = NA_integer_,
              removed_zero_variance_items = NA_character_,
              missing_items = NA_character_,
              kmo_overall = NA_real_,
              bartlett_chisq = NA_real_,
              bartlett_df = NA_real_,
              bartlett_p = NA_real_,
              min_item_variance = NA_real_,
              max_absolute_item_correlation = NA_real_,
              status = paste("Estimation failed:", conditionMessage(e))
            )
          }
        )
        result |>
          dplyr::mutate(
            sample_key = sample_key,
            sample = sample_label,
            .before = 1
          )
      }
    )
  }
)

reverse_means_all_samples <- purrr::map_dfr(
  names(analysis_subsamples),
  function(sample_key) {
    sample_data <- analysis_subsamples[[sample_key]]
    sample_label <- attr(sample_data, "sample_label")
    factor_reverse_pairs |>
      purrr::pmap_dfr(
        function(original_variable, reversed_variable) {
          before <- suppressWarnings(
            as.numeric(sample_data[[original_variable]])
          )
          after <- suppressWarnings(
            as.numeric(sample_data[[reversed_variable]])
          )
          mean_before <- safe_mean(before)
          mean_after <- safe_mean(after)
          expected <- if (is.na(mean_before)) NA_real_ else 6 - mean_before
          tibble::tibble(
            sample_key = sample_key,
            sample = sample_label,
            original_variable = original_variable,
            reversed_variable = reversed_variable,
            n_before = sum(!is.na(before)),
            n_after = sum(!is.na(after)),
            mean_before = mean_before,
            mean_after = mean_after,
            expected_mean_after = expected,
            difference_from_expected = mean_after - expected,
            reverse_check = if (
              is.na(expected)
            ) {
              "NOT ESTIMABLE"
            } else if (
              abs(mean_after - expected) <= 1e-10
            ) {
              "OK"
            } else {
              "CHECK"
            }
          )
        }
      )
  }
)

model_decision_summary <- purrr::map_dfr(
  all_results,
  function(result) {
    definition <- model_definitions[[result$model_key]]
    sample_role <- sample_roles$sample_role[
      match(result$sample_key, sample_roles$sample_key)
    ]
    fit_values <- if (
      is.null(result$fit) || !result$converged
    ) {
      cfi <- NA_real_
      rmsea <- NA_real_
      srmr <- NA_real_
    } else {
      measures <- tryCatch(
        lavaan::fitMeasures(result$fit),
        error = function(e) numeric(0)
      )
      cfi <- unname(measures[["cfi.scaled"]])
      rmsea <- unname(measures[["rmsea.scaled"]])
      srmr <- unname(measures[["srmr"]])
    }

    loading_data <- tryCatch(
      extract_loadings(result),
      error = function(e) tibble::tibble()
    )
    latent_data <- tryCatch(
      extract_latent_correlations(result),
      error = function(e) {
        tibble::tibble(
          factor_1 = character(0),
          factor_2 = character(0),
          correlation = numeric(0)
        )
      }
    )

    # Failed or zero-row models may return an empty tibble without the
    # expected columns. Keep the downstream diagnostics well-defined.
    required_latent_columns <- c(
      "factor_1",
      "factor_2",
      "correlation"
    )
    if (
      !all(required_latent_columns %in% names(latent_data))
    ) {
      latent_data <- tibble::tibble(
        factor_1 = character(0),
        factor_2 = character(0),
        correlation = numeric(0)
      )
    }

    off_diagonal <- latent_data |>
      dplyr::filter(factor_1 != factor_2)
    max_latent_correlation <- if (
      nrow(off_diagonal) == 0
    ) {
      NA_real_
    } else {
      max(abs(off_diagonal$correlation), na.rm = TRUE)
    }
    n_latent_corr_ge_0_95 <- if (
      nrow(off_diagonal) == 0
    ) {
      0L
    } else {
      sum(abs(off_diagonal$correlation) >= 0.95, na.rm = TRUE)
    }
    min_abs_loading <- if (
      nrow(loading_data) == 0
    ) {
      NA_real_
    } else {
      min(abs(loading_data$loading), na.rm = TRUE)
    }
    n_abs_loadings_lt_0_30 <- if (
      nrow(loading_data) == 0
    ) {
      0L
    } else {
      sum(abs(loading_data$loading) < 0.30, na.rm = TRUE)
    }

    decision <- dplyr::case_when(
      !result$converged ~ "Not estimable",
      !result$admissible ~ "Do not interpret: inadmissible solution",
      result$model_key %in% c("competence_3f_awareness_evaluation", "competence_1f") ~
        "Exploratory alternative: do not select by fit alone",
      TRUE ~ "Admissible reference model: interpret with fit and theory"
    )

    tibble::tibble(
      model_key = result$model_key,
      model = definition$label,
      analysis_role = definition$analysis_role,
      sample_key = result$sample_key,
      sample = result$sample_label,
      sample_role = sample_role,
      attention_key_status = attention_key_status,
      n_used = if (
        is.null(result$fit)
      ) {
        0L
      } else {
        tryCatch(
          as.integer(lavaan::lavInspect(result$fit, "nobs")),
          error = function(e) NA_integer_
        )
      },
      converged = result$converged,
      post_check = result$post_check,
      admissible = result$admissible,
      min_eigen_latent_covariance = result$min_eigen_latent_covariance,
      max_abs_latent_correlation = max_latent_correlation,
      n_latent_correlations_abs_ge_0_95 = n_latent_corr_ge_0_95,
      min_abs_loading = min_abs_loading,
      n_abs_loadings_lt_0_30 = n_abs_loadings_lt_0_30,
      cfi_scaled = cfi,
      rmsea_scaled = rmsea,
      srmr = srmr,
      decision = decision
    )
  }
)

decision_register <- tibble::tribble(
  ~decision_id, ~decision, ~rule, ~status,
  "D1", "Primary sample", "Use all core-eligible observations until attention-key validation is complete", "Applied",
  "D2", "Attention sensitivity", "Retain provisional attention subsamples; do not treat them as validated exclusions", "Applied",
  "D3", "CFA admissibility", "Do not interpret models with post-check failure, negative latent-covariance eigenvalue, or latent correlation >= .95", "Applied",
  "D4", "Alternative models", "Report pre-specified alternatives without selecting by fit alone", "Applied",
  "D5", "Item deletion", "No item is deleted automatically; content and scoring review are required", "Applied",
  "D6", "Future validation", "Use EFA/CFA split or independent replication for any data-informed restructuring", "Recommended"
)

message("\nRobustness and decision diagnostics")
message("Primary analysis sample: ", PRIMARY_ANALYSIS_SAMPLE)
message("Attention-key status: ", attention_key_status)
print(sample_roles, n = Inf)
print(reliability_all_samples, n = Inf)
print(model_decision_summary, n = Inf)
print(decision_register, n = Inf)

reliability_plot_data <- reliability_all_samples |>
  dplyr::filter(!is.na(cronbach_alpha))

if (nrow(reliability_plot_data) > 0) {
  p_reliability <- ggplot2::ggplot(
    reliability_plot_data,
    ggplot2::aes(
      x = sample,
      y = cronbach_alpha,
      group = scale,
      color = scale
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0.70,
      linetype = 2,
      color = PALETTE_OKABE_ITO[["black"]]
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      title = "Cronbach alpha across primary and sensitivity samples",
      subtitle = "The dashed line is a descriptive 0.70 reference, not a selection rule",
      x = NULL,
      y = "Cronbach alpha",
      color = "Scale"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
    ) +
    theme_accessible()

  save_plot_300(
    p_reliability,
    file.path(
      FIGURE_DIR,
      "reliability_across_all_attention_samples.png"
    ),
    width = 12,
    height = 7
  )
}

decision_plot_data <- model_decision_summary |>
  dplyr::mutate(
    status = dplyr::case_when(
      !converged ~ "Not estimable",
      !admissible ~ "Inadmissible",
      TRUE ~ "Admissible"
    )
  )

if (nrow(decision_plot_data) > 0) {
  p_decisions <- ggplot2::ggplot(
    decision_plot_data,
    ggplot2::aes(
      x = sample,
      y = model,
      fill = status
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::labs(
      title = "Model admissibility across samples",
      subtitle = "Admissibility is reported separately from fit-index comparisons",
      x = NULL,
      y = NULL,
      fill = "Solution status"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
    ) +
    theme_accessible()

  save_plot_300(
    p_decisions,
    file.path(
      FIGURE_DIR,
      "model_admissibility_across_samples.png"
    ),
    width = 12,
    height = 7
  )
}

write_workbook_safely(
  list(
    Sample_roles = sample_roles,
    Reliability_all_samples = reliability_all_samples,
    Factorability_all_samples = factorability_all_samples,
    Reverse_means_all_samples = reverse_means_all_samples,
    Model_decision_summary = model_decision_summary,
    Decision_register = decision_register
  ),
  file.path(
    TABLE_DIR,
    "robustness_and_decisions.xlsx"
  )
)

writeLines(
  c(
    "Robustness and decision register",
    paste0("Primary sample: ", PRIMARY_ANALYSIS_SAMPLE),
    paste0("Attention-key status: ", attention_key_status),
    "",
    capture.output(print(decision_register, n = Inf)),
    "",
    capture.output(print(model_decision_summary, n = Inf))
  ),
  file.path(
    LOG_DIR,
    "robustness_decision_register.txt"
  )
)

append_audit_log(
  action = "Robustness and decision diagnostics",
  object = "All psychometric models and attention subsamples",
  detail = paste0(
    "Reported reliability, factorability, reverse-score means, model admissibility, ",
    "pre-specified alternatives, and decision rules across all configured samples. ",
    "No model was selected by fit index alone. Primary sample = ",
    PRIMARY_ANALYSIS_SAMPLE,
    "; attention-key status = ",
    attention_key_status,
    "."
  ),
  rows_affected = nrow(analysis_data),
  columns_affected = length(unique(c(ATTITUDE_MODEL_ITEMS, COMP_ALL_ITEMS)))
)
