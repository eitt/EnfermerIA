# Confirmatory factor analysis ------------------------------------------------
# WLSMV is appropriate for five-category ordinal items and does not require
# multivariate normality of the observed responses.

att_neg_scored <- paste0(ATT_NEG_ITEMS, "_r")
att_ordered <- c(ATT_POS_ITEMS, att_neg_scored)
comp_ordered <- COMP_ALL_ITEMS

att_model <- paste0(
  "Positive =~ ", paste(ATT_POS_ITEMS, collapse = " + "), "\n",
  "NegativeFavorable =~ ", paste(att_neg_scored, collapse = " + ")
)

comp_model_4f <- paste0(
  "Awareness =~ ", paste(COMP_AWARENESS, collapse = " + "), "\n",
  "Usage =~ ", paste(COMP_USAGE, collapse = " + "), "\n",
  "Evaluation =~ ", paste(COMP_EVALUATION, collapse = " + "), "\n",
  "Ethics =~ ", paste(COMP_ETHICS, collapse = " + ")
)

comp_model_second_order <- paste0(
  comp_model_4f, "\n",
  "AICompetence =~ Awareness + Usage + Evaluation + Ethics"
)

fit_cfa_safely <- function(model, data, ordered_items, model_name) {
  fit <- tryCatch(
    suppressWarnings(lavaan::cfa(
      model,
      data = data,
      ordered = ordered_items,
      estimator = "WLSMV",
      std.lv = TRUE,
      missing = "pairwise",
      control = list(iter.max = 20000)
    )),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      fit = NULL,
      status = tibble::tibble(
        model = model_name,
        converged = FALSE,
        admissible_solution = FALSE,
        n_used = NA_integer_,
        free_parameters = NA_integer_,
        warning_or_error = conditionMessage(fit)
      )
    ))
  }

  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post_check <- tryCatch(lavaan::lavInspect(fit, "post.check"), error = function(e) FALSE)
  cov_lv <- tryCatch(lavaan::lavInspect(fit, "cov.lv"), error = function(e) NULL)
  min_eigen <- if (is.null(cov_lv)) NA_real_ else min(eigen(cov_lv, symmetric = TRUE, only.values = TRUE)$values)
  admissible <- converged && isTRUE(post_check) && (is.na(min_eigen) || min_eigen > -1e-6)

  warning_text <- character(0)
  if (!converged) warning_text <- c(warning_text, "Model did not converge")
  if (!isTRUE(post_check)) warning_text <- c(warning_text, "lavaan post-estimation check failed")
  if (!is.na(min_eigen) && min_eigen <= -1e-6) warning_text <- c(warning_text, "Latent covariance matrix is not positive definite")

  list(
    fit = fit,
    status = tibble::tibble(
      model = model_name,
      converged = converged,
      admissible_solution = admissible,
      n_used = tryCatch(lavaan::lavInspect(fit, "nobs"), error = function(e) NA_integer_),
      free_parameters = tryCatch(lavaan::lavInspect(fit, "npar"), error = function(e) NA_integer_),
      warning_or_error = if (length(warning_text) == 0) "None" else paste(warning_text, collapse = "; ")
    )
  )
}

extract_fit_safely <- function(fit_result, model_name, sample_label = "Full sample") {
  fit <- fit_result$fit
  if (is.null(fit) || !isTRUE(fit_result$status$converged)) {
    return(tibble::tibble(
      model = model_name, sample = sample_label,
      measure = c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"),
      value = NA_real_
    ))
  }
  requested <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                 "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")
  vals <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
  tibble::tibble(
    model = model_name,
    sample = sample_label,
    measure = requested,
    value = if (is.null(vals)) NA_real_ else unname(vals[requested])
  )
}

loading_table_safely <- function(fit_result, model_name, sample_label = "Full sample") {
  fit <- fit_result$fit
  if (is.null(fit) || !isTRUE(fit_result$status$converged)) return(tibble::tibble())
  tryCatch(
    lavaan::standardizedSolution(fit) |>
      dplyr::filter(op == "=~") |>
      dplyr::transmute(
        model = model_name,
        sample = sample_label,
        factor = lhs,
        item = rhs,
        standardized_loading = est.std,
        se = se,
        z = z,
        p = pvalue
      ),
    error = function(e) tibble::tibble()
  )
}

latent_correlations_safely <- function(fit_result, model_name) {
  fit <- fit_result$fit
  if (is.null(fit) || !isTRUE(fit_result$status$converged)) return(tibble::tibble())
  cor_lv <- tryCatch(lavaan::lavInspect(fit, "cor.lv"), error = function(e) NULL)
  if (is.null(cor_lv)) return(tibble::tibble())
  as.data.frame(as.table(cor_lv)) |>
    dplyr::rename(factor_1 = Var1, factor_2 = Var2, correlation = Freq) |>
    dplyr::mutate(model = model_name, .before = 1)
}

reliability_cfa_safely <- function(fit_result, model_name) {
  fit <- fit_result$fit
  if (is.null(fit) || !isTRUE(fit_result$status$converged) || !isTRUE(fit_result$status$admissible_solution)) {
    return(tibble::tibble(model = model_name, factor = NA_character_,
                          composite_reliability = NA_real_, ave = NA_real_))
  }
  rel <- tryCatch(semTools::compRelSEM(fit), error = function(e) NULL)
  ave <- tryCatch(semTools::AVE(fit), error = function(e) NULL)
  factors <- union(names(rel), names(ave))
  if (length(factors) == 0) {
    return(tibble::tibble(model = model_name, factor = NA_character_,
                          composite_reliability = NA_real_, ave = NA_real_))
  }
  tibble::tibble(
    model = model_name,
    factor = factors,
    composite_reliability = if (is.null(rel)) NA_real_ else as.numeric(rel[factors]),
    ave = if (is.null(ave)) NA_real_ else as.numeric(ave[factors])
  )
}

# Full-sample models.
fit_att <- fit_cfa_safely(att_model, analysis_data, att_ordered, "GAAIS two-factor")
fit_comp_4f <- fit_cfa_safely(comp_model_4f, analysis_data, comp_ordered, "AI Literacy four-factor")
fit_comp_second <- fit_cfa_safely(comp_model_second_order, analysis_data, comp_ordered,
                                  "AI Literacy second-order")

model_status <- dplyr::bind_rows(fit_att$status, fit_comp_4f$status, fit_comp_second$status)
fit_summary <- dplyr::bind_rows(
  extract_fit_safely(fit_att, "GAAIS two-factor"),
  extract_fit_safely(fit_comp_4f, "AI Literacy four-factor"),
  extract_fit_safely(fit_comp_second, "AI Literacy second-order")
)
loadings <- dplyr::bind_rows(
  loading_table_safely(fit_att, "GAAIS two-factor"),
  loading_table_safely(fit_comp_4f, "AI Literacy four-factor"),
  loading_table_safely(fit_comp_second, "AI Literacy second-order")
)
latent_correlations <- dplyr::bind_rows(
  latent_correlations_safely(fit_att, "GAAIS two-factor"),
  latent_correlations_safely(fit_comp_4f, "AI Literacy four-factor"),
  latent_correlations_safely(fit_comp_second, "AI Literacy second-order")
)
validity <- dplyr::bind_rows(
  reliability_cfa_safely(fit_att, "GAAIS two-factor"),
  reliability_cfa_safely(fit_comp_4f, "AI Literacy four-factor"),
  reliability_cfa_safely(fit_comp_second, "AI Literacy second-order")
)

# Split-sample cross-validation for the two primary first-order models.
# This is not a multigroup invariance analysis; it is a stability check.
set.seed(CFA_CV_SEED)
row_ids <- sample(seq_len(nrow(analysis_data)))
train_n <- floor(CFA_TRAIN_PROP * nrow(analysis_data))
train_data <- analysis_data[row_ids[seq_len(train_n)], , drop = FALSE]
test_data <- analysis_data[row_ids[(train_n + 1):length(row_ids)], , drop = FALSE]

cv_models <- list(
  list(name = "GAAIS two-factor", syntax = att_model, ordered = att_ordered),
  list(name = "AI Literacy four-factor", syntax = comp_model_4f, ordered = comp_ordered)
)

cv_results <- purrr::map(cv_models, function(spec) {
  train_fit <- fit_cfa_safely(spec$syntax, train_data, spec$ordered, paste0(spec$name, " train"))
  test_fit <- fit_cfa_safely(spec$syntax, test_data, spec$ordered, paste0(spec$name, " test"))
  list(
    status = dplyr::bind_rows(
      train_fit$status |> dplyr::mutate(sample = "Training"),
      test_fit$status |> dplyr::mutate(sample = "Holdout")
    ),
    fit = dplyr::bind_rows(
      extract_fit_safely(train_fit, spec$name, "Training"),
      extract_fit_safely(test_fit, spec$name, "Holdout")
    ),
    loadings = dplyr::bind_rows(
      loading_table_safely(train_fit, spec$name, "Training"),
      loading_table_safely(test_fit, spec$name, "Holdout")
    )
  )
})

cv_status <- dplyr::bind_rows(purrr::map(cv_results, "status"))
cv_fit <- dplyr::bind_rows(purrr::map(cv_results, "fit"))
cv_loadings <- dplyr::bind_rows(purrr::map(cv_results, "loadings"))

cv_fit_wide <- cv_fit |>
  dplyr::filter(measure %in% c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")) |>
  tidyr::pivot_wider(names_from = c(sample, measure), values_from = value)

cv_loading_stability <- cv_loadings |>
  dplyr::select(model, sample, factor, item, standardized_loading) |>
  tidyr::pivot_wider(names_from = sample, values_from = standardized_loading) |>
  dplyr::mutate(abs_loading_difference = abs(Training - Holdout))

cv_decision <- cv_fit_wide |>
  dplyr::mutate(
    cfi_drop = `Training_cfi.scaled` - `Holdout_cfi.scaled`,
    rmsea_increase = `Holdout_rmsea.scaled` - `Training_rmsea.scaled`,
    srmr_increase = `Holdout_srmr` - `Training_srmr`,
    possible_overfit = cfi_drop > .02 | rmsea_increase > .015 | srmr_increase > .015,
    interpretation = dplyr::if_else(
      possible_overfit,
      "Material deterioration in the holdout sample suggests instability or possible overfitting.",
      "Fit is reasonably stable across training and holdout samples; no strong evidence of overfitting."
    )
  )

# Decision-oriented CFA applicability table.
applicability <- factorability_summary |>
  dplyr::mutate(
    sample_to_item_ratio = n_complete / items,
    kmo_adequate = kmo_overall >= .60,
    bartlett_significant = bartlett_p < .05,
    item_variance_adequate = min_item_variance > 0,
    sample_size_adequate = n_complete >= 200 & sample_to_item_ratio >= 10,
    cfa_applicable = kmo_adequate & bartlett_significant & item_variance_adequate & sample_size_adequate,
    interpretation = dplyr::if_else(
      cfa_applicable,
      "CFA is applicable. Interpret fit, loadings, residuals, latent correlations, and holdout stability jointly.",
      "CFA may be unstable or poorly identified; inspect failed criteria before interpreting fit."
    )
  )

append_audit_log("CFA estimation", "ordinal CFA models",
                 "Estimated full-sample models and 70/30 split-sample stability checks using WLSMV")

write_workbook_safely(
  list(
    Model_status = model_status,
    Fit_indices = fit_summary,
    Standardized_loadings = loadings,
    Latent_correlations = latent_correlations,
    Reliability_AVE = validity,
    CFA_applicability = applicability,
    CV_model_status = cv_status,
    CV_fit_indices = cv_fit,
    CV_fit_decision = cv_decision,
    CV_loading_stability = cv_loading_stability
  ),
  file.path(TABLE_DIR, "cfa_results.xlsx")
)

capture_model_summary <- function(fit_result, path) {
  if (is.null(fit_result$fit)) {
    writeLines(fit_result$status$warning_or_error, path)
  } else {
    capture.output(summary(fit_result$fit, fit.measures = TRUE, standardized = TRUE), file = path)
  }
}

capture_model_summary(fit_att, file.path(MODEL_DIR, "cfa_gaais_summary.txt"))
capture_model_summary(fit_comp_4f, file.path(MODEL_DIR, "cfa_ai_literacy_4factor_summary.txt"))
capture_model_summary(fit_comp_second, file.path(MODEL_DIR, "cfa_ai_literacy_second_order_summary.txt"))

saveRDS(
  list(
    attitudes = fit_att$fit,
    competence_4factor = fit_comp_4f$fit,
    competence_second_order = fit_comp_second$fit,
    model_status = model_status,
    cross_validation = cv_results
  ),
  file.path(MODEL_DIR, "cfa_fitted_models.rds")
)