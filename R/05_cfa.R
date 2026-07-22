# Confirmatory factor analysis ------------------------------------------------
# Ordinal CFA using WLSMV. Each model is checked before fit indices are
# requested so that one inadmissible model does not stop the whole pipeline.

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

# The second-order model is exploratory here. With only four first-order
# factors it can become empirically inadmissible when factor correlations are
# very high. It is therefore estimated but never allowed to stop the pipeline.
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
prepare_ordinal_data <- function(data, vars) {
  out <- data[, vars, drop = FALSE]
  out[] <- lapply(out, function(x) {
    x <- suppressWarnings(as.integer(x))
    ordered(x, levels = sort(unique(x[!is.na(x)])))
  })
  out
}

att_cfa_data <- prepare_ordinal_data(analysis_data, att_ordered)
comp_cfa_data <- prepare_ordinal_data(analysis_data, comp_ordered)

safe_cfa <- function(model, data, ordered_vars, model_name) {
  warnings_captured <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      lavaan::cfa(
        model = model,
        data = data,
        ordered = ordered_vars,
        estimator = "WLSMV",
        parameterization = "theta",
        std.lv = TRUE,
        missing = "pairwise",
        control = list(iter.max = 10000)
      ),
      error = function(e) structure(list(error = conditionMessage(e)), class = "cfa_error")
    ),
    warning = function(w) {
      warnings_captured <<- c(warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(fit, "cfa_error")) {
    return(list(
      name = model_name, fit = NULL, converged = FALSE, admissible = FALSE,
      error = fit$error, warnings = unique(warnings_captured)
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
  post_check <- tryCatch(isTRUE(lavaan::lavInspect(fit, "post.check")), error = function(e) FALSE)
  cov_lv <- tryCatch(lavaan::lavInspect(fit, "cov.lv"), error = function(e) NULL)
  min_eigen_cov_lv <- if (is.null(cov_lv)) NA_real_ else min(eigen(cov_lv, symmetric = TRUE, only.values = TRUE)$values)
  admissible <- converged && post_check && (is.na(min_eigen_cov_lv) || min_eigen_cov_lv > 1e-8)

  list(
    name = model_name, fit = fit, converged = converged,
    admissible = admissible, error = NA_character_,
    warnings = unique(warnings_captured), min_eigen_cov_lv = min_eigen_cov_lv
  )
}

results <- list(
  attitudes = safe_cfa(
    att_model, att_cfa_data, att_ordered,
    "GAAIS: correlated Positive and NegativeFavorable factors"
  ),
  competence_4f = safe_cfa(
    comp_model_4f, comp_cfa_data, comp_ordered,
    "AI Literacy: four correlated factors"
  ),
  competence_second = safe_cfa(
    comp_model_second_order, comp_cfa_data, comp_ordered,
    "AI Literacy: second-order competence factor"
  )
)

extract_fit <- function(res) {
  requested <- c(
    "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"
  )
  if (is.null(res$fit) || !res$converged) {
    return(tibble::tibble(model = res$name, measure = requested, value = NA_real_))
  }
  vals <- tryCatch(lavaan::fitMeasures(res$fit), error = function(e) numeric(0))
  tibble::tibble(
    model = res$name,
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
model_status <- dplyr::bind_rows(lapply(results, function(res) {
  tibble::tibble(
    model = res$name,
    converged = res$converged,
    admissible_solution = res$admissible,
    min_eigenvalue_latent_covariance = res$min_eigen_cov_lv %||% NA_real_,
    error = res$error,
    warnings = if (length(res$warnings) == 0) NA_character_ else paste(res$warnings, collapse = " | ")
  )
}))

fit_summary <- dplyr::bind_rows(lapply(results, extract_fit))

loading_table <- function(res) {
  if (is.null(res$fit) || !res$converged) return(tibble::tibble())
  tryCatch(
    lavaan::standardizedSolution(res$fit) |>
      dplyr::filter(op == "=~") |>
      dplyr::transmute(
        model = res$name, factor = lhs, item = rhs,
        standardized_loading = est.std, se = se, z = z, p = pvalue
      ),
    error = function(e) tibble::tibble()
  )
}
loadings <- dplyr::bind_rows(lapply(results, loading_table))

reliability_cfa <- function(res) {
  if (is.null(res$fit) || !res$converged || !res$admissible) return(tibble::tibble())
  rel <- tryCatch(semTools::compRelSEM(res$fit), error = function(e) NULL)
  ave <- tryCatch(semTools::AVE(res$fit), error = function(e) NULL)
  factors <- union(names(rel), names(ave))
  if (length(factors) == 0) return(tibble::tibble())
  tibble::tibble(
    model = res$name,
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
validity <- dplyr::bind_rows(lapply(results, reliability_cfa))

# Latent correlations are especially important when a non-positive-definite
# covariance matrix is reported.
latent_correlations <- dplyr::bind_rows(lapply(results, function(res) {
  if (is.null(res$fit) || !res$converged) return(tibble::tibble())
  cor_lv <- tryCatch(lavaan::lavInspect(res$fit, "cor.lv"), error = function(e) NULL)
  if (is.null(cor_lv)) return(tibble::tibble())
  as.data.frame(as.table(cor_lv), stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::rename(factor_1 = Var1, factor_2 = Var2, correlation = Freq) |>
    dplyr::filter(as.character(factor_1) < as.character(factor_2)) |>
    dplyr::mutate(model = res$name, .before = 1)
}))

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
      "CFA is applicable; model adequacy still depends on convergence, admissibility, fit, and theoretically coherent loadings.",
      "CFA prerequisites are not fully met; inspect the failed criteria before interpreting model fit."
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
for (nm in names(results)) {
  res <- results[[nm]]
  outfile <- file.path(MODEL_DIR, paste0("cfa_", nm, "_summary.txt"))
  if (is.null(res$fit)) {
    writeLines(c("MODEL FAILED", paste0("Error: ", res$error), res$warnings), outfile)
  } else {
    capture.output(
      summary(res$fit, fit.measures = res$converged, standardized = TRUE),
      file = outfile
    )
  }
}

saveRDS(
  lapply(results, `[[`, "fit"),
  file.path(MODEL_DIR, "cfa_fitted_models.rds")
)

# Continue the pipeline even if the optional second-order model is inadmissible.
if (!results$attitudes$converged) warning("The GAAIS CFA did not converge; inspect cfa_results.xlsx and the model summary.")
if (!results$competence_4f$converged) warning("The four-factor AI literacy CFA did not converge; inspect cfa_results.xlsx and the model summary.")
if (!results$competence_second$admissible) message("The optional second-order AI competence model was not admissible and should not be interpreted.")
