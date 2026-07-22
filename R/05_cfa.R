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
    value = unname(vals[requested])
  )
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
      "CFA is applicable; model adequacy still depends on convergence, admissibility, fit, and theoretically coherent loadings.",
      "CFA prerequisites are not fully met; inspect the failed criteria before interpreting model fit."
    )
  )

write_workbook_safely(
  list(
    Model_status = model_status,
    Fit_indices = fit_summary,
    Standardized_loadings = loadings,
    Latent_correlations = latent_correlations,
    Reliability_AVE = validity,
    CFA_applicability = applicability
  ),
  file.path(TABLE_DIR, "cfa_results.xlsx")
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
