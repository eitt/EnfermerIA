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

fit_att <- lavaan::cfa(
  att_model, data = analysis_data, ordered = att_ordered,
  estimator = "WLSMV", std.lv = TRUE, missing = "pairwise"
)
fit_comp_4f <- lavaan::cfa(
  comp_model_4f, data = analysis_data, ordered = comp_ordered,
  estimator = "WLSMV", std.lv = TRUE, missing = "pairwise"
)
fit_comp_second <- lavaan::cfa(
  comp_model_second_order, data = analysis_data, ordered = comp_ordered,
  estimator = "WLSMV", std.lv = TRUE, missing = "pairwise"
)

extract_fit <- function(fit, model_name) {
  requested <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                 "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")
  vals <- lavaan::fitMeasures(fit)
  tibble::tibble(
    model = model_name,
    measure = requested,
    value = unname(vals[requested])
  )
}

fit_summary <- dplyr::bind_rows(
  extract_fit(fit_att, "GAAIS: correlated Positive and NegativeFavorable factors"),
  extract_fit(fit_comp_4f, "AI Literacy: four correlated factors"),
  extract_fit(fit_comp_second, "AI Literacy: second-order competence factor")
)

loading_table <- function(fit, model_name) {
  lavaan::standardizedSolution(fit) |>
    dplyr::filter(op == "=~") |>
    dplyr::transmute(model = model_name, factor = lhs, item = rhs,
                     standardized_loading = est.std, se = se, z = z, p = pvalue)
}
loadings <- dplyr::bind_rows(
  loading_table(fit_att, "GAAIS two-factor"),
  loading_table(fit_comp_4f, "AI Literacy four-factor"),
  loading_table(fit_comp_second, "AI Literacy second-order")
)

# Composite reliability and AVE, with graceful handling across semTools versions.
reliability_cfa <- function(fit, model_name) {
  rel <- tryCatch(semTools::compRelSEM(fit), error = function(e) NULL)
  ave <- tryCatch(semTools::AVE(fit), error = function(e) NULL)
  factors <- union(names(rel), names(ave))
  tibble::tibble(
    model = model_name,
    factor = factors,
    composite_reliability = if (is.null(rel)) NA_real_ else as.numeric(rel[factors]),
    ave = if (is.null(ave)) NA_real_ else as.numeric(ave[factors])
  )
}
validity <- dplyr::bind_rows(
  reliability_cfa(fit_att, "GAAIS two-factor"),
  reliability_cfa(fit_comp_4f, "AI Literacy four-factor"),
  reliability_cfa(fit_comp_second, "AI Literacy second-order")
)

# Decision-oriented CFA applicability table.
fit_wide <- fit_summary |> tidyr::pivot_wider(names_from = measure, values_from = value)
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
      "CFA is applicable. Use ordinal WLSMV, inspect fit, loadings, residuals, and theory jointly.",
      "CFA may be unstable or poorly identified; inspect failed criteria before interpreting fit."
    )
  )

write_workbook_safely(
  list(
    Fit_indices = fit_summary,
    Standardized_loadings = loadings,
    Reliability_AVE = validity,
    CFA_applicability = applicability
  ),
  file.path(TABLE_DIR, "cfa_results.xlsx")
)

capture.output(summary(fit_att, fit.measures = TRUE, standardized = TRUE),
               file = file.path(MODEL_DIR, "cfa_gaais_summary.txt"))
capture.output(summary(fit_comp_4f, fit.measures = TRUE, standardized = TRUE),
               file = file.path(MODEL_DIR, "cfa_ai_literacy_4factor_summary.txt"))
capture.output(summary(fit_comp_second, fit.measures = TRUE, standardized = TRUE),
               file = file.path(MODEL_DIR, "cfa_ai_literacy_second_order_summary.txt"))
saveRDS(list(attitudes = fit_att, competence_4factor = fit_comp_4f,
             competence_second_order = fit_comp_second),
        file.path(MODEL_DIR, "cfa_fitted_models.rds"))
