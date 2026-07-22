# Reliability, factorability, and correlation diagnostics ---------------------

att_neg_scored <- paste0(ATT_NEG_ITEMS, "_r")
att_all_scored <- c(ATT_POS_ITEMS, att_neg_scored)

reliability_summary <- dplyr::bind_rows(
  reliability_row(analysis_data, ATT_POS_ITEMS, "GAAIS Positive"),
  reliability_row(analysis_data, att_neg_scored, "GAAIS Negative, reverse-scored/favorable"),
  reliability_row(analysis_data, COMP_AWARENESS, "AI Literacy: Awareness"),
  reliability_row(analysis_data, COMP_USAGE, "AI Literacy: Usage"),
  reliability_row(analysis_data, COMP_EVALUATION, "AI Literacy: Evaluation"),
  reliability_row(analysis_data, COMP_ETHICS, "AI Literacy: Ethics"),
  reliability_row(analysis_data, COMP_ALL_ITEMS, "AI Literacy: Total")
)

factorability <- function(data, items, scale_name) {
  x <- data |> dplyr::select(dplyr::all_of(items))
  complete_n <- sum(stats::complete.cases(x))
  poly_result <- safe_polychoric(data, items)
  poly <- poly_result$rho

  if (nrow(poly) < 2) {
    return(tibble::tibble(
      scale = scale_name, n_complete = complete_n, items = length(items),
      usable_items = length(poly_result$usable), removed_zero_variance_items = paste(poly_result$removed, collapse = ", "),
      kmo_overall = NA_real_, bartlett_chisq = NA_real_, bartlett_df = NA_real_,
      bartlett_p = NA_real_, min_item_variance = NA_real_, max_absolute_item_correlation = NA_real_
    ))
  }

  kmo <- psych::KMO(poly)
  bart <- psych::cortest.bartlett(poly, n = complete_n)
  tibble::tibble(
    scale = scale_name,
    n_complete = complete_n,
    items = length(items),
    usable_items = length(poly_result$usable),
    removed_zero_variance_items = paste(poly_result$removed, collapse = ", "),
    kmo_overall = unname(kmo$MSA),
    bartlett_chisq = unname(bart$chisq),
    bartlett_df = unname(bart$df),
    bartlett_p = unname(bart$p.value),
    min_item_variance = min(vapply(x, stats::var, numeric(1), na.rm = TRUE)),
    max_absolute_item_correlation = max(abs(poly[upper.tri(poly)]), na.rm = TRUE)
  )
}

factorability_summary <- dplyr::bind_rows(
  factorability(analysis_data, att_all_scored, "GAAIS two-factor item set"),
  factorability(analysis_data, COMP_ALL_ITEMS, "AI Literacy four-factor item set")
)

# Parallel analysis is diagnostic, not a replacement for source-specified CFA.
png(file.path(FIGURE_DIR, "parallel_analysis_attitudes.png"), width = 10, height = 7, units = "in", res = FIG_DPI)
suppressMessages(psych::fa.parallel(analysis_data[, att_all_scored], fa = "fa", cor = "poly", plot = TRUE,
                                    main = "Parallel analysis: GAAIS items"))
dev.off()

png(file.path(FIGURE_DIR, "parallel_analysis_competence.png"), width = 10, height = 7, units = "in", res = FIG_DPI)
suppressMessages(psych::fa.parallel(analysis_data[, COMP_ALL_ITEMS], fa = "fa", cor = "poly", plot = TRUE,
                                    main = "Parallel analysis: AI Literacy items"))
dev.off()

# Correlation matrices of observed ordinal items (polychoric correlations).
att_item_cor_result <- safe_polychoric(analysis_data, att_all_scored)
comp_item_cor_result <- safe_polychoric(analysis_data, COMP_ALL_ITEMS)
att_item_cor <- att_item_cor_result$rho
comp_item_cor <- comp_item_cor_result$rho

if (length(att_item_cor_result$removed) > 0) {
  append_audit_log("correlation exclusion", "GAAIS observed items",
                   paste0("Excluded zero-variance items: ", paste(att_item_cor_result$removed, collapse = ", ")),
                   columns_affected = length(att_item_cor_result$removed))
}
if (length(comp_item_cor_result$removed) > 0) {
  append_audit_log("correlation exclusion", "AI competence observed items",
                   paste0("Excluded zero-variance items: ", paste(comp_item_cor_result$removed, collapse = ", ")),
                   columns_affected = length(comp_item_cor_result$removed))
}

# Correlation matrix of factor/subscale scores (Pearson correlations).
factor_score_vars <- c(
  "attitude_positive", "attitude_negative_favorable", "attitude_favorability_index",
  "competence_awareness", "competence_usage", "competence_evaluation",
  "competence_ethics", "competence_total"
)
factor_score_data <- analysis_data |> dplyr::select(dplyr::all_of(factor_score_vars))
zero_variance_scores <- names(factor_score_data)[vapply(factor_score_data, function(x) {
  stats::var(x, na.rm = TRUE) == 0 || all(is.na(x))
}, logical(1))]
usable_factor_scores <- setdiff(factor_score_vars, zero_variance_scores)
score_cor <- psych::corr.test(
  factor_score_data[, usable_factor_scores, drop = FALSE],
  use = "pairwise", method = "pearson", adjust = "holm"
)
factor_cor_matrix <- score_cor$r
factor_p_matrix <- score_cor$p
factor_n_matrix <- score_cor$n

if (length(zero_variance_scores) > 0) {
  append_audit_log("correlation exclusion", "factor scores",
                   paste0("Excluded zero-variance scores: ", paste(zero_variance_scores, collapse = ", ")),
                   columns_affected = length(zero_variance_scores))
}

correlation_table <- as.data.frame(as.table(factor_cor_matrix)) |>
  dplyr::rename(variable_1 = Var1, variable_2 = Var2, r = Freq) |>
  dplyr::mutate(
    p_holm = as.vector(factor_p_matrix),
    n_pairwise = as.vector(factor_n_matrix)
  )

# Export correlation heatmaps at 300 dpi.
plot_corr_matrix <- function(mat, title, filename, label_size = 0.6) {
  if (nrow(mat) < 2) return(invisible(NULL))
  grDevices::png(file.path(FIGURE_DIR, filename), width = 10, height = 9, units = "in", res = FIG_DPI)
  corrplot::corrplot(
    mat,
    method = "color",
    type = "upper",
    order = "original",
    addCoef.col = "black",
    number.cex = label_size,
    tl.col = "black",
    tl.srt = 45,
    col = grDevices::colorRampPalette(c(
      PALETTE_OKABE_ITO[["vermillion"]], "white", PALETTE_OKABE_ITO[["blue"]]
    ))(200),
    mar = c(0, 0, 3, 0),
    title = title
  )
  grDevices::dev.off()
}

plot_corr_matrix(att_item_cor, "Observed-item correlations: attitudes toward AI",
                 "correlation_observed_attitude_items.png", label_size = 0.45)
plot_corr_matrix(comp_item_cor, "Observed-item correlations: AI competence",
                 "correlation_observed_competence_items.png", label_size = 0.55)
plot_corr_matrix(factor_cor_matrix, "Correlations among scale and factor scores",
                 "correlation_factor_scores.png", label_size = 0.7)

append_audit_log("correlation analysis", "observed items and factor scores",
                 "Generated polychoric item matrices and Pearson factor-score matrix with pairwise sample sizes")

write_workbook_safely(
  list(
    Reliability = reliability_summary,
    Factorability = factorability_summary,
    Observed_attitude_items = matrix_to_table(att_item_cor),
    Observed_competence_items = matrix_to_table(comp_item_cor),
    Factor_score_correlations = matrix_to_table(factor_cor_matrix),
    Factor_score_p_Holm = matrix_to_table(factor_p_matrix),
    Factor_score_pairwise_n = matrix_to_table(factor_n_matrix),
    Factor_correlations_long = correlation_table
  ),
  file.path(TABLE_DIR, "psychometric_diagnostics.xlsx")
)