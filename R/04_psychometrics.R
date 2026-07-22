# Reliability, factorability, and item analysis -------------------------------

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
  x <- data |> dplyr::select(dplyr::all_of(items)) |> tidyr::drop_na()
  poly <- psych::polychoric(x)$rho
  kmo <- psych::KMO(poly)
  bart <- psych::cortest.bartlett(poly, n = nrow(x))
  tibble::tibble(
    scale = scale_name, n_complete = nrow(x), items = length(items),
    kmo_overall = unname(kmo$MSA),
    bartlett_chisq = unname(bart$chisq), bartlett_df = unname(bart$df),
    bartlett_p = unname(bart$p.value),
    min_item_variance = min(vapply(x, stats::var, numeric(1), na.rm = TRUE)),
    max_absolute_item_correlation = max(abs(poly[upper.tri(poly)]), na.rm = TRUE)
  )
}

factorability_summary <- dplyr::bind_rows(
  factorability(analysis_data, att_all_scored, "GAAIS two-factor item set"),
  factorability(analysis_data, COMP_ALL_ITEMS, "AI Literacy four-factor item set")
)

# Parallel analysis is diagnostic, not a replacement for the source-specified CFA.
png(file.path(FIGURE_DIR, "parallel_analysis_attitudes.png"), width = 10, height = 7, units = "in", res = FIG_DPI)
psych::fa.parallel(analysis_data[, att_all_scored], fa = "fa", cor = "poly", plot = TRUE,
                   main = "Parallel analysis: GAAIS items")
dev.off()

png(file.path(FIGURE_DIR, "parallel_analysis_competence.png"), width = 10, height = 7, units = "in", res = FIG_DPI)
psych::fa.parallel(analysis_data[, COMP_ALL_ITEMS], fa = "fa", cor = "poly", plot = TRUE,
                   main = "Parallel analysis: AI Literacy items")
dev.off()

# Inter-scale correlations with confidence-compatible sample sizes.
score_vars <- c(
  "attitude_positive", "attitude_negative_favorable", "attitude_favorability_index",
  "competence_awareness", "competence_usage", "competence_evaluation",
  "competence_ethics", "competence_total"
)
score_cor <- psych::corr.test(analysis_data[, score_vars], use = "pairwise", method = "pearson", adjust = "holm")
correlation_table <- as.data.frame(as.table(score_cor$r)) |>
  dplyr::rename(variable_1 = Var1, variable_2 = Var2, r = Freq) |>
  dplyr::mutate(p_holm = as.vector(score_cor$p), n_pairwise = as.vector(score_cor$n))

write_workbook_safely(
  list(
    Reliability = reliability_summary,
    Factorability = factorability_summary,
    Score_correlations = correlation_table
  ),
  file.path(TABLE_DIR, "psychometric_diagnostics.xlsx")
)
