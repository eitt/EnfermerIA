# Import, recode, and score ----------------------------------------------------

if (!file.exists(DATA_FILE)) {
  stop("Data file not found at project root: ", DATA_FILE)
}

raw_data <- readxl::read_excel(DATA_FILE, sheet = DATA_SHEET, .name_repair = "unique")
raw_data <- raw_data |> dplyr::rename_with(~ stringr::str_trim(.x))

# Resolve duplicated ac_total columns created by Excel/readxl name repair.
# Keep the non-empty observed score and discard empty duplicate columns.
ac_candidates <- grep("^ac_total(\\.\\.\\.[0-9]+)?$", names(raw_data), value = TRUE)
if (length(ac_candidates) > 0) {
  nonempty_ac <- ac_candidates[!vapply(raw_data[ac_candidates], function(x) all(is.na(x)), logical(1))]
  if (length(nonempty_ac) == 1) {
    raw_data <- raw_data |>
      dplyr::rename(ac_total = dplyr::all_of(nonempty_ac)) |>
      dplyr::select(-dplyr::any_of(setdiff(ac_candidates, nonempty_ac)))
  } else if (length(nonempty_ac) > 1) {
    stop("More than one non-empty ac_total column was found: ", paste(nonempty_ac, collapse = ", "))
  } else {
    raw_data <- raw_data |> dplyr::select(-dplyr::all_of(ac_candidates))
  }
}

# Harmonize age naming found in the data dictionary versus the data frame.
if ("age" %in% names(raw_data) && !"age_years" %in% names(raw_data)) {
  raw_data <- raw_data |> dplyr::rename(age_years = age)
}

# Remove columns that are entirely missing, such as duplicated empty ac_total.1.
empty_columns <- names(raw_data)[vapply(raw_data, function(x) all(is.na(x)), logical(1))]
if (length(empty_columns) > 0) {
  writeLines(empty_columns, file.path(LOG_DIR, "removed_all_missing_columns.txt"))
  raw_data <- raw_data |> dplyr::select(-dplyr::all_of(empty_columns))
}

# Reconstruct raw wording variables when only reverse-scored versions exist.
# This preserves both original-response and analytically scored versions.
reverse_pairs <- c(
  competence_ET_2_r = "competence_ET_2_raw",
  competence_US_3_r = "competence_US_3_raw",
  competence_AW_8_r = "competence_AW_8_raw"
)
for (scored_name in names(reverse_pairs)) {
  raw_name <- unname(reverse_pairs[[scored_name]])
  if (scored_name %in% names(raw_data) && !raw_name %in% names(raw_data)) {
    raw_data[[raw_name]] <- reverse_1_to_5(raw_data[[scored_name]])
  }
}

# If unscored source variables are supplied in a future dataset, create _r versions.
forward_pairs <- c(
  competence_ET_2 = "competence_ET_2_r",
  competence_US_3 = "competence_US_3_r",
  competence_AW_8 = "competence_AW_8_r"
)
for (raw_name in names(forward_pairs)) {
  scored_name <- unname(forward_pairs[[raw_name]])
  if (raw_name %in% names(raw_data) && !scored_name %in% names(raw_data)) {
    raw_data[[scored_name]] <- reverse_1_to_5(raw_data[[raw_name]])
  }
}

check_required_columns(raw_data, c(ATT_POS_ITEMS, ATT_NEG_ITEMS, COMP_ALL_ITEMS), "scale scoring")

# Recoded labels for descriptive output. Numeric source variables remain unchanged.
data_labeled <- raw_data |>
  dplyr::mutate(
    consent_label = factor(consent, levels = c(0, 1), labels = c("No", "Yes")),
    gender_label = factor(gender, levels = c(0, 1, 2, 3),
                          labels = c("Prefer not to answer", "Woman", "Man", "Non-binary")),
    occupation_label = factor(occupation, levels = 0:4,
      labels = c("Other", "University student", "University professor",
                 "Technician or technologist", "Healthcare professional")),
    study_work_area_label = factor(study_work_area, levels = 0:8,
      labels = c("Other", "Psychology", "Nursing", "Medicine",
                 "Other healthcare professions", "Education", "Engineering",
                 "Law, social sciences & humanities", "Business & commerce")),
    commitment_label = factor(commitment, levels = 0:2,
      labels = c("No, I do not commit", "Yes, I commit", "I cannot make that commitment")),
    across(dplyr::all_of(BINARY_TECH_VARS), ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes")),
           .names = "{.col}_label")
  )

# Analysis sample: keep exclusions transparent and export the flow.
analysis_flag <- rep(TRUE, nrow(raw_data))
if (REQUIRE_CONSENT && "consent" %in% names(raw_data)) analysis_flag <- analysis_flag & raw_data$consent == 1
if (REQUIRE_COMMITMENT && "commitment" %in% names(raw_data)) analysis_flag <- analysis_flag & raw_data$commitment == 1
if ("ac_total" %in% names(raw_data)) analysis_flag <- analysis_flag & raw_data$ac_total >= MIN_ATTENTION_TOTAL

analysis_data <- raw_data[analysis_flag, , drop = FALSE]

# GAAIS: source recommends separate Positive and Negative subscales.
# Negative items are reversed so higher values represent a more favorable attitude.
analysis_data <- analysis_data |>
  dplyr::mutate(
    across(dplyr::all_of(ATT_NEG_ITEMS), reverse_1_to_5, .names = "{.col}_r"),
    attitude_positive = rowMeans(dplyr::pick(dplyr::all_of(ATT_POS_ITEMS)), na.rm = TRUE),
    attitude_negative_favorable = rowMeans(
      dplyr::pick(dplyr::all_of(paste0(ATT_NEG_ITEMS, "_r"))), na.rm = TRUE
    ),
    # Visualization-only global favorability index. It is not the primary GAAIS score.
    attitude_favorability_index = rowMeans(
      dplyr::pick(dplyr::all_of(c(ATT_POS_ITEMS, paste0(ATT_NEG_ITEMS, "_r")))), na.rm = TRUE
    ),
    competence_awareness = rowMeans(dplyr::pick(dplyr::all_of(COMP_AWARENESS)), na.rm = TRUE),
    competence_usage = rowMeans(dplyr::pick(dplyr::all_of(COMP_USAGE)), na.rm = TRUE),
    competence_evaluation = rowMeans(dplyr::pick(dplyr::all_of(COMP_EVALUATION)), na.rm = TRUE),
    competence_ethics = rowMeans(dplyr::pick(dplyr::all_of(COMP_ETHICS)), na.rm = TRUE),
    competence_total = rowMeans(dplyr::pick(dplyr::all_of(COMP_ALL_ITEMS)), na.rm = TRUE)
  )

sample_flow <- tibble::tibble(
  stage = c("Imported", "Consent criterion", "Commitment criterion", "Attention-check criterion", "Final analysis sample"),
  rule = c(
    "All rows in source file",
    ifelse(REQUIRE_CONSENT, "consent == 1", "Not applied"),
    ifelse(REQUIRE_COMMITMENT, "commitment == 1", "Not applied"),
    paste0("ac_total >= ", MIN_ATTENTION_TOTAL),
    "All active criteria"
  ),
  n = c(
    nrow(raw_data),
    if (REQUIRE_CONSENT) sum(raw_data$consent == 1, na.rm = TRUE) else nrow(raw_data),
    if (REQUIRE_COMMITMENT) sum(raw_data$commitment == 1, na.rm = TRUE) else nrow(raw_data),
    if ("ac_total" %in% names(raw_data)) sum(raw_data$ac_total >= MIN_ATTENTION_TOTAL, na.rm = TRUE) else nrow(raw_data),
    nrow(analysis_data)
  )
)

openxlsx::write.xlsx(sample_flow, file.path(TABLE_DIR, "sample_flow.xlsx"), overwrite = TRUE)
saveRDS(analysis_data, file.path(OUTPUT_DIR, "analysis_data_scored.rds"))
openxlsx::write.xlsx(analysis_data, file.path(OUTPUT_DIR, "analysis_data_scored.xlsx"), overwrite = TRUE)
