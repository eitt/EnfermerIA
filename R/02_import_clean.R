# Import, validation, scoring, and sample construction ------------------------

if (!file.exists(DATA_FILE)) {
  stop(
    "Data file not found at project root: ",
    DATA_FILE
  )
}

# Start a fresh transformation log.
audit_path <- file.path(
  LOG_DIR,
  "transformation_audit_log.csv"
)

if (file.exists(audit_path)) {
  file.remove(audit_path)
}

# ---------------------------------------------------------------------------
# Import
# ---------------------------------------------------------------------------

raw_data <- readxl::read_excel(
  DATA_FILE,
  sheet = DATA_SHEET,
  .name_repair = "unique"
)

raw_data <- raw_data |>
  dplyr::rename_with(
    ~ stringr::str_trim(.x)
  )

# Excel row 1 contains headers, so the first participant is on row 2.
raw_data$source_row <- seq_len(
  nrow(raw_data)
) + 1L

append_audit_log(
  action = "Import",
  object = DATA_FILE,
  detail = paste0(
    "Imported Excel sheet ",
    DATA_SHEET,
    ". source_row corresponds to the original Excel row."
  ),
  rows_affected = nrow(raw_data),
  columns_affected = ncol(raw_data)
)

# ---------------------------------------------------------------------------
# Resolve duplicated ac_total columns
# ---------------------------------------------------------------------------

ac_candidates <- grep(
  "^ac_total(\\.\\.\\.[0-9]+|\\.[0-9]+)?$",
  names(raw_data),
  value = TRUE
)

if (length(ac_candidates) > 0) {
  
  ac_nonmissing <- vapply(
    raw_data[ac_candidates],
    function(x) {
      sum(
        !is.na(x)
      )
    },
    numeric(1)
  )
  
  keep_ac <- ac_candidates[
    which.max(ac_nonmissing)
  ]
  
  # When the selected column is not already named ac_total, first remove any
  # existing ac_total column and then rename the selected candidate.
  if (keep_ac != "ac_total") {
    
    if ("ac_total" %in% names(raw_data)) {
      raw_data <- raw_data |>
        dplyr::select(
          -dplyr::all_of("ac_total")
        )
    }
    
    raw_data <- raw_data |>
      dplyr::rename(
        ac_total = dplyr::all_of(keep_ac)
      )
  }
  
  drop_ac <- setdiff(
    ac_candidates,
    keep_ac
  )
  
  drop_ac <- intersect(
    drop_ac,
    names(raw_data)
  )
  
  if (length(drop_ac) > 0) {
    raw_data <- raw_data |>
      dplyr::select(
        -dplyr::all_of(drop_ac)
      )
  }
  
  append_audit_log(
    action = "Duplicate-column resolution",
    object = "ac_total",
    detail = paste0(
      "Retained ",
      keep_ac,
      " because it had the largest non-missing count. Removed: ",
      ifelse(
        length(drop_ac) == 0,
        "none",
        paste(
          drop_ac,
          collapse = ", "
        )
      )
    ),
    columns_affected = length(drop_ac)
  )
}

# ---------------------------------------------------------------------------
# Harmonize variable names
# ---------------------------------------------------------------------------

if (
  "age" %in% names(raw_data) &&
  !"age_years" %in% names(raw_data)
) {
  
  raw_data <- raw_data |>
    dplyr::rename(
      age_years = age
    )
  
  append_audit_log(
    action = "Rename",
    object = "age",
    detail = "Renamed age to age_years.",
    columns_affected = 1
  )
}

# Support legacy files containing only competence names ending in _r.
#
# This step copies the stored values into the raw-name column only when the raw
# column is absent. No reverse scoring occurs here because the direction of the
# legacy field must not be silently assumed.

legacy_competence_map <- c(
  competence_AW_8_r = "competence_AW_8",
  competence_US_3_r = "competence_US_3",
  competence_ET_2_r = "competence_ET_2"
)

for (legacy_name in names(legacy_competence_map)) {
  
  raw_name <- unname(
    legacy_competence_map[[legacy_name]]
  )
  
  if (
    legacy_name %in% names(raw_data) &&
    !raw_name %in% names(raw_data)
  ) {
    
    raw_data[[raw_name]] <- suppressWarnings(
      as.numeric(
        as.character(
          raw_data[[legacy_name]]
        )
      )
    )
    
    append_audit_log(
      action = "Legacy-name recovery",
      object = raw_name,
      detail = paste0(
        "Created raw-name column ",
        raw_name,
        " from legacy column ",
        legacy_name,
        ". No reversal was applied at this stage."
      ),
      rows_affected = sum(
        !is.na(
          raw_data[[legacy_name]]
        )
      ),
      columns_affected = 1
    )
  }
}

# ---------------------------------------------------------------------------
# Remove columns that are entirely missing
# ---------------------------------------------------------------------------

empty_columns <- names(raw_data)[
  vapply(
    raw_data,
    function(x) {
      all(
        is.na(x)
      )
    },
    logical(1)
  )
]

if (length(empty_columns) > 0) {
  
  raw_data <- raw_data |>
    dplyr::select(
      -dplyr::all_of(empty_columns)
    )
  
  append_audit_log(
    action = "Remove columns",
    object = "All-missing columns",
    detail = paste(
      empty_columns,
      collapse = ", "
    ),
    columns_affected = length(empty_columns)
  )
}

# ---------------------------------------------------------------------------
# Check required variables
# ---------------------------------------------------------------------------

required_source_items <- unique(
  c(
    ATT_POS_ITEMS,
    ATT_NEG_ITEMS,
    COMP_RAW_ITEMS,
    ATTENTION_ITEMS
  )
)

check_required_columns(
  raw_data,
  required_source_items,
  "source-item validation"
)

# ---------------------------------------------------------------------------
# Convert scale and attention variables to numeric
# ---------------------------------------------------------------------------

numeric_item_variables <- unique(
  c(
    ATT_POS_ITEMS,
    ATT_NEG_ITEMS,
    COMP_RAW_ITEMS,
    ATTENTION_ITEMS,
    "consent",
    "commitment",
    "ac_competence",
    "ac_att",
    "ac_total"
  )
)

numeric_item_variables <- intersect(
  numeric_item_variables,
  names(raw_data)
)

conversion_log <- purrr::map_dfr(
  numeric_item_variables,
  function(variable) {
    
    original <- raw_data[[variable]]
    
    converted <- suppressWarnings(
      as.numeric(
        as.character(
          original
        )
      )
    )
    
    introduced_na <- (
      !is.na(original) &
        is.na(converted)
    )
    
    raw_data[[variable]] <<- converted
    
    tibble::tibble(
      variable = variable,
      nonmissing_before = sum(
        !is.na(original)
      ),
      nonmissing_after = sum(
        !is.na(converted)
      ),
      na_introduced_by_numeric_conversion = sum(
        introduced_na
      )
    )
  }
)

numeric_conversion_na_total <- sum(
  conversion_log$na_introduced_by_numeric_conversion
)

append_audit_log(
  action = "Numeric conversion",
  object = "Scale and attention variables",
  detail = paste0(
    "Converted declared numeric variables. New NA values introduced: ",
    numeric_conversion_na_total,
    ". See numeric_conversion_log.xlsx."
  ),
  rows_affected = numeric_conversion_na_total,
  columns_affected = nrow(conversion_log)
)

write_workbook_safely(
  list(
    Numeric_conversion = conversion_log
  ),
  file.path(
    TABLE_DIR,
    "numeric_conversion_log.xlsx"
  )
)

message(
  "\nNumeric conversion log"
)

print(
  conversion_log,
  n = Inf
)

# ---------------------------------------------------------------------------
# Validate ranges
# ---------------------------------------------------------------------------
# Scale indicators and the four individual attention checks must be between
# 1 and 5. Aggregate attention scores are not included because their valid
# ranges are 0-2 or 0-4.

likert_variables <- unique(
  c(
    ATT_POS_ITEMS,
    ATT_NEG_ITEMS,
    COMP_RAW_ITEMS,
    ATTENTION_ITEMS
  )
)

invalid_value_detail <- purrr::map_dfr(
  likert_variables,
  function(variable) {
    
    values <- suppressWarnings(
      as.numeric(
        raw_data[[variable]]
      )
    )
    
    invalid <- (
      !is.na(values) &
        !(values %in% 1:5)
    )
    
    if (!any(invalid)) {
      return(
        tibble::tibble(
          variable = character(0),
          source_row = integer(0),
          original_value = numeric(0),
          replacement = numeric(0)
        )
      )
    }
    
    invalid_rows <- raw_data$source_row[
      invalid
    ]
    
    original_invalid_values <- values[
      invalid
    ]
    
    raw_data[[variable]][invalid] <<- NA_real_
    
    tibble::tibble(
      variable = variable,
      source_row = invalid_rows,
      original_value = original_invalid_values,
      replacement = NA_real_
    )
  }
)

if (nrow(invalid_value_detail) > 0) {
  
  append_audit_log(
    action = "Invalid-value recoding",
    object = "Likert and attention-check items",
    detail = paste0(
      nrow(invalid_value_detail),
      " cells outside the valid 1-5 range were converted to NA. ",
      "Every affected row and original value is stored in ",
      "invalid_value_detail.xlsx."
    ),
    rows_affected = nrow(invalid_value_detail),
    columns_affected = dplyr::n_distinct(
      invalid_value_detail$variable
    )
  )
  
} else {
  
  append_audit_log(
    action = "Invalid-value validation",
    object = "Likert and attention-check items",
    detail = "No values outside the valid 1-5 range were detected.",
    rows_affected = 0,
    columns_affected = length(likert_variables)
  )
}

write_workbook_safely(
  list(
    Invalid_values = invalid_value_detail
  ),
  file.path(
    TABLE_DIR,
    "invalid_value_detail.xlsx"
  )
)

message(
  "\nInvalid-value validation"
)

message(
  "Cells converted to NA because they were outside 1-5: ",
  nrow(invalid_value_detail)
)

if (nrow(invalid_value_detail) > 0) {
  print(
    invalid_value_detail,
    n = Inf
  )
}

# ---------------------------------------------------------------------------
# Descriptive values before reverse scoring
# ---------------------------------------------------------------------------

describe_items <- function(
    data,
    variables,
    stage
) {
  
  purrr::map_dfr(
    variables,
    function(variable) {
      
      x <- suppressWarnings(
        as.numeric(
          data[[variable]]
        )
      )
      
      all_missing <- all(
        is.na(x)
      )
      
      nonmissing_n <- sum(
        !is.na(x)
      )
      
      tibble::tibble(
        stage = stage,
        variable = variable,
        n = nonmissing_n,
        missing = sum(
          is.na(x)
        ),
        missing_pct = mean(
          is.na(x)
        ),
        mean = if (
          all_missing
        ) {
          NA_real_
        } else {
          mean(
            x,
            na.rm = TRUE
          )
        },
        sd = if (
          nonmissing_n < 2
        ) {
          NA_real_
        } else {
          stats::sd(
            x,
            na.rm = TRUE
          )
        },
        median = if (
          all_missing
        ) {
          NA_real_
        } else {
          stats::median(
            x,
            na.rm = TRUE
          )
        },
        min = if (
          all_missing
        ) {
          NA_real_
        } else {
          min(
            x,
            na.rm = TRUE
          )
        },
        max = if (
          all_missing
        ) {
          NA_real_
        } else {
          max(
            x,
            na.rm = TRUE
          )
        }
      )
    }
  )
}

descriptives_before_reverse <- describe_items(
  raw_data,
  c(
    ATT_NEG_ITEMS,
    names(COMP_REVERSE_MAP)
  ),
  "Before reverse scoring"
)

# ---------------------------------------------------------------------------
# Reverse-score declared competence indicators
# ---------------------------------------------------------------------------

for (raw_name in names(COMP_REVERSE_MAP)) {
  
  scored_name <- unname(
    COMP_REVERSE_MAP[[raw_name]]
  )
  
  raw_data[[scored_name]] <- reverse_1_to_5(
    raw_data[[raw_name]]
  )
  
  append_audit_log(
    action = "Reverse scoring",
    object = scored_name,
    detail = paste0(
      scored_name,
      " = 6 - ",
      raw_name,
      "."
    ),
    rows_affected = sum(
      !is.na(
        raw_data[[raw_name]]
      )
    ),
    columns_affected = 1
  )
}

# ---------------------------------------------------------------------------
# Reverse-score the GAAIS negative-concern items
# ---------------------------------------------------------------------------

raw_data <- raw_data |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(ATT_NEG_ITEMS),
      reverse_1_to_5,
      .names = "{.col}_r"
    )
  )

append_audit_log(
  action = "Reverse scoring",
  object = "GAAIS negative items",
  detail = paste0(
    "Created favorable-direction versions: ",
    paste(
      ATT_NEG_SCORED,
      collapse = ", "
    ),
    ". Original negative-concern variables were retained."
  ),
  rows_affected = nrow(raw_data),
  columns_affected = length(ATT_NEG_ITEMS)
)

descriptives_after_reverse <- describe_items(
  raw_data,
  c(
    ATT_NEG_SCORED,
    unname(COMP_REVERSE_MAP)
  ),
  "After reverse scoring"
)

reverse_score_comparison <- dplyr::bind_rows(
  descriptives_before_reverse,
  descriptives_after_reverse
)

write_workbook_safely(
  list(
    Before_and_after = reverse_score_comparison
  ),
  file.path(
    TABLE_DIR,
    "reverse_scoring_descriptives.xlsx"
  )
)

message(
  "\nReverse-scoring descriptive comparison"
)

print(
  reverse_score_comparison,
  n = Inf
)

# ---------------------------------------------------------------------------
# Recalculate attention checks from item responses
# ---------------------------------------------------------------------------

raw_data <- raw_data |>
  dplyr::mutate(
    ac_1_correct = (
      !is.na(ac_1) &
        ac_1 == ATTENTION_CORRECT_RESPONSES[["ac_1"]]
    ),
    ac_2_correct = (
      !is.na(ac_2) &
        ac_2 == ATTENTION_CORRECT_RESPONSES[["ac_2"]]
    ),
    ac_3_correct = (
      !is.na(ac_3) &
        ac_3 == ATTENTION_CORRECT_RESPONSES[["ac_3"]]
    ),
    ac_4_correct = (
      !is.na(ac_4) &
        ac_4 == ATTENTION_CORRECT_RESPONSES[["ac_4"]]
    ),
    ac_competence_recalculated =
      as.integer(ac_1_correct) +
      as.integer(ac_2_correct),
    ac_att_recalculated =
      as.integer(ac_3_correct) +
      as.integer(ac_4_correct),
    ac_total_recalculated =
      ac_competence_recalculated +
      ac_att_recalculated,
    ac_failed_recalculated =
      4L -
      ac_total_recalculated
  )

# Prepare optional columns outside dplyr verbs. This avoids combining a
# one-element condition with a full-length vector inside dplyr::if_else().

id_values <- if (
  "id" %in% names(raw_data)
) {
  as.character(
    raw_data$id
  )
} else {
  rep(
    NA_character_,
    nrow(raw_data)
  )
}

ac_competence_existing_values <- if (
  "ac_competence" %in% names(raw_data)
) {
  suppressWarnings(
    as.numeric(
      raw_data$ac_competence
    )
  )
} else {
  rep(
    NA_real_,
    nrow(raw_data)
  )
}

ac_att_existing_values <- if (
  "ac_att" %in% names(raw_data)
) {
  suppressWarnings(
    as.numeric(
      raw_data$ac_att
    )
  )
} else {
  rep(
    NA_real_,
    nrow(raw_data)
  )
}

ac_total_existing_values <- if (
  "ac_total" %in% names(raw_data)
) {
  suppressWarnings(
    as.numeric(
      raw_data$ac_total
    )
  )
} else {
  rep(
    NA_real_,
    nrow(raw_data)
  )
}

# Use tibble directly rather than dplyr::if_else() inside transmute().
attention_comparison <- tibble::tibble(
  source_row = raw_data$source_row,
  id = id_values,
  ac_1 = raw_data$ac_1,
  ac_2 = raw_data$ac_2,
  ac_3 = raw_data$ac_3,
  ac_4 = raw_data$ac_4,
  ac_1_correct = raw_data$ac_1_correct,
  ac_2_correct = raw_data$ac_2_correct,
  ac_3_correct = raw_data$ac_3_correct,
  ac_4_correct = raw_data$ac_4_correct,
  ac_competence_existing =
    ac_competence_existing_values,
  ac_competence_recalculated =
    raw_data$ac_competence_recalculated,
  ac_att_existing =
    ac_att_existing_values,
  ac_att_recalculated =
    raw_data$ac_att_recalculated,
  ac_total_existing =
    ac_total_existing_values,
  ac_total_recalculated =
    raw_data$ac_total_recalculated,
  ac_failed_recalculated =
    raw_data$ac_failed_recalculated
) |>
  dplyr::mutate(
    competence_disagreement = (
      !is.na(ac_competence_existing) &
        ac_competence_existing !=
        ac_competence_recalculated
    ),
    attitude_disagreement = (
      !is.na(ac_att_existing) &
        ac_att_existing !=
        ac_att_recalculated
    ),
    total_disagreement = (
      !is.na(ac_total_existing) &
        ac_total_existing !=
        ac_total_recalculated
    ),
    any_disagreement = (
      competence_disagreement |
        attitude_disagreement |
        total_disagreement
    )
  )

competence_attention_disagreements <- sum(
  attention_comparison$competence_disagreement,
  na.rm = TRUE
)

attitude_attention_disagreements <- sum(
  attention_comparison$attitude_disagreement,
  na.rm = TRUE
)

total_attention_disagreements <- sum(
  attention_comparison$total_disagreement,
  na.rm = TRUE
)

rows_with_any_attention_disagreement <- sum(
  attention_comparison$any_disagreement,
  na.rm = TRUE
)

append_audit_log(
  action = "Attention-check recalculation",
  object = "ac_competence, ac_att, ac_total",
  detail = paste0(
    "Recalculated scores directly from ac_1-ac_4. ",
    "Disagreements with stored scores: competence = ",
    competence_attention_disagreements,
    "; attitude = ",
    attitude_attention_disagreements,
    "; total = ",
    total_attention_disagreements,
    ". Rows with any disagreement = ",
    rows_with_any_attention_disagreement,
    ". Recalculated scores are used for subsample construction."
  ),
  rows_affected = rows_with_any_attention_disagreement,
  columns_affected = 3
)

write_workbook_safely(
  list(
    Attention_check_comparison =
      attention_comparison,
    Attention_disagreements =
      attention_comparison |>
      dplyr::filter(
        any_disagreement
      )
  ),
  file.path(
    TABLE_DIR,
    "attention_check_recalculation.xlsx"
  )
)

message(
  "\nAttention-check recalculation"
)

message(
  "Competence-score disagreements: ",
  competence_attention_disagreements
)

message(
  "Attitude-score disagreements: ",
  attitude_attention_disagreements
)

message(
  "Total-score disagreements: ",
  total_attention_disagreements
)

message(
  "Rows with any attention-score disagreement: ",
  rows_with_any_attention_disagreement
)

# ---------------------------------------------------------------------------
# Core sample
# ---------------------------------------------------------------------------
# Attention checks do not cause global deletion. They define alternative
# subsamples for the CFA sensitivity analyses.

row_audit <- raw_data |>
  dplyr::mutate(
    consent_ok = if (REQUIRE_CONSENT) {
      !is.na(consent) &
        consent == 1
    } else {
      TRUE
    },
    commitment_ok = if (REQUIRE_COMMITMENT) {
      !is.na(commitment) &
        commitment == 1
    } else {
      TRUE
    },
    core_analysis_included = (
      consent_ok &
        commitment_ok
    ),
    core_exclusion_reason = dplyr::case_when(
      !consent_ok ~
        "Consent criterion not met or missing",
      !commitment_ok ~
        "Commitment criterion not met or missing",
      TRUE ~
        "Included"
    ),
    subsample_all_core =
      core_analysis_included,
    subsample_fail_at_most_2 = (
      core_analysis_included &
        !is.na(ac_total_recalculated) &
        ac_total_recalculated >= 2
    ),
    subsample_fail_at_most_1 = (
      core_analysis_included &
        !is.na(ac_total_recalculated) &
        ac_total_recalculated >= 3
    ),
    subsample_pass_all_4 = (
      core_analysis_included &
        !is.na(ac_total_recalculated) &
        ac_total_recalculated == 4
    )
  )

analysis_data <- raw_data[
  row_audit$core_analysis_included,
  ,
  drop = FALSE
]

append_audit_log(
  action = "Core row filtering",
  object = "Core analysis sample",
  detail = paste0(
    "Applied consent and commitment criteria only. Attention checks were not ",
    "used for global deletion. Retained ",
    nrow(analysis_data),
    " of ",
    nrow(raw_data),
    " rows."
  ),
  rows_affected = sum(
    !row_audit$core_analysis_included
  )
)

# Exact row-deletion log.
deleted_rows_log <- row_audit |>
  dplyr::filter(
    !core_analysis_included
  ) |>
  dplyr::select(
    source_row,
    dplyr::any_of("id"),
    consent,
    commitment,
    consent_ok,
    commitment_ok,
    core_exclusion_reason,
    ac_total_recalculated,
    ac_failed_recalculated
  )

message(
  "\nCore sample construction"
)

message(
  "Imported rows: ",
  nrow(raw_data)
)

message(
  "Rows retained in the core sample: ",
  nrow(analysis_data)
)

message(
  "Rows excluded by consent or commitment criteria: ",
  nrow(deleted_rows_log)
)

# ---------------------------------------------------------------------------
# Participant-level scale scores
# ---------------------------------------------------------------------------

safe_row_mean <- function(
    data,
    variables,
    minimum_proportion
) {
  
  missing_variables <- setdiff(
    variables,
    names(data)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "safe_row_mean() is missing variables: ",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  }
  
  matrix_data <- data[
    ,
    variables,
    drop = FALSE
  ] |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(
          as.numeric(.x)
        )
      )
    )
  
  valid_count <- rowSums(
    !is.na(matrix_data)
  )
  
  valid_proportion <- valid_count /
    length(variables)
  
  score <- rowMeans(
    matrix_data,
    na.rm = TRUE
  )
  
  score[
    valid_count == 0
  ] <- NA_real_
  
  score[
    valid_proportion <
      minimum_proportion
  ] <- NA_real_
  
  list(
    score = score,
    valid_count = valid_count,
    valid_proportion = valid_proportion,
    required_count = ceiling(
      length(variables) *
        minimum_proportion
    )
  )
}

positive_score <- safe_row_mean(
  analysis_data,
  ATT_POS_ITEMS,
  MIN_VALID_ITEM_PROP
)

negative_raw_score <- safe_row_mean(
  analysis_data,
  ATT_NEG_ITEMS,
  MIN_VALID_ITEM_PROP
)

negative_favorable_score <- safe_row_mean(
  analysis_data,
  ATT_NEG_SCORED,
  MIN_VALID_ITEM_PROP
)

competence_total_score <- safe_row_mean(
  analysis_data,
  COMP_ALL_ITEMS,
  MIN_VALID_ITEM_PROP
)

analysis_data$attitude_positive <-
  positive_score$score

analysis_data$attitude_positive_valid_count <-
  positive_score$valid_count

analysis_data$attitude_positive_valid_prop <-
  positive_score$valid_proportion

analysis_data$attitude_negative_concern <-
  negative_raw_score$score

analysis_data$attitude_negative_concern_valid_count <-
  negative_raw_score$valid_count

analysis_data$attitude_negative_concern_valid_prop <-
  negative_raw_score$valid_proportion

analysis_data$attitude_negative_favorable <-
  negative_favorable_score$score

analysis_data$attitude_negative_favorable_valid_count <-
  negative_favorable_score$valid_count

analysis_data$attitude_negative_favorable_valid_prop <-
  negative_favorable_score$valid_proportion

analysis_data$competence_total <-
  competence_total_score$score

analysis_data$competence_valid_count <-
  competence_total_score$valid_count

analysis_data$competence_valid_prop <-
  competence_total_score$valid_proportion

# Four competence subscale scores.
competence_awareness_score <- safe_row_mean(
  analysis_data,
  COMP_AWARENESS,
  MIN_VALID_ITEM_PROP
)

competence_usage_score <- safe_row_mean(
  analysis_data,
  COMP_USAGE,
  MIN_VALID_ITEM_PROP
)

competence_evaluation_score <- safe_row_mean(
  analysis_data,
  COMP_EVALUATION,
  MIN_VALID_ITEM_PROP
)

competence_ethics_score <- safe_row_mean(
  analysis_data,
  COMP_ETHICS,
  MIN_VALID_ITEM_PROP
)

analysis_data$competence_awareness <-
  competence_awareness_score$score

analysis_data$competence_usage <-
  competence_usage_score$score

analysis_data$competence_evaluation <-
  competence_evaluation_score$score

analysis_data$competence_ethics <-
  competence_ethics_score$score

# ---------------------------------------------------------------------------
# Participant-score missingness log
# ---------------------------------------------------------------------------

score_availability <- tibble::tibble(
  score = c(
    "attitude_positive",
    "attitude_negative_concern",
    "attitude_negative_favorable",
    "competence_awareness",
    "competence_usage",
    "competence_evaluation",
    "competence_ethics",
    "competence_total"
  ),
  available = c(
    sum(
      !is.na(
        analysis_data$attitude_positive
      )
    ),
    sum(
      !is.na(
        analysis_data$attitude_negative_concern
      )
    ),
    sum(
      !is.na(
        analysis_data$attitude_negative_favorable
      )
    ),
    sum(
      !is.na(
        analysis_data$competence_awareness
      )
    ),
    sum(
      !is.na(
        analysis_data$competence_usage
      )
    ),
    sum(
      !is.na(
        analysis_data$competence_evaluation
      )
    ),
    sum(
      !is.na(
        analysis_data$competence_ethics
      )
    ),
    sum(
      !is.na(
        analysis_data$competence_total
      )
    )
  )
) |>
  dplyr::mutate(
    core_sample_n = nrow(analysis_data),
    unavailable = core_sample_n - available,
    available_pct = available / core_sample_n
  )

message(
  "\nParticipant-level score availability"
)

print(
  score_availability,
  n = Inf
)

# ---------------------------------------------------------------------------
# Sample and missingness logs
# ---------------------------------------------------------------------------

subsample_flow <- tibble::tibble(
  sample_key = names(
    ATTENTION_SUBSAMPLES
  ),
  sample_label = vapply(
    ATTENTION_SUBSAMPLES,
    function(x) {
      x$label
    },
    character(1)
  ),
  minimum_correct_attention_checks = vapply(
    ATTENTION_SUBSAMPLES,
    function(x) {
      x$minimum_correct
    },
    numeric(1)
  ),
  n = c(
    sum(
      row_audit$subsample_all_core
    ),
    sum(
      row_audit$subsample_fail_at_most_2
    ),
    sum(
      row_audit$subsample_fail_at_most_1
    ),
    sum(
      row_audit$subsample_pass_all_4
    )
  )
)

sample_flow <- subsample_flow

factor_missingness_log <- tibble::tibble(
  model = c(
    "Attitude two-factor",
    "Competence four-factor",
    "Quadrant: positive and negative attitudes"
  ),
  required_variables = c(
    paste(
      ATTITUDE_MODEL_ITEMS,
      collapse = ", "
    ),
    paste(
      COMP_ALL_ITEMS,
      collapse = ", "
    ),
    paste(
      c(
        "attitude_positive",
        "attitude_negative_concern"
      ),
      collapse = ", "
    )
  ),
  rows_in_core_sample = nrow(
    analysis_data
  ),
  rows_complete_for_model = c(
    sum(
      stats::complete.cases(
        analysis_data[
          ,
          ATTITUDE_MODEL_ITEMS,
          drop = FALSE
        ]
      )
    ),
    sum(
      stats::complete.cases(
        analysis_data[
          ,
          COMP_ALL_ITEMS,
          drop = FALSE
        ]
      )
    ),
    sum(
      stats::complete.cases(
        analysis_data[
          ,
          c(
            "attitude_positive",
            "attitude_negative_concern"
          ),
          drop = FALSE
        ]
      )
    )
  )
) |>
  dplyr::mutate(
    rows_excluded_for_model = (
      rows_in_core_sample -
        rows_complete_for_model
    ),
    decision = paste0(
      "No global drop_na(). Complete-case filtering is applied only to ",
      "the variables required by this specific analysis."
    )
  )

missingness_before <- missingness_summary(
  raw_data
)

missingness_after <- missingness_summary(
  analysis_data
)

write_workbook_safely(
  list(
    Subsample_flow = subsample_flow,
    Core_row_audit = row_audit,
    Deleted_rows = deleted_rows_log,
    Model_missingness = factor_missingness_log,
    Score_availability = score_availability,
    Missingness_before = missingness_before,
    Missingness_after = missingness_after,
    Reverse_score_descriptives = reverse_score_comparison
  ),
  file.path(
    TABLE_DIR,
    "sample_and_missingness_audit.xlsx"
  )
)

message(
  "\nAttention-check subsamples"
)

print(
  subsample_flow,
  n = Inf
)

message(
  "\nModel-specific missingness decisions"
)

print(
  factor_missingness_log,
  n = Inf
)

# ---------------------------------------------------------------------------
# Save prepared data
# ---------------------------------------------------------------------------

saveRDS(
  raw_data,
  file.path(
    OUTPUT_DIR,
    "raw_data_validated.rds"
  )
)

saveRDS(
  analysis_data,
  file.path(
    OUTPUT_DIR,
    "analysis_data_scored.rds"
  )
)

openxlsx::write.xlsx(
  analysis_data,
  file.path(
    OUTPUT_DIR,
    "analysis_data_scored.xlsx"
  ),
  overwrite = TRUE
)

message(
  "\nPrepared data saved successfully."
)

message(
  "Validated source data: ",
  file.path(
    OUTPUT_DIR,
    "raw_data_validated.rds"
  )
)

message(
  "Scored analysis data: ",
  file.path(
    OUTPUT_DIR,
    "analysis_data_scored.xlsx"
  )
)