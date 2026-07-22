# Project configuration -------------------------------------------------------

DATA_FILE <- "ds_all_2026_07_22.xlsx"
DATA_SHEET <- 1
OUTPUT_DIR <- "outputs"
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")
MODEL_DIR <- file.path(OUTPUT_DIR, "models")
LOG_DIR <- file.path(OUTPUT_DIR, "logs")

for (path in c(OUTPUT_DIR, FIGURE_DIR, TABLE_DIR, MODEL_DIR, LOG_DIR)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# Accessible, color-vision-deficiency-safe Okabe-Ito palette.
PALETTE_OKABE_ITO <- c(
  black = "#000000", orange = "#E69F00", sky_blue = "#56B4E9",
  bluish_green = "#009E73", yellow = "#F0E442", blue = "#0072B2",
  vermillion = "#D55E00", reddish_purple = "#CC79A7"
)

FIG_DPI <- 300
FIG_WIDTH <- 8.5
FIG_HEIGHT <- 5.5
BASE_SIZE <- 12

# Analysis inclusion rule. Change to 3 or 4 for stricter attention-check filtering.
MIN_ATTENTION_TOTAL <- 0
REQUIRE_CONSENT <- TRUE
REQUIRE_COMMITMENT <- FALSE

# Quadrant cut points: "mean" or "median".
QUADRANT_CUT <- "mean"

# GAAIS item structure from Schepman & Rodway (2023).
ATT_POS_ITEMS <- c(
  "att_pos_1", "att_pos_2", "att_pos_4", "att_pos_5", "att_pos_7",
  "att_pos_11", "att_pos_12", "att_pos_13", "att_pos_14", "att_pos_16",
  "att_pos_17", "att_pos_18"
)
ATT_NEG_ITEMS <- c(
  "att_neg_3", "att_neg_6", "att_neg_8", "att_neg_9", "att_neg_10",
  "att_neg_15", "att_neg_19", "att_neg_20"
)

# AI Literacy Scale structure from Wang, Rau, & Yuan (2023).
COMP_AWARENESS <- c("competence_AW_1", "competence_AW_8_r", "competence_AW_9")
COMP_USAGE <- c("competence_US_1", "competence_US_3_r", "competence_US_5")
COMP_EVALUATION <- c("competence_EV_2", "competence_EV_3", "competence_EV_6")
COMP_ETHICS <- c("competence_ET_1", "competence_ET_2_r", "competence_ET_5")
COMP_ALL_ITEMS <- c(COMP_AWARENESS, COMP_USAGE, COMP_EVALUATION, COMP_ETHICS)

ID_VARS <- c("ds", "id")
OPEN_TEXT_VARS <- c("city", "higher_ed_ai_impact", "additional_comments")
BINARY_TECH_VARS <- c("laptop", "home_desktop", "work_study_desktop", "smartphone", "tablet")
CATEGORICAL_VARS <- c(
  "consent", "gender", "occupation", "study_work_area", "commitment",
  "internet_quality", "ai_use_frequency", "ac_competence", "ac_att", "ac_total",
  BINARY_TECH_VARS
)
