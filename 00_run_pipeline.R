# EnfermerIA analysis pipeline -------------------------------------------------
# Run from the project root: source("00_run_pipeline.R")

required_packages <- c(
  "readxl", "openxlsx", "dplyr", "tidyr", "purrr", "stringr", "forcats",
  "ggplot2", "scales", "patchwork", "psych", "lavaan", "semTools",
  "GPArotation", "corrplot", "rlang", "tibble"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

scripts <- c(
  "R/00_config.R",
  "R/01_utils.R",
  "R/02_import_clean.R",
  "R/03_descriptive.R",
  "R/04_psychometrics.R",
  "R/05_cfa.R",
  "R/06_bivariate_quadrants.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script, encoding = "UTF-8")
}

message("Pipeline completed. See the outputs/ directory.")
