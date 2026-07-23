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
  "R/06_bivariate_quadrants.R",
  "R/07_robustness_diagnostics.R"
)

# Keep an auditable copy of all console messages and printed tables.
source(scripts[[1]], encoding = "UTF-8")
pipeline_console_log <- file.path(
  LOG_DIR,
  "pipeline_console_output.txt"
)
pipeline_message_log <- file.path(
  LOG_DIR,
  "pipeline_messages.log"
)

output_connection <- file(pipeline_console_log, open = "wt", encoding = "UTF-8")
message_connection <- file(pipeline_message_log, open = "wt", encoding = "UTF-8")

sink(output_connection, split = TRUE)
sink(message_connection, type = "message")

tryCatch(
  {
    for (script in scripts[-1]) {
      message("Running ", script)
      source(script, encoding = "UTF-8")
    }
    message("Pipeline completed. See the outputs/ directory.")
  },
  finally = {
    if (sink.number(type = "message") > 0) {
      sink(type = "message")
    }
    if (sink.number() > 0) {
      sink()
    }
    close(output_connection)
    close(message_connection)
  }
)
