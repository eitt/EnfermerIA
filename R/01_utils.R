# Utility functions -----------------------------------------------------------

theme_accessible <- function(base_size = BASE_SIZE) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 15, 10, 10)
    )
}

save_plot_300 <- function(plot, filename, width = FIG_WIDTH, height = FIG_HEIGHT) {
  ggplot2::ggsave(
    filename = filename, plot = plot, width = width, height = height,
    units = "in", dpi = FIG_DPI, bg = "white"
  )
}

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else stats::sd(x, na.rm = TRUE)

reverse_1_to_5 <- function(x) {
  dplyr::if_else(is.na(x), NA_real_, 6 - as.numeric(x))
}

check_required_columns <- function(data, columns, context = "analysis") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop("Missing columns for ", context, ": ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

item_summary <- function(data, items, scale_name) {
  data |>
    dplyr::select(dplyr::all_of(items)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "value") |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      scale = scale_name,
      n = sum(!is.na(value)),
      missing = sum(is.na(value)),
      mean = safe_mean(value),
      sd = safe_sd(value),
      median = stats::median(value, na.rm = TRUE),
      min = suppressWarnings(min(value, na.rm = TRUE)),
      max = suppressWarnings(max(value, na.rm = TRUE)),
      skewness = psych::skew(value, na.rm = TRUE),
      kurtosis = psych::kurtosi(value, na.rm = TRUE),
      .groups = "drop"
    )
}

reliability_row <- function(data, items, scale_name) {
  x <- data |> dplyr::select(dplyr::all_of(items))
  alpha_obj <- suppressWarnings(psych::alpha(x, check.keys = FALSE, warnings = FALSE))
  omega_obj <- tryCatch(
    suppressWarnings(psych::omega(x, nfactors = 1, plot = FALSE, warnings = FALSE)),
    error = function(e) NULL
  )
  tibble::tibble(
    scale = scale_name,
    items = length(items),
    n_complete = sum(stats::complete.cases(x)),
    cronbach_alpha = unname(alpha_obj$total$raw_alpha),
    standardized_alpha = unname(alpha_obj$total$std.alpha),
    omega_total = if (is.null(omega_obj)) NA_real_ else unname(omega_obj$omega.tot),
    mean_interitem_r = unname(alpha_obj$total$average_r)
  )
}

write_workbook_safely <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, substr(nm, 1, 31))
    openxlsx::writeDataTable(wb, substr(nm, 1, 31), sheets[[nm]], tableStyle = "TableStyleMedium2")
    openxlsx::freezePane(wb, substr(nm, 1, 31), firstRow = TRUE)
    openxlsx::setColWidths(wb, substr(nm, 1, 31), cols = 1:ncol(sheets[[nm]]), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
