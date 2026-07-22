# EnfermerIA: R analysis pipeline

This project analyzes `ds_all_2026_07_22.xlsx` and produces English-language tables and 300-dpi figures using a color-vision-deficiency-safe Okabe-Ito palette.

## Source-based scale specification

- **GAAIS:** two correlated subscales, Positive (12 items) and Negative (8 items). Negative items are reverse-scored so higher values indicate more favorable attitudes. The source does **not** recommend an overall mean. The pipeline therefore reports both subscales separately and creates a combined favorability index only for the requested quadrant visualization.
- **AI Literacy Scale:** four first-order dimensions—Awareness, Usage, Evaluation, and Ethics—with 12 items. `AW_8`, `US_3`, and `ET_2` are reverse-worded. In the supplied variable structure, their `_r` suffix is treated as already reverse-scored. The pipeline also reconstructs `_raw` versions for documentation.

## Project layout

```text
EnfermerIA_R_Pipeline/
├── 00_run_pipeline.R
├── README.md
├── ds_all_2026_07_22.xlsx   # place the data here
├── R/
│   ├── 00_config.R
│   ├── 01_utils.R
│   ├── 02_import_clean.R
│   ├── 03_descriptive.R
│   ├── 04_psychometrics.R
│   ├── 05_cfa.R
│   └── 06_bivariate_quadrants.R
└── outputs/                  # created automatically
```

## Run

Open the project root in RStudio and execute:

```r
source("00_run_pipeline.R")
```

Missing packages are installed automatically. The main configuration options are in `R/00_config.R`, including consent filtering, attention-check threshold, figure dimensions, and mean-versus-median quadrant cuts.

## Main outputs

- `outputs/tables/descriptive_statistics_all_variables.xlsx`
- `outputs/tables/psychometric_diagnostics.xlsx`
- `outputs/tables/cfa_results.xlsx`
- `outputs/tables/bivariate_attitudes_competence.xlsx`
- `outputs/analysis_data_scored.xlsx`
- `outputs/figures/*.png` at 300 dpi
- `outputs/models/*.txt` and fitted lavaan objects in `.rds`

## CFA decision rule

The pipeline evaluates factorability using polychoric correlations, KMO, Bartlett's test, item variance, sample size, and the participant-to-item ratio. It then estimates ordinal CFA models with WLSMV. With approximately 1,211 observations and 20- and 12-item instruments, the sample size is ordinarily adequate; final applicability still depends on response variation, factorability, convergence, standardized loadings, residuals, and fit indices.
