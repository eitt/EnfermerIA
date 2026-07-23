
from __future__ import annotations

import io
import json
import math
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Iterable

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st
from scipy import stats
from semopy import Model, calc_stats


# =============================================================================
# Application configuration
# =============================================================================

st.set_page_config(
    page_title="EnfermerIA Psychometric Playground",
    page_icon="🧠",
    layout="wide",
)

APP_TITLE = "EnfermerIA Psychometric Playground"
LIKERT_MIN = 1
LIKERT_MAX = 5

# These are the validated R-pipeline keys.  The original columns ac_total,
# ac_competence, and ac_att are never overwritten; the playground creates
# explicit recalculated columns for sensitivity analysis.
ATTENTION_CORRECT_RESPONSES = {
    "ac_1": 1,
    "ac_2": 1,
    "ac_3": 1,
    "ac_4": 1,
}
ATTENTION_SAMPLE_LABELS = {
    "No attention filter (all_core)": None,
    "At least 2 correct (fail_at_most_2)": 2,
    "At least 3 correct (fail_at_most_1)": 3,
    "All 4 correct (pass_all_4)": 4,
}

ATT_POS_ITEMS = [
    "att_pos_1", "att_pos_2", "att_pos_4", "att_pos_5", "att_pos_7",
    "att_pos_11", "att_pos_12", "att_pos_13", "att_pos_14",
    "att_pos_16", "att_pos_17", "att_pos_18",
]

ATT_NEG_ITEMS = [
    "att_neg_3", "att_neg_6", "att_neg_8", "att_neg_9",
    "att_neg_10", "att_neg_15", "att_neg_19", "att_neg_20",
]

COMP_AWARENESS = [
    "competence_AW_1", "competence_AW_8_r", "competence_AW_9",
]

COMP_USAGE = [
    "competence_US_1", "competence_US_3_r", "competence_US_5",
]

COMP_EVALUATION = [
    "competence_EV_2", "competence_EV_3", "competence_EV_6",
]

COMP_ETHICS = [
    "competence_ET_1", "competence_ET_2_r", "competence_ET_5",
]

COMP_ALL_ITEMS = (
    COMP_AWARENESS + COMP_USAGE + COMP_EVALUATION + COMP_ETHICS
)

DEFAULT_REVERSE_COMPETENCE = [
    "competence_AW_8_r",
    "competence_US_3_r",
    "competence_ET_2_r",
]

ANALYSIS_COMPETENCE_ITEMS = {
    item: f"{item}__analysis" for item in DEFAULT_REVERSE_COMPETENCE
}


def analysis_items(items: list[str]) -> list[str]:
    return [ANALYSIS_COMPETENCE_ITEMS.get(item, item) for item in items]

DEFAULT_CONSTRUCTS = {
    "Positive attitudes toward AI": ATT_POS_ITEMS,
    "Negative attitudes toward AI": ATT_NEG_ITEMS,
    "AI competence: Awareness": analysis_items(COMP_AWARENESS),
    "AI competence: Usage": analysis_items(COMP_USAGE),
    "AI competence: Evaluation": analysis_items(COMP_EVALUATION),
    "AI competence: Ethics": analysis_items(COMP_ETHICS),
}

CONSTRUCT_DESCRIPTIONS = {
    "Positive attitudes toward AI": (
        "Favorable evaluations of artificial intelligence, including perceived "
        "usefulness, excitement, beneficial applications, wellbeing effects, "
        "and willingness to use AI."
    ),
    "Negative attitudes toward AI": (
        "Concerns about error, danger, unethical use, surveillance, negative "
        "consequences, discomfort, and potential personal harm."
    ),
    "AI competence: Awareness": (
        "Ability to recognize AI-enabled systems, distinguish smart from "
        "non-smart technologies, and understand how AI can provide support."
    ),
    "AI competence: Usage": (
        "Ability to use AI applications effectively in daily, academic, or "
        "professional work."
    ),
    "AI competence: Evaluation": (
        "Ability to assess AI capabilities and limitations and select an "
        "appropriate AI system or output for a specific task."
    ),
    "AI competence: Ethics": (
        "Attention to ethical principles, misuse, privacy, and information "
        "security when using AI."
    ),
}


# =============================================================================
# Helpers
# =============================================================================

@dataclass
class FilterResult:
    data: pd.DataFrame
    audit: pd.DataFrame
    row_flags: pd.DataFrame


def safe_numeric(series: pd.Series) -> pd.Series:
    return pd.to_numeric(series, errors="coerce")


def recalculate_attention_checks(df: pd.DataFrame) -> pd.DataFrame:
    """Recalculate attention checks using the same key as R/00_config.R.

    The stored ac_total/ac_competence/ac_att columns are retained for audit
    and comparison. Missing checks are treated as incorrect, matching the R
    expressions ``!is.na(x) & x == expected``.
    """
    out = df.copy()
    correct_columns = []
    for variable, expected in ATTENTION_CORRECT_RESPONSES.items():
        correct_name = f"{variable}_correct_recalculated"
        if variable in out.columns:
            out[correct_name] = safe_numeric(out[variable]).eq(expected).fillna(False)
        else:
            out[correct_name] = False
        correct_columns.append(correct_name)

    out["ac_competence_recalculated"] = (
        out[["ac_1_correct_recalculated", "ac_2_correct_recalculated"]]
        .astype(int)
        .sum(axis=1)
    )
    out["ac_att_recalculated"] = (
        out[["ac_3_correct_recalculated", "ac_4_correct_recalculated"]]
        .astype(int)
        .sum(axis=1)
    )
    out["ac_total_recalculated"] = (
        out["ac_competence_recalculated"] + out["ac_att_recalculated"]
    )
    out["ac_failed_recalculated"] = 4 - out["ac_total_recalculated"]
    return out


def attention_key_validation_table(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for variable, expected in ATTENTION_CORRECT_RESPONSES.items():
        observed = safe_numeric(df[variable]) if variable in df.columns else pd.Series(dtype=float)
        rows.append({
            "variable": variable,
            "expected_response": expected,
            "expected_response_observed": bool(observed.eq(expected).any()),
            "n_expected": int(observed.eq(expected).sum()),
            "key_status": (
                "Expected response observed"
                if bool(observed.eq(expected).any())
                else "Expected response not observed"
            ),
        })
    return pd.DataFrame(rows)


def reverse_likert(series: pd.Series) -> pd.Series:
    numeric = safe_numeric(series)
    return (LIKERT_MAX + LIKERT_MIN) - numeric


def unique_preserve_order(values: Iterable[str]) -> list[str]:
    return list(dict.fromkeys(values))


def canonicalize_columns(df: pd.DataFrame) -> tuple[pd.DataFrame, list[dict[str, Any]]]:
    out = df.copy()
    audit: list[dict[str, Any]] = []

    stripped = [str(c).strip() for c in out.columns]
    if stripped != list(out.columns):
        out.columns = stripped
        audit.append({
            "action": "trim_column_names",
            "detail": "Leading and trailing spaces were removed from column names.",
            "rows_affected": 0,
            "columns_affected": len(out.columns),
        })

    if "age" in out.columns and "age_years" not in out.columns:
        out = out.rename(columns={"age": "age_years"})
        audit.append({
            "action": "rename_column",
            "detail": "age was renamed to age_years.",
            "rows_affected": 0,
            "columns_affected": 1,
        })

    duplicated_names = pd.Series(out.columns).duplicated(keep=False)
    duplicate_candidates = pd.Series(out.columns)[duplicated_names].tolist()

    if duplicate_candidates:
        new_names = []
        counts: dict[str, int] = {}
        for col in out.columns:
            counts[col] = counts.get(col, 0) + 1
            suffix = counts[col]
            new_names.append(col if suffix == 1 else f"{col}__dup{suffix}")
        out.columns = new_names
        audit.append({
            "action": "disambiguate_duplicate_columns",
            "detail": f"Duplicate names renamed: {', '.join(duplicate_candidates)}",
            "rows_affected": 0,
            "columns_affected": len(duplicate_candidates),
        })

    ac_candidates = [c for c in out.columns if c == "ac_total" or c.startswith("ac_total__dup")]
    if len(ac_candidates) > 1:
        nonmissing_counts = {c: int(out[c].notna().sum()) for c in ac_candidates}
        keeper = max(nonmissing_counts, key=nonmissing_counts.get)
        if keeper != "ac_total":
            if "ac_total" in out.columns:
                out = out.drop(columns=["ac_total"])
            out = out.rename(columns={keeper: "ac_total"})
        drop_cols = [c for c in ac_candidates if c != keeper and c in out.columns]
        if drop_cols:
            out = out.drop(columns=drop_cols)
        audit.append({
            "action": "resolve_ac_total_duplicates",
            "detail": (
                f"Retained {keeper} as ac_total based on non-missing count; "
                f"removed {', '.join(drop_cols) if drop_cols else 'none'}."
            ),
            "rows_affected": 0,
            "columns_affected": len(drop_cols) + 1,
        })

    all_missing = [c for c in out.columns if out[c].isna().all()]
    if all_missing:
        out = out.drop(columns=all_missing)
        audit.append({
            "action": "remove_all_missing_columns",
            "detail": f"Removed: {', '.join(all_missing)}",
            "rows_affected": 0,
            "columns_affected": len(all_missing),
        })

    return out, audit


def load_data(uploaded_file) -> tuple[pd.DataFrame, list[dict[str, Any]]]:
    if uploaded_file is None:
        raise ValueError("Upload an Excel or CSV file to begin.")

    name = uploaded_file.name.lower()
    if name.endswith(".xlsx") or name.endswith(".xls"):
        df = pd.read_excel(uploaded_file)
    elif name.endswith(".csv"):
        df = pd.read_csv(uploaded_file)
    else:
        raise ValueError("Supported formats: .xlsx, .xls, and .csv.")

    df, audit = canonicalize_columns(df)
    return df, audit


def existing(df: pd.DataFrame, variables: Iterable[str]) -> list[str]:
    return [v for v in variables if v in df.columns]


def missingness_table(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for col in df.columns:
        s = df[col]
        blank = s.isna()
        if pd.api.types.is_object_dtype(s) or pd.api.types.is_string_dtype(s):
            blank = blank | s.astype("string").str.strip().eq("")
        rows.append({
            "variable": col,
            "n": len(s),
            "nonmissing": int((~blank).sum()),
            "missing_or_blank": int(blank.sum()),
            "missing_pct": float(blank.mean()),
            "unique_nonmissing": int(s[~blank].nunique(dropna=True)),
        })
    return pd.DataFrame(rows).sort_values(
        ["missing_pct", "variable"], ascending=[False, True]
    )


def build_filter_result(
    df: pd.DataFrame,
    require_consent: bool,
    require_commitment: bool,
    min_attention: int,
    attention_mode: str,
    selected_scale_items: list[str],
    min_valid_proportion: float,
    remove_fully_blank_rows: bool,
    coerce_invalid_likert: bool,
    reverse_competence_items: list[str],
    reverse_negative_attitude_items: bool,
) -> FilterResult:
    data = df.copy()
    audit_rows: list[dict[str, Any]] = []
    flags = pd.DataFrame(index=data.index)
    flags["source_row"] = data.index + 2

    if remove_fully_blank_rows:
        all_blank = data.isna().all(axis=1)
        if all_blank.any():
            data = data.loc[~all_blank].copy()
            flags = flags.loc[~all_blank].copy()
            audit_rows.append({
                "action": "remove_fully_blank_rows",
                "detail": "Rows with no recorded value in any column were removed.",
                "rows_affected": int(all_blank.sum()),
                "columns_affected": int(data.shape[1]),
            })

    likert_candidates = unique_preserve_order(
        existing(data, ATT_POS_ITEMS + ATT_NEG_ITEMS + COMP_ALL_ITEMS)
    )
    invalid_detail = []
    if coerce_invalid_likert:
        for col in likert_candidates:
            numeric = safe_numeric(data[col])
            invalid = numeric.notna() & ~numeric.between(LIKERT_MIN, LIKERT_MAX)
            if invalid.any():
                invalid_detail.append(f"{col}: {int(invalid.sum())}")
                data.loc[invalid, col] = np.nan
        if invalid_detail:
            audit_rows.append({
                "action": "coerce_invalid_likert_to_na",
                "detail": "; ".join(invalid_detail),
                "rows_affected": int(
                    sum(int(part.split(": ")[1]) for part in invalid_detail)
                ),
                "columns_affected": len(invalid_detail),
            })

    data = recalculate_attention_checks(data)
    key_validation = attention_key_validation_table(data)
    audit_rows.append({
        "action": "recalculate_attention_checks",
        "detail": (
            "Used ac_1=1, ac_2=1, ac_3=1, ac_4=1; original ac_total, "
            "ac_competence, and ac_att were retained unchanged."
        ),
        "rows_affected": len(data),
        "columns_affected": 8,
    })

    for col in existing(data, reverse_competence_items):
        original = safe_numeric(data[col])
        data[f"{col}__analysis"] = reverse_likert(original)
        audit_rows.append({
            "action": "reverse_score_competence_item",
            "detail": f"Created {col}__analysis = 6 - {col}.",
            "rows_affected": int(original.notna().sum()),
            "columns_affected": 1,
        })

    if reverse_negative_attitude_items:
        for col in existing(data, ATT_NEG_ITEMS):
            original = safe_numeric(data[col])
            data[f"{col}_r"] = reverse_likert(original)
        audit_rows.append({
            "action": "reverse_score_negative_attitudes",
            "detail": "Created reverse-scored favorable versions of all available negative-attitude items.",
            "rows_affected": len(data),
            "columns_affected": len(existing(data, ATT_NEG_ITEMS)),
        })

    keep = pd.Series(True, index=data.index, dtype=bool)

    if require_consent and "consent" in data.columns:
        criterion = safe_numeric(data["consent"]).eq(1)
        flags["exclude_consent"] = ~criterion
        keep &= criterion
    else:
        flags["exclude_consent"] = False

    if require_commitment and "commitment" in data.columns:
        criterion = safe_numeric(data["commitment"]).eq(1)
        flags["exclude_commitment"] = ~criterion
        keep &= criterion
    else:
        flags["exclude_commitment"] = False

    attention_cols = existing(
        data,
        ["ac_total_recalculated", "ac_competence_recalculated", "ac_att_recalculated"],
    )
    if attention_mode == "Total score" and "ac_total_recalculated" in attention_cols:
        criterion = safe_numeric(data["ac_total_recalculated"]).ge(min_attention)
    else:
        criterion = pd.Series(True, index=data.index)

    flags["exclude_attention"] = ~criterion
    flags["ac_total_recalculated"] = data["ac_total_recalculated"].to_numpy()
    flags["ac_competence_recalculated"] = data["ac_competence_recalculated"].to_numpy()
    flags["ac_att_recalculated"] = data["ac_att_recalculated"].to_numpy()
    keep &= criterion

    scale_vars = existing(data, selected_scale_items)
    if scale_vars:
        valid_count = data[scale_vars].apply(pd.to_numeric, errors="coerce").notna().sum(axis=1)
        required_count = max(1, math.ceil(len(scale_vars) * min_valid_proportion))
        sparse = valid_count < required_count
        flags["valid_scale_items"] = valid_count
        flags["required_scale_items"] = required_count
        flags["exclude_sparse_scale_data"] = sparse
        keep &= ~sparse
    else:
        flags["valid_scale_items"] = np.nan
        flags["required_scale_items"] = np.nan
        flags["exclude_sparse_scale_data"] = False

    flags["included"] = keep
    flags["exclusion_reason"] = flags.apply(
        lambda row: "; ".join([
            name.replace("exclude_", "")
            for name in [
                "exclude_consent",
                "exclude_commitment",
                "exclude_attention",
                "exclude_sparse_scale_data",
            ]
            if bool(row.get(name, False))
        ]) or "Included",
        axis=1,
    )

    filtered = data.loc[keep].copy()

    audit_rows.extend([
        {
            "action": "apply_analysis_filters",
            "detail": (
                f"Consent={require_consent}; commitment={require_commitment}; "
                f"attention mode={attention_mode}; recalculated minimum attention={min_attention}; "
                f"minimum valid item proportion={min_valid_proportion:.2f}."
            ),
            "rows_affected": int((~keep).sum()),
            "columns_affected": len(scale_vars),
        },
        {
            "action": "final_analysis_sample",
            "detail": f"Retained {len(filtered)} of {len(data)} rows.",
            "rows_affected": len(filtered),
            "columns_affected": filtered.shape[1],
        },
    ])

    audit_df = pd.DataFrame(audit_rows)
    audit_df.insert(0, "timestamp", datetime.now().isoformat(timespec="seconds"))
    return FilterResult(filtered, audit_df, flags.reset_index(drop=True))


def cronbach_alpha(df: pd.DataFrame) -> float:
    x = df.apply(pd.to_numeric, errors="coerce").dropna()
    if x.shape[0] < 3 or x.shape[1] < 2:
        return np.nan
    item_vars = x.var(axis=0, ddof=1)
    total_var = x.sum(axis=1).var(ddof=1)
    if total_var <= 0:
        return np.nan
    k = x.shape[1]
    return float((k / (k - 1)) * (1 - item_vars.sum() / total_var))


def build_model_syntax(
    selected_constructs: dict[str, list[str]],
    available_columns: list[str],
) -> tuple[str, dict[str, list[str]]]:
    syntax_lines = []
    used: dict[str, list[str]] = {}
    safe_names = {
        "Positive attitudes toward AI": "Positive",
        "Negative attitudes toward AI": "Negative",
        "AI competence: Awareness": "Awareness",
        "AI competence: Usage": "Usage",
        "AI competence: Evaluation": "Evaluation",
        "AI competence: Ethics": "Ethics",
    }

    for construct, items in selected_constructs.items():
        valid_items = [i for i in items if i in available_columns]
        if len(valid_items) >= 2:
            safe_construct = safe_names.get(
                construct,
                construct.replace(" ", "_").replace(":", "").replace("-", "_"),
            )
            syntax_lines.append(f"{safe_construct} =~ {' + '.join(valid_items)}")
            used[safe_construct] = valid_items

    return "\n".join(syntax_lines), used


def run_semopy_cfa(
    data: pd.DataFrame,
    model_syntax: str,
    objective: str,
) -> dict[str, Any]:
    if not model_syntax.strip():
        raise ValueError("Select at least one construct with two available indicators.")

    indicator_names = sorted(set(
        token.strip()
        for line in model_syntax.splitlines()
        if "=~" in line
        for token in line.split("=~", 1)[1].split("+")
    ))

    fit_data = data[indicator_names].apply(pd.to_numeric, errors="coerce").dropna()

    if fit_data.shape[0] < 100:
        raise ValueError(
            f"Only {fit_data.shape[0]} complete cases remain. "
            "At least 100 are recommended for this playground."
        )
    if any(fit_data[col].nunique() < 2 for col in fit_data.columns):
        bad = [c for c in fit_data.columns if fit_data[c].nunique() < 2]
        raise ValueError(f"Indicators without variation: {', '.join(bad)}")

    model = Model(model_syntax)
    result = model.fit(fit_data, obj=objective)
    fit_stats = calc_stats(model).T.reset_index().rename(columns={"index": "measure"})
    estimates = model.inspect(std_est=True)

    return {
        "model": model,
        "result": result,
        "stats": fit_stats,
        "estimates": estimates,
        "n": len(fit_data),
        "indicators": indicator_names,
    }


def fit_summary_table(stats_df: pd.DataFrame) -> pd.DataFrame:
    desired = [
        "DoF", "chi2", "chi2 p-value", "CFI", "TLI",
        "RMSEA", "GFI", "AGFI", "NFI", "AIC", "BIC",
    ]
    available = [c for c in desired if c in stats_df.columns]
    if not available:
        return stats_df
    return (
        stats_df[["measure"] + available]
        .melt(id_vars="measure", var_name="fit_index", value_name="value")
        .drop(columns=["measure"])
    )


def make_correlation_heatmap(
    corr: pd.DataFrame,
    title: str,
) -> go.Figure:
    fig = px.imshow(
        corr,
        text_auto=".2f",
        zmin=-1,
        zmax=1,
        color_continuous_scale="RdBu_r",
        aspect="auto",
        title=title,
    )
    fig.update_layout(height=max(500, 35 * len(corr.columns)))
    return fig



def scalar_fit_value(stats_df: pd.DataFrame, name: str) -> float:
    """Extract one numeric fit index from semopy's calc_stats output."""
    if name not in stats_df.columns or stats_df.empty:
        return np.nan
    value = pd.to_numeric(stats_df[name], errors="coerce").iloc[0]
    return float(value) if pd.notna(value) else np.nan


def classify_fit_index(index_name: str, value: float) -> tuple[str, str]:
    """Return an accessible interpretation and status for a common fit index."""
    if pd.isna(value):
        return "Not available", "Unavailable"

    if index_name in {"CFI", "TLI", "GFI", "AGFI", "NFI"}:
        if value >= 0.95:
            return "Strong fit", "Good"
        if value >= 0.90:
            return "Marginal or acceptable fit", "Caution"
        return "Poor fit", "Poor"

    if index_name == "RMSEA":
        if value <= 0.05:
            return "Close fit", "Good"
        if value <= 0.08:
            return "Reasonable fit", "Caution"
        return "Poor fit", "Poor"

    if index_name == "SRMR":
        if value <= 0.05:
            return "Strong residual fit", "Good"
        if value <= 0.08:
            return "Acceptable residual fit", "Caution"
        return "Poor residual fit", "Poor"

    if index_name == "chi2 p-value":
        if value >= 0.05:
            return "Exact-fit test not rejected", "Good"
        return "Exact-fit test rejected; inspect approximate fit indices", "Caution"

    return "Use comparatively across models", "Information"


def build_fit_diagnostics(stats_df: pd.DataFrame) -> pd.DataFrame:
    indices = ["CFI", "TLI", "RMSEA", "GFI", "AGFI", "NFI", "chi2 p-value"]
    rows = []
    for index_name in indices:
        value = scalar_fit_value(stats_df, index_name)
        interpretation, status = classify_fit_index(index_name, value)
        rows.append({
            "fit_index": index_name,
            "value": value,
            "interpretation": interpretation,
            "status": status,
        })
    return pd.DataFrame(rows)


def global_fit_conclusion(diagnostics: pd.DataFrame) -> str:
    available = diagnostics.dropna(subset=["value"])
    if available.empty:
        return "Fit could not be evaluated because the required indices were unavailable."

    poor = int((available["status"] == "Poor").sum())
    caution = int((available["status"] == "Caution").sum())
    good = int((available["status"] == "Good").sum())

    if poor >= 2:
        return (
            "Overall interpretation: the model does not reproduce the observed "
            "covariance structure adequately. Inspect coding, indicator loadings, "
            "factor correlations, and theoretically defensible alternative models."
        )
    if poor == 0 and caution <= 1 and good >= 3:
        return (
            "Overall interpretation: the available indices provide broadly "
            "supportive evidence of model fit. Confirm the result in lavaan/WLSMV "
            "and inspect local diagnostics before publication."
        )
    return (
        "Overall interpretation: evidence is mixed. The model may be useful for "
        "diagnostic comparison, but it should not be treated as clearly fitting "
        "without additional local and sensitivity checks."
    )


def loading_quality_table(estimates: pd.DataFrame) -> pd.DataFrame:
    if estimates.empty or "op" not in estimates.columns:
        return pd.DataFrame()

    loadings = estimates.loc[estimates["op"].eq("~")].copy()
    if loadings.empty:
        return loadings

    loading_col = "Est. Std" if "Est. Std" in loadings.columns else "Estimate"
    loadings["standardized_loading"] = pd.to_numeric(
        loadings[loading_col], errors="coerce"
    )
    loadings["absolute_loading"] = loadings["standardized_loading"].abs()
    loadings["loading_status"] = pd.cut(
        loadings["absolute_loading"],
        bins=[-np.inf, 0.30, 0.50, 0.70, np.inf],
        labels=["Very weak", "Weak", "Moderate", "Strong"],
        right=False,
    )
    return loadings


def construct_score_frame(data: pd.DataFrame) -> pd.DataFrame:
    scores = pd.DataFrame(index=data.index)
    for construct, items in DEFAULT_CONSTRUCTS.items():
        valid = existing(data, items)
        if valid:
            scores[construct] = (
                data[valid].apply(pd.to_numeric, errors="coerce").mean(axis=1)
            )
    return scores


def to_excel_bytes(sheets: dict[str, pd.DataFrame]) -> bytes:
    buffer = io.BytesIO()
    with pd.ExcelWriter(buffer, engine="openpyxl") as writer:
        for name, frame in sheets.items():
            safe_name = name[:31]
            frame.to_excel(writer, sheet_name=safe_name, index=False)
    buffer.seek(0)
    return buffer.getvalue()


# =============================================================================
# State initialization
# =============================================================================

if "raw_data" not in st.session_state:
    st.session_state.raw_data = None
if "initial_audit" not in st.session_state:
    st.session_state.initial_audit = []
if "filtered_result" not in st.session_state:
    st.session_state.filtered_result = None
if "cfa_result" not in st.session_state:
    st.session_state.cfa_result = None
if "uploaded_signature" not in st.session_state:
    st.session_state.uploaded_signature = None


# =============================================================================
# Sidebar
# =============================================================================

st.sidebar.title(APP_TITLE)
page = st.sidebar.radio(
    "Page",
    [
        "1. Data and construct guide",
        "2. Filtering playground",
        "3. Descriptive diagnostics",
        "4. Correlation explorer",
        "5. CFA fit playground",
        "6. Score quadrants",
        "7. Export and audit",
        "8. Pipeline and sensitivity guide",
    ],
)

uploaded = st.sidebar.file_uploader(
    "Upload dataset",
    type=["xlsx", "xls", "csv"],
    help="Recommended file: ds_all_2026_07_22.xlsx",
)

if uploaded is not None:
    signature = (uploaded.name, getattr(uploaded, "size", None))
    if signature != st.session_state.uploaded_signature:
        try:
            raw_data, initial_audit = load_data(uploaded)
            st.session_state.raw_data = raw_data
            st.session_state.initial_audit = initial_audit
            st.session_state.filtered_result = None
            st.session_state.cfa_result = None
            st.session_state.uploaded_signature = signature
        except Exception as exc:
            st.sidebar.error(str(exc))

df = st.session_state.raw_data

if df is None:
    st.title(APP_TITLE)
    st.info(
        "Upload the EnfermerIA Excel or CSV dataset in the sidebar. "
        "The application will then enable interactive filtering, missing-data "
        "diagnostics, correlations, CFA model comparison, and quadrant plots."
    )
    st.stop()

st.sidebar.success(
    f"Loaded {df.shape[0]:,} rows and {df.shape[1]:,} columns."
)

# ---------------------------------------------------------------------------
# Global analysis filters
# ---------------------------------------------------------------------------
# These controls are evaluated on every rerun and therefore apply consistently
# to every page in the application.

with st.sidebar.expander("Global analysis filters", expanded=True):
    require_consent = st.checkbox(
        "Require consent = 1",
        value=True,
        key="global_require_consent",
    )
    require_commitment = st.checkbox(
        "Require commitment = 1",
        value=False,
        key="global_require_commitment",
    )
    remove_blank_rows = st.checkbox(
        "Remove fully blank rows",
        value=True,
        key="global_remove_blank_rows",
    )
    coerce_invalid = st.checkbox(
        "Convert scale values outside 1–5 to missing",
        value=True,
        key="global_coerce_invalid",
    )
    reverse_att_neg = st.checkbox(
        "Create favorable reverse-scored negative-attitude items",
        value=True,
        key="global_reverse_attitude",
    )

    st.markdown("**Attention checks (R-compatible, recalculated key)**")
    attention_sample = st.selectbox(
        "Sensitivity sample",
        options=list(ATTENTION_SAMPLE_LABELS),
        index=0,
        key="global_attention_sample",
        help=(
            "Uses recalculated ac_total_recalculated. The original ac_total "
            "is retained and never overwritten."
        ),
    )
    selected_attention_threshold = ATTENTION_SAMPLE_LABELS[attention_sample]
    use_attention_filter = selected_attention_threshold is not None
    min_attention = selected_attention_threshold or 0

    st.markdown("**Sparse-response filtering**")
    use_sparse_filter = st.checkbox(
        "Exclude rows with insufficient scale responses",
        value=False,
        key="global_use_sparse_filter",
    )
    all_scale_items = existing(
        df,
        ATT_POS_ITEMS + ATT_NEG_ITEMS + COMP_ALL_ITEMS,
    )
    selected_scale_items = st.multiselect(
        "Items defining row completeness",
        options=all_scale_items,
        default=all_scale_items,
        disabled=not use_sparse_filter,
        key="global_sparse_items",
    )
    min_valid_prop = st.slider(
        "Minimum valid item proportion",
        min_value=0.50,
        max_value=1.00,
        value=0.80,
        step=0.05,
        disabled=not use_sparse_filter,
        key="global_min_valid_prop",
    )

    st.markdown("**Optional competence recoding**")
    reverse_comp = st.multiselect(
        "Reverse these stored competence columns",
        options=existing(df, COMP_ALL_ITEMS),
        default=[],
        key="global_reverse_comp",
        help=(
            "Select only when inspection confirms that a stored `_r` column "
            "has not already been reverse-scored."
        ),
    )

attention_mode = "Total score" if use_attention_filter else "Do not filter"

active_sparse_items = selected_scale_items if use_sparse_filter else []

st.session_state.filtered_result = build_filter_result(
    df=df,
    require_consent=require_consent,
    require_commitment=require_commitment,
    min_attention=min_attention,
    attention_mode=attention_mode,
    selected_scale_items=active_sparse_items,
    min_valid_proportion=min_valid_prop,
    remove_fully_blank_rows=remove_blank_rows,
    coerce_invalid_likert=coerce_invalid,
    reverse_competence_items=unique_preserve_order(
        DEFAULT_REVERSE_COMPETENCE + reverse_comp
    ),
    reverse_negative_attitude_items=reverse_att_neg,
)

analysis_df_global = st.session_state.filtered_result.data

st.sidebar.caption(
    f"Active sample: {len(analysis_df_global):,} of {len(df):,} rows"
)


# =============================================================================
# Page 1
# =============================================================================

if page == "1. Data and construct guide":
    st.title("Data and construct guide")
    st.markdown(
        """
        This playground is designed for sensitivity analysis rather than for
        automatically selecting a “best” model. It lets the analyst vary
        attention-check thresholds, sparse-row rules, reverse coding, and
        indicator inclusion while preserving an explicit audit trail.
        """
    )

    c1, c2, c3 = st.columns(3)
    c1.metric("Rows", f"{len(df):,}")
    c2.metric("Columns", f"{df.shape[1]:,}")
    c3.metric("Potential scale items", len(existing(df, ATT_POS_ITEMS + ATT_NEG_ITEMS + COMP_ALL_ITEMS)))

    st.subheader("Construct definitions")
    for construct, items in DEFAULT_CONSTRUCTS.items():
        with st.expander(construct, expanded=False):
            st.write(CONSTRUCT_DESCRIPTIONS[construct])
            availability = pd.DataFrame({
                "item": items,
                "available": [i in df.columns for i in items],
            })
            st.dataframe(availability, use_container_width=True, hide_index=True)

    st.subheader("Core scoring equations")
    st.latex(r"x_i^{(R)} = 6 - x_i")
    st.caption(
        "Reverse scoring for a five-category item. Apply only when the original "
        "item wording requires reversal and the stored column has not already "
        "been reversed."
    )
    st.latex(r"\bar{x}_{j} = \frac{1}{m_j}\sum_{i=1}^{m_j}x_{ij}")
    st.caption(
        "Participant-level construct score calculated from the valid indicators "
        "retained for that construct."
    )
    st.latex(r"\alpha = \frac{k}{k-1}\left(1-\frac{\sum \sigma_i^2}{\sigma_T^2}\right)")
    st.caption("Cronbach's alpha, reported as a descriptive reliability estimate.")

    st.warning(
        "The competence columns ending in `_r` require verification. The app "
        "therefore makes reverse scoring optional and always records the choice."
    )


# =============================================================================
# Page 2
# =============================================================================

elif page == "2. Filtering playground":
    st.title("Global filtering playground")
    st.markdown(
        """
        All filtering controls are located in the left panel and apply
        simultaneously to every page. Change any checkbox or threshold to
        regenerate the active analysis sample, correlations, descriptive
        summaries, CFA inputs, quadrant plots, and exports.
        """
    )

    result = st.session_state.filtered_result
    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Source rows", f"{len(df):,}")
    c2.metric("Retained rows", f"{len(result.data):,}")
    c3.metric("Excluded rows", f"{len(df) - len(result.data):,}")
    c4.metric("Retention rate", f"{len(result.data) / max(len(df), 1):.1%}")

    reason_counts = (
        result.row_flags["exclusion_reason"]
        .value_counts(dropna=False)
        .rename_axis("reason")
        .reset_index(name="rows")
        .sort_values("rows", ascending=True)
    )
    fig = px.bar(
        reason_counts,
        x="rows",
        y="reason",
        orientation="h",
        title="Row disposition under the active global filters",
        labels={"rows": "Rows", "reason": ""},
    )
    st.plotly_chart(fig, use_container_width=True)

    st.subheader("Current filter audit")
    st.dataframe(result.audit, use_container_width=True, hide_index=True)

    st.subheader("Row-level eligibility flags")
    st.dataframe(result.row_flags, use_container_width=True, hide_index=True)


# =============================================================================
# Page 3
# =============================================================================

elif page == "3. Descriptive diagnostics":
    st.title("Descriptive diagnostics")
    analysis_df = analysis_df_global.copy()

    missing = missingness_table(analysis_df)
    max_missing = st.slider(
        "Show variables with missingness at or above",
        min_value=0.0,
        max_value=1.0,
        value=0.0,
        step=0.05,
    )
    shown = missing[missing["missing_pct"] >= max_missing].copy()
    st.dataframe(
        shown.style.format({"missing_pct": "{:.1%}"}),
        use_container_width=True,
        hide_index=True,
    )

    fig = px.bar(
        shown.sort_values("missing_pct"),
        x="missing_pct",
        y="variable",
        orientation="h",
        title="Missingness by variable",
        labels={"missing_pct": "Missing proportion", "variable": ""},
    )
    fig.update_xaxes(tickformat=".0%")
    fig.update_layout(height=max(450, 24 * len(shown)))
    st.plotly_chart(fig, use_container_width=True)

    st.subheader("Interactive variable distribution")
    variable = st.selectbox("Variable", analysis_df.columns.tolist())
    s = analysis_df[variable]

    if pd.api.types.is_numeric_dtype(s) or safe_numeric(s).notna().mean() > 0.8:
        numeric = safe_numeric(s)
        fig = px.histogram(
            pd.DataFrame({variable: numeric}),
            x=variable,
            nbins=30,
            marginal="box",
            title=f"Distribution of {variable}",
        )
    else:
        counts = (
            s.astype("string")
            .fillna("Missing")
            .value_counts()
            .rename_axis(variable)
            .reset_index(name="n")
        )
        counts["percent"] = counts["n"] / counts["n"].sum()
        counts = counts.sort_values(variable)
        fig = px.bar(
            counts,
            x="percent",
            y=variable,
            orientation="h",
            title=f"Distribution of {variable}",
        )
        fig.update_xaxes(tickformat=".0%")

    st.plotly_chart(fig, use_container_width=True)


# =============================================================================
# Page 4
# =============================================================================

elif page == "4. Correlation explorer":
    st.title("Correlation explorer")
    analysis_df = analysis_df_global.copy()

    default_vars = existing(
        analysis_df,
        ATT_POS_ITEMS + ATT_NEG_ITEMS + COMP_ALL_ITEMS,
    )
    selected = st.multiselect(
        "Observed variables",
        options=analysis_df.columns.tolist(),
        default=default_vars[:12],
    )
    method = st.selectbox(
        "Correlation method",
        ["spearman", "pearson", "kendall"],
        index=0,
    )
    min_pair_n = st.slider("Minimum pairwise sample size", 10, len(analysis_df), min(100, len(analysis_df)))

    if len(selected) >= 2:
        numeric = analysis_df[selected].apply(pd.to_numeric, errors="coerce")
        corr = numeric.corr(method=method, min_periods=min_pair_n)
        st.plotly_chart(
            make_correlation_heatmap(
                corr,
                f"{method.title()} correlations among selected observed variables",
            ),
            use_container_width=True,
        )
        st.dataframe(corr.style.format("{:.3f}"), use_container_width=True)

        pairwise_n = numeric.notna().astype(int).T.dot(numeric.notna().astype(int))
        st.subheader("Pairwise sample sizes")
        st.dataframe(pairwise_n, use_container_width=True)
    else:
        st.info("Select at least two variables.")


# =============================================================================
# Page 5
# =============================================================================

elif page == "5. CFA fit playground":
    st.title("CFA fit playground")
    analysis_df = analysis_df_global.copy()

    st.warning(
        "This web implementation uses semopy. DWLS is available as an ordinal-"
        "oriented approximation, but it is not identical to lavaan's WLSMV "
        "robust correction. Final publication models should still be confirmed "
        "in the R/lavaan pipeline."
    )

    selected_construct_names = st.multiselect(
        "Constructs to include",
        options=list(DEFAULT_CONSTRUCTS),
        default=[
            "AI competence: Awareness",
            "AI competence: Usage",
            "AI competence: Evaluation",
            "AI competence: Ethics",
        ],
    )

    selected_constructs: dict[str, list[str]] = {}
    for construct in selected_construct_names:
        available_items = existing(analysis_df, DEFAULT_CONSTRUCTS[construct])
        selected_constructs[construct] = st.multiselect(
            f"Indicators: {construct}",
            options=available_items,
            default=available_items,
            key=f"items_{construct}",
        )

    model_syntax, used_constructs = build_model_syntax(
        selected_constructs,
        analysis_df.columns.tolist(),
    )
    st.code(model_syntax or "# No estimable construct selected", language="text")

    objective = st.selectbox(
        "Estimator objective",
        ["DWLS", "MLW", "ULS"],
        index=0,
    )

    if st.button("Estimate CFA model", type="primary"):
        try:
            with st.spinner("Estimating model..."):
                st.session_state.cfa_result = run_semopy_cfa(
                    analysis_df,
                    model_syntax,
                    objective,
                )
            st.success("Model estimation completed.")
        except Exception as exc:
            st.session_state.cfa_result = None
            st.error(str(exc))

    cfa = st.session_state.cfa_result
    if cfa is not None:
        st.metric("Complete cases used", f"{cfa['n']:,}")

        diagnostics = build_fit_diagnostics(cfa["stats"])
        st.subheader("Visual interpretation of model fit")

        status_order = ["Good", "Caution", "Poor", "Unavailable", "Information"]
        diagnostics["status"] = pd.Categorical(
            diagnostics["status"],
            categories=status_order,
            ordered=True,
        )

        gauge_cols = st.columns(3)
        key_indices = ["CFI", "TLI", "RMSEA"]
        for col, index_name in zip(gauge_cols, key_indices):
            row = diagnostics.loc[diagnostics["fit_index"].eq(index_name)]
            value = row["value"].iloc[0] if not row.empty else np.nan
            interpretation = (
                row["interpretation"].iloc[0] if not row.empty else "Unavailable"
            )
            display_value = "NA" if pd.isna(value) else f"{value:.3f}"
            col.metric(index_name, display_value, interpretation)

        st.info(global_fit_conclusion(diagnostics))

        plot_diag = diagnostics.dropna(subset=["value"]).copy()
        if not plot_diag.empty:
            plot_diag["reference"] = plot_diag["fit_index"].map({
                "CFI": 0.95,
                "TLI": 0.95,
                "RMSEA": 0.08,
                "GFI": 0.90,
                "AGFI": 0.90,
                "NFI": 0.90,
                "chi2 p-value": 0.05,
            })
            fig = px.bar(
                plot_diag,
                x="fit_index",
                y="value",
                color="status",
                hover_data=["interpretation", "reference"],
                title="Model-fit profile",
                labels={"fit_index": "Fit index", "value": "Observed value"},
                category_orders={"status": status_order},
            )
            st.plotly_chart(fig, use_container_width=True)

        st.dataframe(
            diagnostics.sort_values(["status", "fit_index"]),
            use_container_width=True,
            hide_index=True,
        )

        fit_table = fit_summary_table(cfa["stats"])
        with st.expander("Complete fit-index table"):
            st.dataframe(fit_table, use_container_width=True, hide_index=True)

        estimates = cfa["estimates"].copy()
        loadings = loading_quality_table(estimates)

        st.subheader("Indicator loading diagnostics")
        if not loadings.empty:
            fig = px.bar(
                loadings.sort_values("standardized_loading"),
                x="standardized_loading",
                y="lval",
                color="loading_status",
                orientation="h",
                facet_col="rval",
                facet_col_wrap=2,
                title="Standardized loadings by factor",
                labels={
                    "lval": "Indicator",
                    "rval": "Factor",
                    "standardized_loading": "Standardized loading",
                },
            )
            fig.add_vline(x=0.50, line_dash="dash")
            fig.update_layout(height=max(550, 30 * len(loadings)))
            st.plotly_chart(fig, use_container_width=True)

            weak = loadings.loc[loadings["absolute_loading"] < 0.50]
            if not weak.empty:
                st.warning(
                    f"{len(weak)} indicator(s) have absolute standardized "
                    "loadings below 0.50. Review wording, coding direction, and "
                    "construct relevance before considering exclusion."
                )
            else:
                st.success("All available standardized loadings are at least 0.50.")

            st.dataframe(
                loadings[
                    [
                        "lval", "rval", "standardized_loading",
                        "absolute_loading", "loading_status",
                    ]
                ],
                use_container_width=True,
                hide_index=True,
            )
        else:
            st.info("Standardized loading estimates were not available.")

        st.subheader("Observed-variable residual proxy")
        st.caption(
            "This diagnostic compares the observed correlation matrix with a "
            "simple loading-based reproduced matrix. It is exploratory and does "
            "not replace lavaan residual diagnostics."
        )
        try:
            observed = (
                analysis_df[cfa["indicators"]]
                .apply(pd.to_numeric, errors="coerce")
                .corr()
            )
            st.plotly_chart(
                make_correlation_heatmap(
                    observed,
                    "Observed correlations among CFA indicators",
                ),
                use_container_width=True,
            )
        except Exception as exc:
            st.warning(f"Observed correlation plot unavailable: {exc}")

        st.subheader("Additional recommended analyses")
        tab1, tab2, tab3 = st.tabs(
            ["Reliability", "Construct correlations", "Sensitivity guidance"]
        )

        with tab1:
            reliability_rows = []
            for construct_name, indicators in used_constructs.items():
                valid = existing(analysis_df, indicators)
                reliability_rows.append({
                    "construct": construct_name,
                    "items": len(valid),
                    "complete_cases": int(
                        analysis_df[valid]
                        .apply(pd.to_numeric, errors="coerce")
                        .dropna()
                        .shape[0]
                    ) if valid else 0,
                    "cronbach_alpha": cronbach_alpha(analysis_df[valid])
                    if len(valid) >= 2 else np.nan,
                })
            reliability_df = pd.DataFrame(reliability_rows)
            st.dataframe(
                reliability_df.style.format({"cronbach_alpha": "{:.3f}"}),
                use_container_width=True,
                hide_index=True,
            )
            if not reliability_df.empty:
                fig = px.bar(
                    reliability_df,
                    x="construct",
                    y="cronbach_alpha",
                    title="Reliability by construct",
                    labels={"cronbach_alpha": "Cronbach's alpha", "construct": ""},
                )
                fig.add_hline(y=0.70, line_dash="dash")
                st.plotly_chart(fig, use_container_width=True)

        with tab2:
            scores = construct_score_frame(analysis_df)
            if scores.shape[1] >= 2:
                score_corr = scores.corr(method="spearman")
                st.plotly_chart(
                    make_correlation_heatmap(
                        score_corr,
                        "Spearman correlations among construct scores",
                    ),
                    use_container_width=True,
                )
                st.dataframe(
                    score_corr.style.format("{:.3f}"),
                    use_container_width=True,
                )
            else:
                st.info("At least two construct scores are required.")

        with tab3:
            st.markdown(
                """
                Compare models by changing one decision at a time:

                1. attention-check threshold;
                2. sparse-row exclusion threshold;
                3. reverse-coding assumptions;
                4. one theoretically questionable indicator;
                5. correlated-factor versus more parsimonious specifications.

                Prefer theoretically defensible stability over the numerically
                best result. Final ordinal CFA inference should be reproduced
                with lavaan and WLSMV.
                """
            )

        with st.expander("All parameter estimates"):
            st.dataframe(estimates, use_container_width=True, hide_index=True)


# =============================================================================
# Page 6
# =============================================================================

elif page == "6. Score quadrants":
    st.title("Positive attitude and negative concern quadrants")
    analysis_df = analysis_df_global.copy()

    selected_positive = st.multiselect(
        "Positive-attitude indicators (x-axis)",
        options=existing(analysis_df, ATT_POS_ITEMS),
        default=existing(analysis_df, ATT_POS_ITEMS),
    )
    selected_concern = st.multiselect(
        "Negative-concern indicators in original direction (y-axis)",
        options=existing(analysis_df, ATT_NEG_ITEMS),
        default=existing(analysis_df, ATT_NEG_ITEMS),
    )

    cut_method = st.radio(
        "Quadrant cut",
        ["Theoretical mean (3)", "Sample mean (exploratory)", "Median (exploratory)"],
        horizontal=True,
    )
    color_variable = st.selectbox(
        "Optional grouping variable",
        ["None"] + existing(
            analysis_df,
            ["gender", "occupation", "study_work_area", "ai_use_frequency", "internet_quality"],
        ),
    )

    if selected_positive and selected_concern:
        analysis_df["positive_attitude_score"] = (
            analysis_df[selected_positive].apply(pd.to_numeric, errors="coerce").mean(axis=1)
        )
        analysis_df["negative_concern_score"] = (
            analysis_df[selected_concern].apply(pd.to_numeric, errors="coerce").mean(axis=1)
        )

        plot_df = analysis_df.dropna(
            subset=["positive_attitude_score", "negative_concern_score"]
        ).copy()
        if cut_method == "Theoretical mean (3)":
            x_cut = 3.0
            y_cut = 3.0
        elif cut_method == "Sample mean (exploratory)":
            x_cut = plot_df["positive_attitude_score"].mean()
            y_cut = plot_df["negative_concern_score"].mean()
        else:
            x_cut = plot_df["positive_attitude_score"].median()
            y_cut = plot_df["negative_concern_score"].median()

        plot_df["quadrant"] = np.select(
            [
                (plot_df["positive_attitude_score"] >= x_cut) & (plot_df["negative_concern_score"] >= y_cut),
                (plot_df["positive_attitude_score"] >= x_cut) & (plot_df["negative_concern_score"] < y_cut),
                (plot_df["positive_attitude_score"] < x_cut) & (plot_df["negative_concern_score"] >= y_cut),
            ],
            [
                "Positive but concerned",
                "Enthusiastic and low-concern",
                "Cautious or skeptical",
            ],
            default="Low-engagement and low-concern",
        )

        color = None if color_variable == "None" else color_variable
        fig = px.scatter(
            plot_df,
            x="positive_attitude_score",
            y="negative_concern_score",
            color=color,
            symbol="quadrant",
            hover_data=existing(plot_df, ["id", "city", "occupation", "study_work_area"]),
            opacity=0.65,
            title="Positive attitude and negative concern",
        )
        fig.add_vline(x=x_cut, line_dash="dash")
        fig.add_hline(y=y_cut, line_dash="dash")
        st.plotly_chart(fig, use_container_width=True)

        counts = (
            plot_df["quadrant"]
            .value_counts()
            .rename_axis("quadrant")
            .reset_index(name="participants")
        )
        st.dataframe(counts, use_container_width=True, hide_index=True)
    else:
        st.info("Select at least one positive-attitude and one negative-concern indicator.")


# =============================================================================
# Page 7
# =============================================================================

elif page == "7. Export and audit":
    st.title("Export and audit")
    result = st.session_state.filtered_result

    if result is None:
        st.info("Apply a filter configuration before exporting an analysis sample.")
        analysis_df = df
        filter_audit = pd.DataFrame()
        row_flags = pd.DataFrame()
    else:
        analysis_df = result.data
        filter_audit = result.audit
        row_flags = result.row_flags

    initial_audit = pd.DataFrame(st.session_state.initial_audit)
    if not initial_audit.empty:
        initial_audit.insert(0, "timestamp", datetime.now().isoformat(timespec="seconds"))

    full_audit = pd.concat(
        [initial_audit, filter_audit],
        ignore_index=True,
        sort=False,
    )

    st.subheader("Transformation audit")
    st.dataframe(full_audit, use_container_width=True, hide_index=True)

    st.download_button(
        "Download filtered data as CSV",
        data=analysis_df.to_csv(index=False).encode("utf-8"),
        file_name="enfermeria_filtered_data.csv",
        mime="text/csv",
    )

    workbook = to_excel_bytes({
        "Filtered_data": analysis_df,
        "Transformation_audit": full_audit,
        "Row_flags": row_flags,
        "Missingness": missingness_table(analysis_df),
        "Attention_key_validation": attention_key_validation_table(df),
        "Attention_total_distribution": (
            recalculate_attention_checks(df)["ac_total_recalculated"]
            .value_counts()
            .sort_index()
            .rename_axis("ac_total_recalculated")
            .reset_index(name="n")
        ),
    })
    st.download_button(
        "Download complete audit workbook",
        data=workbook,
        file_name="enfermeria_playground_export.xlsx",
        mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    )

    config = {
        "exported_at": datetime.now().isoformat(timespec="seconds"),
        "source_rows": len(df),
        "analysis_rows": len(analysis_df),
        "columns": analysis_df.columns.tolist(),
    }
    st.download_button(
        "Download run configuration",
        data=json.dumps(config, indent=2).encode("utf-8"),
        file_name="enfermeria_playground_config.json",
        mime="application/json",
    )


# =============================================================================
# Page 8
# =============================================================================

elif page == "8. Pipeline and sensitivity guide":
    st.title("How the filters and sensitivity playground work")
    st.markdown(
        """
        This page explains the difference between the primary R-pipeline sample
        and exploratory filters. The application preserves the source columns
        and creates explicit recalculated attention-check columns, so changing
        a filter does not rewrite the original data.
        """
    )

    st.subheader("R-compatible sample roles")
    st.dataframe(
        pd.DataFrame([
            {
                "sample_key": "all_core",
                "rule": "Consent/commitment core criteria only",
                "n_expected": 1211,
                "role": "Primary analysis",
            },
            {
                "sample_key": "fail_at_most_2",
                "rule": "Recalculated attention total >= 2",
                "n_expected": 1073,
                "role": "Sensitivity",
            },
            {
                "sample_key": "fail_at_most_1",
                "rule": "Recalculated attention total >= 3",
                "n_expected": 980,
                "role": "Sensitivity",
            },
            {
                "sample_key": "pass_all_4",
                "rule": "Recalculated attention total == 4",
                "n_expected": 823,
                "role": "Strict sensitivity",
            },
        ]),
        use_container_width=True,
        hide_index=True,
    )

    st.info(
        "The attention key is ac_1=1, ac_2=1, ac_3=1, ac_4=1. "
        "The original ac_total is retained for audit; filters use "
        "ac_total_recalculated."
    )

    st.subheader("What each filter implies")
    st.markdown(
        """
        - **Consent = 1:** applies the R core eligibility rule. It is a
          substantive inclusion criterion, not a model-fit optimization.
        - **Commitment = 1:** is off by default because the R pipeline uses
          `REQUIRE_COMMITMENT = FALSE`. Activating it changes the target
          population and must be justified before analysis.
        - **Attention threshold:** creates a sensitivity sample from the
          recalculated key. It should not replace `all_core` merely because a
          CFA fit index improves.
        - **Invalid Likert values:** converts values outside 1--5 to missing,
          following the R validation rule. It does not delete the entire row.
        - **Sparse-response filter:** is a playground diagnostic. It is not the
          same as CFA complete-case handling and should be reported separately.
        - **Reverse scoring:** creates analysis columns and retains raw values.
          Confirm the `_r` naming convention before applying any additional
          reversal; reversing an already reversed item would invert the result.
        """
    )

    st.subheader("Recommended sensitivity workflow")
    st.markdown(
        """
        1. Keep the primary configuration fixed: `all_core`, validated key,
           consent rule, and the R scoring direction.
        2. Change one decision at a time: attention threshold, commitment rule,
           missing-item threshold, estimator, or prespecified CFA structure.
        3. Record `n`, complete cases, reliability, factorability, CFI/TLI,
           RMSEA/SRMR, convergence, and admissibility.
        4. Treat a model as inadmissible when latent covariance checks fail,
           even if the optimizer reports convergence.
        5. Do not remove rows or items solely because the numerical fit becomes
           better. Investigate data quality, influence, content, and coding,
           then validate changes on independent or held-out data.
        """
    )

    st.subheader("Playground experiments that are appropriate")
    st.markdown(
        """
        Useful exploratory comparisons include: all four R-compatible samples;
        Pearson versus Spearman correlations; robust/ordinal versus continuous
        estimation; the prespecified 4-factor, 3-factor, and 1-factor competence
        models; and case-influence diagnostics. These experiments describe
        robustness. They do not authorize selecting the most favorable result
        after looking at the output.
        """
    )
