
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

DEFAULT_CONSTRUCTS = {
    "Positive attitudes toward AI": ATT_POS_ITEMS,
    "Negative attitudes toward AI": ATT_NEG_ITEMS,
    "AI competence: Awareness": COMP_AWARENESS,
    "AI competence: Usage": COMP_USAGE,
    "AI competence: Evaluation": COMP_EVALUATION,
    "AI competence: Ethics": COMP_ETHICS,
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

    attention_cols = existing(data, ["ac_total", "ac_competence", "ac_att"])
    if attention_mode == "Total score" and "ac_total" in attention_cols:
        criterion = safe_numeric(data["ac_total"]).ge(min_attention)
    elif attention_mode == "Both subscales" and {"ac_competence", "ac_att"}.issubset(attention_cols):
        sub_threshold = min(2, max(0, math.ceil(min_attention / 2)))
        criterion = (
            safe_numeric(data["ac_competence"]).ge(sub_threshold)
            & safe_numeric(data["ac_att"]).ge(sub_threshold)
        )
    else:
        criterion = pd.Series(True, index=data.index)

    flags["exclude_attention"] = ~criterion
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
                f"attention mode={attention_mode}; minimum attention={min_attention}; "
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
    ],
)

uploaded = st.sidebar.file_uploader(
    "Upload dataset",
    type=["xlsx", "xls", "csv"],
    help="Recommended file: ds_all_2026_07_22.xlsx",
)

if uploaded is not None:
    try:
        raw_data, initial_audit = load_data(uploaded)
        st.session_state.raw_data = raw_data
        st.session_state.initial_audit = initial_audit
        st.session_state.filtered_result = None
        st.session_state.cfa_result = None
        st.sidebar.success(
            f"Loaded {raw_data.shape[0]:,} rows and {raw_data.shape[1]:,} columns."
        )
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
    st.title("Filtering playground")

    with st.form("filter_form"):
        st.subheader("Eligibility rules")
        a, b, c = st.columns(3)
        require_consent = a.checkbox("Require consent = 1", value=True)
        require_commitment = b.checkbox("Require commitment = 1", value=False)
        remove_blank_rows = c.checkbox("Remove fully blank rows", value=True)

        attention_mode = st.radio(
            "Attention-check rule",
            ["Total score", "Both subscales", "Do not filter"],
            horizontal=True,
        )
        min_attention = st.slider(
            "Minimum attention-check score",
            min_value=0,
            max_value=4,
            value=0,
            step=1,
            disabled=attention_mode == "Do not filter",
        )

        st.subheader("Sparse-response rule")
        all_scale_items = existing(df, ATT_POS_ITEMS + ATT_NEG_ITEMS + COMP_ALL_ITEMS)
        selected_scale_items = st.multiselect(
            "Variables used to determine whether a row is too sparse",
            options=all_scale_items,
            default=all_scale_items,
        )
        min_valid_prop = st.slider(
            "Minimum proportion of selected items required",
            min_value=0.50,
            max_value=1.00,
            value=0.80,
            step=0.05,
        )

        st.subheader("Coding options")
        coerce_invalid = st.checkbox(
            "Convert values outside 1–5 to missing",
            value=True,
        )
        reverse_comp = st.multiselect(
            "Competence variables to reverse for analysis",
            options=existing(df, COMP_ALL_ITEMS),
            default=[],
            help=(
                "Leave empty when `_r` columns are already reversed. Select an "
                "item only when inspection confirms that the stored values remain "
                "in the original direction."
            ),
        )
        reverse_att_neg = st.checkbox(
            "Create favorable reverse-scored versions of negative-attitude items",
            value=True,
        )

        apply_filters = st.form_submit_button("Apply filters")

    if apply_filters:
        st.session_state.filtered_result = build_filter_result(
            df=df,
            require_consent=require_consent,
            require_commitment=require_commitment,
            min_attention=min_attention,
            attention_mode=attention_mode,
            selected_scale_items=selected_scale_items,
            min_valid_proportion=min_valid_prop,
            remove_fully_blank_rows=remove_blank_rows,
            coerce_invalid_likert=coerce_invalid,
            reverse_competence_items=reverse_comp,
            reverse_negative_attitude_items=reverse_att_neg,
        )
        st.session_state.cfa_result = None

    result = st.session_state.filtered_result
    if result is None:
        st.info("Apply a filter configuration to generate an analysis sample.")
    else:
        c1, c2, c3 = st.columns(3)
        c1.metric("Source rows", f"{len(df):,}")
        c2.metric("Retained rows", f"{len(result.data):,}")
        c3.metric("Excluded rows", f"{len(df) - len(result.data):,}")

        reason_counts = (
            result.row_flags["exclusion_reason"]
            .value_counts(dropna=False)
            .rename_axis("reason")
            .reset_index(name="rows")
        )
        fig = px.bar(
            reason_counts,
            x="rows",
            y="reason",
            orientation="h",
            title="Row disposition by exclusion reason",
        )
        st.plotly_chart(fig, use_container_width=True)
        st.dataframe(result.row_flags, use_container_width=True, hide_index=True)


# =============================================================================
# Page 3
# =============================================================================

elif page == "3. Descriptive diagnostics":
    st.title("Descriptive diagnostics")
    analysis_df = (
        st.session_state.filtered_result.data
        if st.session_state.filtered_result is not None
        else df
    )

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
    analysis_df = (
        st.session_state.filtered_result.data
        if st.session_state.filtered_result is not None
        else df
    )

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
    analysis_df = (
        st.session_state.filtered_result.data
        if st.session_state.filtered_result is not None
        else df
    )

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

        fit_table = fit_summary_table(cfa["stats"])
        st.subheader("Fit indices")
        st.dataframe(fit_table, use_container_width=True, hide_index=True)

        estimates = cfa["estimates"].copy()
        loading_mask = estimates["op"].eq("~")
        loadings = estimates.loc[loading_mask].copy()
        if "Est. Std" in loadings.columns:
            loading_col = "Est. Std"
        elif "Estimate" in loadings.columns:
            loading_col = "Estimate"
        else:
            loading_col = None

        if loading_col is not None and not loadings.empty:
            fig = px.bar(
                loadings,
                x=loading_col,
                y="lval",
                color="rval",
                orientation="h",
                title="Standardized indicator loadings",
                labels={"lval": "Indicator", "rval": "Factor"},
            )
            st.plotly_chart(fig, use_container_width=True)

        st.subheader("Parameter estimates")
        st.dataframe(estimates, use_container_width=True, hide_index=True)

        st.subheader("Sensitivity guidance")
        st.markdown(
            """
            Compare fit after changing only one analytical decision at a time:
            attention threshold, sparse-row threshold, reverse coding, or
            indicator inclusion. A change that improves fit but destroys the
            construct definition should not be retained merely because the
            numerical fit is better.
            """
        )


# =============================================================================
# Page 6
# =============================================================================

elif page == "6. Score quadrants":
    st.title("Score quadrants")
    analysis_df = (
        st.session_state.filtered_result.data.copy()
        if st.session_state.filtered_result is not None
        else df.copy()
    )

    att_items = existing(analysis_df, ATT_POS_ITEMS)
    comp_items = existing(analysis_df, COMP_ALL_ITEMS)

    selected_att = st.multiselect(
        "Attitude indicators",
        options=existing(analysis_df, ATT_POS_ITEMS + ATT_NEG_ITEMS + [f"{x}_r" for x in ATT_NEG_ITEMS]),
        default=att_items,
    )
    selected_comp = st.multiselect(
        "Competence indicators",
        options=existing(analysis_df, COMP_ALL_ITEMS + [f"{x}__analysis" for x in COMP_ALL_ITEMS]),
        default=comp_items,
    )

    cut_method = st.radio("Quadrant cut", ["Mean", "Median"], horizontal=True)
    color_variable = st.selectbox(
        "Optional grouping variable",
        ["None"] + existing(
            analysis_df,
            ["gender", "occupation", "study_work_area", "ai_use_frequency", "internet_quality"],
        ),
    )

    if selected_att and selected_comp:
        analysis_df["attitude_score"] = (
            analysis_df[selected_att].apply(pd.to_numeric, errors="coerce").mean(axis=1)
        )
        analysis_df["competence_score"] = (
            analysis_df[selected_comp].apply(pd.to_numeric, errors="coerce").mean(axis=1)
        )

        plot_df = analysis_df.dropna(subset=["attitude_score", "competence_score"]).copy()
        if cut_method == "Mean":
            x_cut = plot_df["attitude_score"].mean()
            y_cut = plot_df["competence_score"].mean()
        else:
            x_cut = plot_df["attitude_score"].median()
            y_cut = plot_df["competence_score"].median()

        plot_df["quadrant"] = np.select(
            [
                (plot_df["attitude_score"] >= x_cut) & (plot_df["competence_score"] >= y_cut),
                (plot_df["attitude_score"] < x_cut) & (plot_df["competence_score"] >= y_cut),
                (plot_df["attitude_score"] < x_cut) & (plot_df["competence_score"] < y_cut),
            ],
            [
                "AI-ready advocates",
                "Competent but cautious",
                "Skeptical and underprepared",
            ],
            default="Positive but capability-limited",
        )

        color = None if color_variable == "None" else color_variable
        fig = px.scatter(
            plot_df,
            x="attitude_score",
            y="competence_score",
            color=color,
            symbol="quadrant",
            hover_data=existing(plot_df, ["id", "city", "occupation", "study_work_area"]),
            opacity=0.65,
            title="Attitudes toward AI and competence in using AI",
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
        st.info("Select at least one attitude and one competence indicator.")


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
