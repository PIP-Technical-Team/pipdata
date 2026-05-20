# Pre-Arrow Cleaning & Standardisation
# Spec:  docs/pre-arrow-cleaning-spec.md
# Schema: inst/schema/arrow-schema.json  (in {piptm})
# Plan:   .cg-docs/plans/2026-05-18-deflated-data-arrow-partitions.md  (Phase 1)
#
# Transforms a deflated survey data.table (from pipload::load_pip_deflated_data())
# into a schema-conformant data.table ready for arrow::write_parquet().
# Metadata (country_code, surveyid_year, welfare_type, version) is read from
# dataset attributes — no separate metadata list is required.
# Multiple welfare columns (welfare_lcu, welfare_ppp_*) are preserved as
# discovered from the welfare_vars attribute.
#
# Exported functions
# ------------------
#   prepare_for_arrow()      — orchestrator: calls all helpers in order
#
# Internal helpers (not exported)
# --------------------------------
#   inject_metadata_cols()   — §1.1: dataset attributes → data.table columns
#   cast_data_cols()         — §2:   type casting all welfare_* cols + weight
#   standardize_gender()     — §3.1: gender factor
#   standardize_area()       — §3.2: area factor
#   standardize_education()  — §3.3: education ordered factor
#   standardize_age()        — §3.4: age integer
#   validate_pre_write()     — §4:   all pre-write checks

# ---------------------------------------------------------------------------
# §1.1 — Metadata injection
# ---------------------------------------------------------------------------

#' Inject survey metadata as constant columns into a microdata data.table
#'
#' Adds `country_code`, `surveyid_year`, `welfare_type`, `pip_id`,
#' and `version` columns by reference. Metadata is read from the dataset
#' attributes set by `pipload::load_pip_deflated_data()` — no separate
#' metadata list is required.
#'
#' `welfare_type` is recoded from the full string (`"income"` /
#' `"consumption"`) to the two-letter code (`"INC"` / `"CON"`). Unknown
#' values are passed through after `toupper()`.
#'
#' @param dt     A `data.table` of deflated survey microdata as returned by
#'   `pipload::load_pip_deflated_data()`. Must carry `country_code`,
#'   `surveyid_year`, `welfare_type`, `vermast`, and `veralt` as attributes.
#'   Modified **by reference**.
#' @param pip_id The canonical `pip_id` string for this specific survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the release inventory.
#'   Used as the Parquet filename stem.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
inject_metadata_cols <- function(dt, pip_id) {
  required_attrs <- c("country_code", "surveyid_year", "welfare_type",
                      "vermast", "veralt")
  missing_attrs <- required_attrs[
    vapply(required_attrs, function(a) is.null(attr(dt, a)), logical(1L))
  ]
  if (length(missing_attrs) > 0L) {
    cli::cli_abort(
      c(
        "Dataset is missing required attribute(s): {.field {missing_attrs}}",
        "i" = "Pass data loaded via {.fn pipload::load_pip_deflated_data}."
      )
    )
  }
  if (!is.character(pip_id) || length(pip_id) != 1L || is.na(pip_id)) {
    cli::cli_abort(
      "{.arg pip_id} must be a single non-NA character string."
    )
  }

  wt_raw <- toupper(as.character(attr(dt, "welfare_type")))

  dt[, `:=`(
    country_code  = toupper(as.character(attr(dt, "country_code"))),
    surveyid_year = as.integer(attr(dt, "surveyid_year")),
    welfare_type  = data.table::fcase(
      wt_raw == "INCOME",      "INC",
      wt_raw == "CONSUMPTION", "CON",
      default = wt_raw
    ),
    pip_id        = as.character(pip_id),
    version       = paste0(
      tolower(as.character(attr(dt, "vermast"))), "_",
      tolower(as.character(attr(dt, "veralt")))
    )
  )]

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §2 — Type casting for data-sourced columns
# ---------------------------------------------------------------------------

#' Cast all welfare columns and weight to double
#'
#' Discovers welfare columns from the `welfare_vars` attribute of `dt` and
#' casts all of them to `double` in-place. Also casts `weight` to `double`.
#'
#' @param dt A `data.table` of deflated survey microdata with a `welfare_vars`
#'   attribute (set by `pipload::load_pip_deflated_data()`). Modified
#'   **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
cast_data_cols <- function(dt) {
  wv <- attr(dt, "welfare_vars")
  if (is.null(wv) || length(wv) == 0L) {
    cli::cli_abort(
      c(
        "Dataset is missing a non-empty {.field welfare_vars} attribute.",
        "i" = "Pass data loaded via {.fn pipload::load_pip_deflated_data}."
      )
    )
  }
  missing_wv <- setdiff(wv, names(dt))
  if (length(missing_wv) > 0L) {
    cli::cli_abort(
      "welfare_vars attribute lists column(s) not found in data: {.field {missing_wv}}"
    )
  }
  if (!"weight" %in% names(dt)) {
    cli::cli_abort("Column {.field weight} not found in data.")
  }

  dt[, (wv)   := lapply(.SD, as.double), .SDcols = wv]
  dt[, weight := as.double(weight)]

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §3.1 — Gender standardisation
# ---------------------------------------------------------------------------

#' Standardise the gender column to a factor
#'
#' Derives a `gender` factor column with levels `c("male", "female")`.
#' Priority: existing `gender` character column, then `male` integer
#' fallback. If neither is present the function returns `dt` unchanged
#' (the column is omitted by the orchestrator if still absent).
#'
#' @param dt A `data.table` of survey microdata. Modified **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
standardize_gender <- function(dt) {
  lvls <- c("male", "female")

  if ("gender" %in% names(dt)) {
    dt[, gender := factor(
      tolower(trimws(as.character(gender))),
      levels = lvls
    )]
  } else if ("male" %in% names(dt)) {
    dt[, gender := factor(
      data.table::fcase(
        male == 1L, "male",
        male == 0L, "female",
        default = NA_character_
      ),
      levels = lvls
    )]
  }
  # If neither column is present: no-op — orchestrator will omit.

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §3.2 — Area standardisation
# ---------------------------------------------------------------------------

#' Standardise the area column to a factor
#'
#' Derives an `area` factor column with levels `c("urban", "rural")`.
#' Priority: existing `area` character column, then `urban` integer fallback.
#' If neither is present the function returns `dt` unchanged.
#'
#' @param dt A `data.table` of survey microdata. Modified **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
standardize_area <- function(dt) {
  lvls <- c("urban", "rural")

  if ("area" %in% names(dt)) {
    dt[, area := factor(
      tolower(trimws(as.character(area))),
      levels = lvls
    )]
  } else if ("urban" %in% names(dt)) {
    dt[, area := factor(
      data.table::fcase(
        urban == 1L, "urban",
        urban == 0L, "rural",
        default = NA_character_
      ),
      levels = lvls
    )]
  }

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §3.3 — Education standardisation
# ---------------------------------------------------------------------------

#' Standardise education columns to factors
#'
#' Ensures any of `educat4`, `educat5`, `educat7` present in `dt` are
#' factors, preserving their original levels as-is. No recoding or renaming
#' is applied. If none are present the function is a no-op.
#'
#' @param dt A `data.table` of survey microdata. Modified **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
standardize_education <- function(dt) {
  edu_cols <- intersect(c("educat4", "educat5", "educat7"), names(dt))

  for (col in edu_cols) {
    if (!is.factor(dt[[col]])) {
      dt[, (col) := factor(get(col))]
    }
  }

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §3.4 — Age standardisation
# ---------------------------------------------------------------------------

#' Cast age to integer
#'
#' Casts the `age` column to `integer` in-place. If `age` is absent the
#' function is a no-op.
#'
#' @param dt A `data.table` of survey microdata. Modified **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
standardize_age <- function(dt) {
  if ("age" %in% names(dt)) {
    dt[, age := as.integer(age)]
  }
  invisible(dt)
}


# ---------------------------------------------------------------------------
# §4 — Pre-write validation
# ---------------------------------------------------------------------------

#' Validate a prepared data.table before writing to Parquet
#'
#' Runs all checks defined in §4 of the pre-arrow-cleaning spec. Aborts with
#' a descriptive error on any hard failure. Emits a warning for zero-welfare
#' observations (permitted, but notable).
#'
#' Welfare columns are discovered from the `welfare_vars` attribute of `dt`
#' (set by `prepare_for_arrow()`). Each welfare column is validated
#' independently.
#'
#' @param dt A prepared `data.table` (output of [prepare_for_arrow()]).
#'   Must carry `welfare_vars` and optionally `ppp_sort` as attributes.
#'
#' @return `TRUE` invisibly when all checks pass.
#' @keywords internal
validate_pre_write <- function(dt) {

  # Discover welfare columns from attribute ----------------------------------
  welfare_vars <- attr(dt, "welfare_vars")
  if (is.null(welfare_vars) || length(welfare_vars) == 0L) {
    cli::cli_abort(
      c(
        "Dataset is missing a non-empty {.field welfare_vars} attribute.",
        "i" = "This attribute is set by {.fn prepare_for_arrow}."
      )
    )
  }

  # §4.1 — Required columns present ------------------------------------------
  fixed_required <- c(
    "country_code", "surveyid_year", "welfare_type", "pip_id",
    "weight", "version"
  )
  missing_cols <- setdiff(c(fixed_required, welfare_vars), names(dt))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Required columns missing from prepared data: {.field {missing_cols}}"
    )
  }

  # §4.2 — Partition key consistency ------------------------------------------
  for (key_col in c("country_code", "surveyid_year", "welfare_type")) {
    n_unique <- dt[, data.table::uniqueN(get(key_col))]
    if (n_unique != 1L) {
      cli::cli_abort(
        paste0(
          "Partition key {.field {key_col}} must be constant within one file",
          " but found {n_unique} distinct value(s)."
        )
      )
    }
  }

  # §4.3 — Welfare type values ------------------------------------------------
  wt_vals    <- dt[, unique(welfare_type)]
  invalid_wt <- setdiff(wt_vals, c("INC", "CON"))
  if (length(invalid_wt) > 0L) {
    cli::cli_abort(
      "welfare_type must be 'INC' or 'CON'; found: {.val {invalid_wt}}"
    )
  }

  # §4.4 — Welfare validity for each welfare column --------------------------
  for (wc in welfare_vars) {
    n_zero <- dt[, sum(get(wc) == 0, na.rm = TRUE)]
    if (n_zero > 0L) {
      rlang::warn(
        paste0(n_zero, " row(s) have ", wc, " == 0 in survey: ", dt[1L, pip_id])
      )
    }
    if (!dt[, all(is.finite(get(wc)))]) {
      cli::cli_abort(
        "{.field {wc}} contains non-finite values (Inf / NaN / NA). All welfare values must be finite."
      )
    }
    if (!dt[, all(get(wc) >= 0)]) {
      cli::cli_abort(
        "{.field {wc}} contains negative values. Negative welfare is not permitted."
      )
    }
  }

  # §4.5 — Weight validity ----------------------------------------------------
  if (dt[, any(is.na(weight))]) {
    cli::cli_abort("weight contains NA values.")
  }
  if (!dt[, all(is.finite(weight))]) {
    cli::cli_abort("weight contains non-finite values (Inf / NaN).")
  }
  if (!dt[, all(weight > 0)]) {
    cli::cli_abort("weight contains non-positive values. Weights must be strictly > 0.")
  }

  # §4.6 — Country code format ------------------------------------------------
  if (!dt[, all(grepl("^[A-Z]{3}$", country_code))]) {
    bad <- dt[!grepl("^[A-Z]{3}$", country_code), unique(country_code)]
    cli::cli_abort(
      "country_code does not match ISO3 format [A-Z]{{3}}: {.val {bad}}"
    )
  }

  # §4.7 — Factor level conformance -------------------------------------------
  valid_gender <- c("male", "female")
  valid_area   <- c("urban", "rural")

  if ("gender" %in% names(dt)) {
    bad <- dt[!is.na(gender) & !as.character(gender) %in% valid_gender,
              unique(as.character(gender))]
    if (length(bad) > 0L) {
      cli::cli_abort(
        "gender has values outside allowed levels {.val {valid_gender}}: {.val {bad}}"
      )
    }
  }
  if ("area" %in% names(dt)) {
    bad <- dt[!is.na(area) & !as.character(area) %in% valid_area,
              unique(as.character(area))]
    if (length(bad) > 0L) {
      cli::cli_abort(
        "area has values outside allowed levels {.val {valid_area}}: {.val {bad}}"
      )
    }
  }
  for (edu_col in c("educat4", "educat5", "educat7")) {
    if (edu_col %in% names(dt) && !is.factor(dt[[edu_col]])) {
      cli::cli_abort("{.field {edu_col}} must be a factor.")
    }
  }

  # §4.8 — No extra columns ---------------------------------------------------
  optional_dim_cols <- c("gender", "area", "educat4", "educat5", "educat7", "age")
  fixed_cols        <- c("country_code", "surveyid_year", "welfare_type", "pip_id",
                         "weight", "version")
  allowed_cols      <- c(fixed_cols, welfare_vars, optional_dim_cols)
  extra             <- setdiff(names(dt), allowed_cols)
  if (length(extra) > 0L) {
    cli::cli_abort(
      "Unexpected columns in output: {.val {extra}}. Drop before writing."
    )
  }

  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

#' Prepare a deflated survey data.table for Arrow / Parquet writing
#'
#' Transforms a deflated survey `data.table` (from
#' `pipload::load_pip_deflated_data()`) into a schema-conformant `data.table`
#' ready to be passed directly to [write_survey_parquet()].
#'
#' The input must carry the following attributes (set by
#' `load_pip_deflated_data()`):
#' - `welfare_vars` — character vector of all welfare column names (e.g.
#'   `c("welfare_lcu", "welfare_ppp_2017_01_02", "welfare_ppp_2021_01_02")`).
#' - `ppp_sort` — integer; base year used for row sorting (e.g. `2017L`).
#' - `country_code`, `surveyid_year`, `welfare_type`, `vermast`, `veralt` —
#'   survey identity scalars.
#'
#' The function applies, in order:
#' 1. Metadata injection (§1.1) — adds `country_code`, `surveyid_year`,
#'    `welfare_type`, `pip_id`, `version` as constant columns from attributes.
#' 2. Type casting (§2) — all `welfare_*` columns and `weight` to `double`.
#' 3. Breakdown dimension standardisation (§3) — `gender`, `area`,
#'    `education`, `age` are derived/normalised where source columns exist.
#' 4. Column selection — only schema-allowed columns are retained; all
#'    others are dropped.  Optional breakdown columns that are entirely `NA`
#'    after standardisation are also dropped (they must be absent, not all-NA).
#' 5. Pre-write validation (§4) — aborts with a descriptive error on any
#'    schema or data-quality violation.
#'
#' The `welfare_vars` and `ppp_sort` attributes are preserved on the output
#' data.table for downstream use by manifest generation.
#'
#' The input `data.table` is **copied** before transformation; the original
#' object passed by the caller is not modified.
#'
#' @param data   A `data.table` as returned by
#'   `pipload::load_pip_deflated_data()`. Must carry `welfare_vars`,
#'   `ppp_sort`, `country_code`, `surveyid_year`, `welfare_type`, `vermast`,
#'   and `veralt` as attributes, plus `weight` and all welfare columns listed
#'   in `welfare_vars` as data columns.
#' @param pip_id The canonical `pip_id` string for this specific survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the `pip_id` column of
#'   the release inventory. Used as the Parquet filename stem.
#'
#' @return A new `data.table` containing only schema-allowed columns
#'   (`welfare_*`, `weight`, partition keys, and available breakdown
#'   dimensions), with `welfare_vars` and `ppp_sort` preserved as attributes.
#'   Ready for [write_survey_parquet()].
#' @seealso [write_survey_parquet()], [generate_arrow_dataset()]
#' @family arrow-prep
#' @export
#' @examples
#' \dontrun{
#' inv    <- pipload::load_pip_master_inventory()
#' pip    <- inv[pip_id == "ARG_2003_EPHC-S2_INC_ALL", pip_id]
#' defl   <- pipload::load_pip_deflated_data(id_name = pip)
#' dt     <- prepare_for_arrow(defl, pip_id = pip)
#' }
prepare_for_arrow <- function(data, pip_id) {
  if (!data.table::is.data.table(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.table}, not {.cls {class(data)[[1L]]}}."
    )
  }

  # Capture welfare_vars and ppp_sort before copying (attributes survive copy)
  wv       <- attr(data, "welfare_vars")
  ppp_sort <- attr(data, "ppp_sort")

  if (is.null(wv) || length(wv) == 0L) {
    cli::cli_abort(
      c(
        "{.arg data} is missing a non-empty {.field welfare_vars} attribute.",
        "i" = "Pass data loaded via {.fn pipload::load_pip_deflated_data}."
      )
    )
  }

  # Work on a copy so the caller's object is not modified by reference
  dt <- data.table::copy(data)

  # ---- Step 1 & 2: inject metadata and cast welfare/weight columns ---------
  inject_metadata_cols(dt, pip_id)
  cast_data_cols(dt)

  # ---- Step 3: breakdown dimension standardisation -------------------------
  standardize_gender(dt)
  standardize_area(dt)
  standardize_education(dt)
  standardize_age(dt)

  # ---- Step 4: column selection --------------------------------------------
  optional_dim_cols <- c("gender", "area", "educat4", "educat5", "educat7", "age")
  fixed_cols        <- c("country_code", "surveyid_year", "welfare_type",
                         "pip_id", "weight", "version")
  allowed_cols      <- c(fixed_cols, wv, optional_dim_cols)

  # Drop columns not in the allowed set
  extra_cols <- setdiff(names(dt), allowed_cols)
  if (length(extra_cols) > 0L) {
    dt[, (extra_cols) := NULL]
  }

  # Drop welfare columns that are entirely non-finite (all NA / Inf / NaN).
  # This can occur when a PPP reference year has no valid deflation for a
  # survey (e.g. welfare_ppp_2005_01_01 for surveys that pre-date that ICP
  # round). The column is dropped and the welfare_vars attribute updated.
  # Abort only if no welfare column survives.
  wv_bad <- wv[vapply(wv, function(col) {
    col %in% names(dt) && dt[, !any(is.finite(get(col)))]
  }, logical(1L))]
  if (length(wv_bad) > 0L) {
    rlang::warn(
      paste0(
        "Dropping welfare column(s) with no finite values: ",
        paste(wv_bad, collapse = ", "),
        ". Survey: ", pip_id
      )
    )
    dt[, (wv_bad) := NULL]
    wv <- setdiff(wv, wv_bad)
  }
  if (length(wv) == 0L) {
    cli::cli_abort(
      c(
        "All welfare columns were dropped for survey {.val {pip_id}}.",
        "i" = "No finite welfare values found in any of the deflated welfare columns."
      )
    )
  }

  # Drop optional breakdown columns that are entirely NA (must be absent, not all-NA)
  dim_cols_present <- intersect(optional_dim_cols, names(dt))
  for (col in dim_cols_present) {
    if (dt[, all(is.na(get(col)))]) {
      rlang::inform(
        paste0(
          "Omitting column '", col,
          "': all values are NA after standardisation."
        )
      )
      dt[, (col) := NULL]
    }
  }

  # ---- Restore attributes on output (needed by write and manifest steps) ---
  data.table::setattr(dt, "welfare_vars", wv)
  data.table::setattr(dt, "ppp_sort",     ppp_sort)

  # ---- Step 5: pre-write validation ----------------------------------------
  validate_pre_write(dt)

  dt[]
}
