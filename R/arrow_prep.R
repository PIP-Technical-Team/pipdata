# Pre-Arrow Cleaning & Standardisation
# Spec:  docs/pre-arrow-cleaning-spec.md
# Schema: inst/schema/arrow-schema.json  (in {piptm})
#
# Transforms a {pipdata} clean survey data.table + metadata list into a
# schema-conformant data.table ready for arrow::write_parquet().
#
# Exported functions
# ------------------
#   prepare_for_arrow()      — orchestrator: calls all helpers in order
#
# Internal helpers (not exported)
# --------------------------------
#   inject_metadata_cols()   — §1.1: metadata → data.table columns
#   cast_data_cols()         — §2:   type casting welfare, weight
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
#' Adds `country_code`, `surveyid_year`, `survey_acronym`, `welfare_type`,
#' and `survey_id` columns by reference. `welfare_type` is recoded from the
#' full string (`"income"` / `"consumption"`) to the two-letter code
#' (`"INC"` / `"CON"`). Unknown values are passed through after `toupper()`.
#'
#' The `survey_id` column in the output holds the **`pip_id`** value (e.g.
#' `"ARG_2003_EPHC-S2_INC_ALL"`), not the raw `survey_id` string. This is
#' because `pip_id` uniquely identifies a single physical file (one welfare
#' type), whereas `survey_id` can map to multiple files (INC + CON). The
#' Parquet filename and partition path are derived from this column.
#'
#' @param dt       A `data.table` of survey microdata. Modified **by reference**.
#' @param metadata Named list returned by
#'   `pipload::load_pip_data(..., metadata = TRUE)`.  Must contain
#'   `country_code`, `surveyid_year`, `survey_acronym`, and `welfare_type`.
#' @param pip_id   The canonical `pip_id` string for this specific survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the release inventory.
#'   This is stored in the `survey_id` column of the output data.table and
#'   used as the Parquet filename stem.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
inject_metadata_cols <- function(dt, metadata, pip_id) {
  required_fields <- c(
    "country_code", "surveyid_year", "survey_acronym", "welfare_type",
    "vermast", "veralt"
  )
  missing_fields <- setdiff(required_fields, names(metadata))
  if (length(missing_fields) > 0L) {
    cli::cli_abort(
      "metadata is missing required field(s): {.field {missing_fields}}"
    )
  }
  if (!is.character(pip_id) || length(pip_id) != 1L || is.na(pip_id)) {
    cli::cli_abort(
      "{.arg pip_id} must be a single non-NA character string."
    )
  }

  wt_raw <- toupper(metadata$welfare_type)

  dt[, `:=`(
    country_code   = toupper(as.character(metadata$country_code)),
    surveyid_year  = as.integer(metadata$surveyid_year),
    survey_acronym = toupper(as.character(metadata$survey_acronym)),
    welfare_type   = data.table::fcase(
      wt_raw == "INCOME",      "INC",
      wt_raw == "CONSUMPTION", "CON",
      default = wt_raw
    ),
    survey_id      = as.character(pip_id),
    version        = paste0(
      tolower(as.character(metadata$vermast)), "_",
      tolower(as.character(metadata$veralt))
    )
  )]

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §2 — Type casting for data-sourced columns
# ---------------------------------------------------------------------------

#' Cast welfare and weight columns to double
#'
#' Casts `welfare` and `weight` to `double` in-place. Both columns must
#' exist in `dt`.
#'
#' @param dt A `data.table` of survey microdata. Modified **by reference**.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
cast_data_cols <- function(dt) {
  if (!"welfare" %in% names(dt)) {
    cli::cli_abort("Column {.field welfare} not found in data.")
  }
  if (!"weight" %in% names(dt)) {
    cli::cli_abort("Column {.field weight} not found in data.")
  }

  dt[, `:=`(
    welfare = as.double(welfare),
    weight  = as.double(weight)
  )]

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
#' @param dt A prepared `data.table` (output of [prepare_for_arrow()]).
#'
#' @return `TRUE` invisibly when all checks pass.
#' @keywords internal
validate_pre_write <- function(dt) {

  # §4.1 — Required columns present ------------------------------------------
  required_cols <- c(
    "country_code", "surveyid_year", "welfare_type", "survey_id",
    "survey_acronym", "welfare", "weight", "version"
  )
  missing_cols <- setdiff(required_cols, names(dt))
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

  # §4.4 — Welfare validity ---------------------------------------------------
  n_zero <- dt[, sum(welfare == 0, na.rm = TRUE)]
  if (n_zero > 0L) {
    rlang::warn(
      paste0(
        n_zero, " row(s) have welfare == 0 in survey: ",
        dt[1L, survey_id]
      )
    )
  }
  if (!dt[, all(is.finite(welfare))]) {
    cli::cli_abort(
      "welfare contains non-finite values (Inf / NaN / NA). All welfare values must be finite."
    )
  }
  if (!dt[, all(welfare >= 0)]) {
    cli::cli_abort("welfare contains negative values. Negative welfare is not permitted.")
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
  allowed_cols <- c(
    "country_code", "surveyid_year", "welfare_type", "survey_id",
    "survey_acronym", "welfare", "weight", "version",
    "gender", "area", "educat4", "educat5", "educat7", "age"
  )
  extra <- setdiff(names(dt), allowed_cols)
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

#' Prepare a survey data.table for Arrow / Parquet writing
#'
#' Transforms a {pipdata} clean survey `data.table` and its associated
#' metadata list into a schema-conformant `data.table` ready to be passed
#' directly to [write_survey_parquet()] (or `arrow::write_parquet()`).
#'
#' The function applies, in order:
#' 1. Metadata injection (§1.1) — adds `country_code`, `surveyid_year`,
#'    `survey_acronym`, `welfare_type`, `survey_id` as constant columns.
#' 2. Type casting (§2) — `welfare` and `weight` to `double`.
#' 3. Breakdown dimension standardisation (§3) — `gender`, `area`,
#'    `education`, `age` are derived/normalised where source columns exist.
#' 4. Column selection — only schema-allowed columns are retained; all
#'    others are dropped.  Optional breakdown columns that are entirely `NA`
#'    after standardisation are also dropped (they must be absent, not all-NA).
#' 5. Pre-write validation (§4) — aborts with a descriptive error on any
#'    schema or data-quality violation.
#'
#' The input `data.table` is **copied** before transformation; the original
#' object passed by the caller is not modified.
#'
#' @param data     A `data.table` of row-level survey microdata as returned by
#'   `pipload::load_pip_data(..., metadata = FALSE)`. Must contain at least
#'   `welfare` and `weight` columns.
#' @param metadata Named list of survey identifiers as returned by
#'   `pipload::load_pip_data(..., metadata = TRUE)`. Must contain
#'   `country_code`, `surveyid_year`, `survey_acronym`, and `welfare_type`.
#' @param pip_id   The canonical `pip_id` string for this specific survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the `pip_id` column of
#'   the release inventory. Stored in the `survey_id` column of the output
#'   data.table and used as the Parquet filename stem.
#'
#' @return A new `data.table` containing only schema-allowed columns, ready
#'   for [write_survey_parquet()].
#' @seealso [write_survey_parquet()], [generate_arrow_dataset()]
#' @family arrow-prep
#' @export
#' @examples
#' \dontrun{
#' inv  <- pipload::load_pip_release_inventory()
#' pip  <- inv[survey_id == "ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL", pip_id]
#' raw  <- pipload::load_pip_data("ARG", 2003, "EPHC-S2", metadata = FALSE)
#' meta <- pipload::load_pip_data("ARG", 2003, "EPHC-S2", metadata = TRUE)
#' dt   <- prepare_for_arrow(raw, meta, pip_id = pip)
#' }
prepare_for_arrow <- function(data, metadata, pip_id) {
  if (!data.table::is.data.table(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.table}, not {.cls {class(data)[[1L]]}}."
    )
  }
  if (!is.list(metadata)) {
    cli::cli_abort(
      "{.arg metadata} must be a named list, not {.cls {class(metadata)[[1L]]}}."
    )
  }

  # Work on a copy so the caller's object is not modified by reference
  dt <- data.table::copy(data)

  # ---- Step 1 & 2: inject metadata and cast core columns -------------------
  inject_metadata_cols(dt, metadata, pip_id)
  cast_data_cols(dt)

  # ---- Step 3: breakdown dimension standardisation -------------------------
  standardize_gender(dt)
  standardize_area(dt)
  standardize_education(dt)
  standardize_age(dt)

  # ---- Step 1 (cont.): column selection ------------------------------------
  allowed_cols      <- c(
    "country_code", "surveyid_year", "welfare_type", "survey_id",
    "survey_acronym", "welfare", "weight", "version",
    "gender", "area", "educat4", "educat5", "educat7", "age"
  )
  optional_dim_cols <- c("gender", "area", "educat4", "educat5", "educat7", "age")

  # Drop columns not in the schema
  extra_cols <- setdiff(names(dt), allowed_cols)
  if (length(extra_cols) > 0L) {
    dt[, (extra_cols) := NULL]
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

  # ---- Step 4: pre-write validation ----------------------------------------
  validate_pre_write(dt)

  dt[]
}
