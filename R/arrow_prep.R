# ---------------------------------------------------------------------------
# Internal helper: Arrow type → R type predicate
# ---------------------------------------------------------------------------

#' Map an Arrow type object to an R type-checking predicate
#'
#' Used by [validate_schema_conformance()] to assert that each column already
#' carries the correct R type as declared in `piptm::pip_arrow_schema()`.
#'
#' @param arrow_type An Arrow type object (e.g. `schema$fields[[col]]$type`).
#'
#' @return A predicate function (`is.integer`, `is.double`, `is.character`)
#'   or `NULL` when the type is unrecognised.
#' @keywords internal
.arrow_type_predicate <- function(arrow_type) {
  type_str <- tolower(arrow_type$ToString())
  switch(type_str,
    "int32"        = is.integer,
    "double"       = is.double,
    "float64"      = is.double,
    "utf8"         = is.character,
    "string"       = is.character,
    "large_utf8"   = is.character,
    "large_string" = is.character,
    NULL
  )
}


# ---------------------------------------------------------------------------
# §1 — Metadata injection
# ---------------------------------------------------------------------------

#' Inject survey metadata as constant columns into a microdata data.table
#'
#' Adds `country_code`, `surveyid_year`, `welfare_type`, `pip_id`, and
#' `version` columns **by reference**, reading values directly from the dataset
#' attributes set by `pipload::load_pip_deflated_data()`. No recoding or type
#' casting is applied — the attribute values are assumed to already conform to
#' the schema.
#'
#' This is the only transformation performed by the preparation pipeline.
#' Everything else is validation.
#'
#' @param dt     A `data.table` of deflated survey microdata. Must carry
#'   `country_code`, `surveyid_year`, `welfare_type`, `vermast`, and `veralt`
#'   as attributes. Modified **by reference**.
#' @param pip_id The canonical pip_id string for this survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the release inventory.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
inject_metadata_cols <- function(dt, pip_id) {

  required_attrs <- c("country_code", "surveyid_year", "welfare_type",
                      "vermast", "veralt")
  missing_attrs  <- required_attrs[
    vapply(required_attrs, function(a) is.null(attr(dt, a)), logical(1L))
  ]
  if (length(missing_attrs) > 0L) {
    cli::cli_abort(c(
      "Dataset is missing required attribute(s): {.field {missing_attrs}}",
      "i" = "Pass data loaded via {.fn pipload::load_pip_deflated_data}."
    ))
  }
  if (!is.character(pip_id) || length(pip_id) != 1L || is.na(pip_id)) {
    cli::cli_abort("{.arg pip_id} must be a single non-NA character string.")
  }

  dt[, `:=`(
    country_code  = as.character(attr(dt, "country_code")),
    surveyid_year = as.integer(attr(dt, "surveyid_year")),
    welfare_type  = as.character(attr(dt, "welfare_type")),
    pip_id        = as.character(pip_id),
    version       = paste0(
      tolower(as.character(attr(dt, "vermast"))), "_",
      tolower(as.character(attr(dt, "veralt")))
    )
  )]

  invisible(dt)
}


# ---------------------------------------------------------------------------
# §2 — Schema conformance validation
# ---------------------------------------------------------------------------

#' Validate a data.table for full conformance with the piptm Arrow schema
#'
#' Collects **all** schema violations before aborting so the caller can fix
#' everything in one pass. No fixing or casting is performed — the data must
#' already conform.
#'
#' Checks performed (in order):
#' \enumerate{
#'   \item All `required = TRUE` schema columns are present.
#'   \item All `welfare_vars` columns are present.
#'   \item Each present schema column carries the correct R type.
#'   \item All welfare columns are `double` (float64).
#'   \item `welfare_type` values are `"INC"` or `"CON"`.
#'   \item `country_code` matches `^[A-Z]{3}$`.
#'   \item Partition key columns are constant within the file.
#'   \item All welfare columns are finite and non-negative (zeros → warning).
#'   \item `weight` is strictly positive and finite.
#' }
#'
#' @param dt A `data.table` with metadata columns already injected and
#'   extra / all-NA columns already dropped. Must carry a non-empty
#'   `welfare_vars` attribute.
#'
#' @return `TRUE` invisibly when all checks pass; aborts otherwise.
#' @keywords internal
validate_schema_conformance <- function(dt) {

  schema       <- piptm::pip_arrow_schema()
  fields       <- schema$fields
  welfare_vars <- attr(dt, "welfare_vars")

  errors <- character(0L)

  # §2.1 — Required schema columns present -----------------------------------
  required_cols    <- names(fields)[
    vapply(fields, function(f) isTRUE(f$required), logical(1L))
  ]
  missing_required <- setdiff(required_cols, names(dt))
  if (length(missing_required) > 0L) {
    errors <- c(errors, paste0(
      "Missing required column(s): ", paste(missing_required, collapse = ", ")
    ))
  }

  # §2.2 — All welfare_vars columns present ----------------------------------
  if (is.null(welfare_vars) || length(welfare_vars) == 0L) {
    errors <- c(errors, "welfare_vars attribute is absent or empty.")
  } else {
    missing_wv <- setdiff(welfare_vars, names(dt))
    if (length(missing_wv) > 0L) {
      errors <- c(errors, paste0(
        "welfare_vars column(s) missing from data: ",
        paste(missing_wv, collapse = ", ")
      ))
    }
  }

  # §2.3 — Type conformance for schema columns present in dt -----------------
  schema_cols_present <- intersect(names(fields), names(dt))
  type_errors <- vapply(schema_cols_present, function(col) {
    predicate <- .arrow_type_predicate(fields[[col]]$type)
    if (is.null(predicate)) {
      return(paste0(
        col, ": unrecognised schema type '",
        fields[[col]]$type$ToString(),
        "' — cannot validate."
      ))
    }
    if (!predicate(dt[[col]])) {
      return(paste0(
        col, ": expected ", fields[[col]]$type$ToString(),
        " but found R type '", class(dt[[col]])[[1L]], "'."
      ))
    }
    NA_character_
  }, character(1L))
  type_errors <- type_errors[!is.na(type_errors)]
  if (length(type_errors) > 0L) {
    errors <- c(errors, paste0("Type mismatch — ", type_errors))
  }

  # §2.4 — Welfare columns must be double ------------------------------------
  if (!is.null(welfare_vars) && length(welfare_vars) > 0L) {
    wv_present   <- intersect(welfare_vars, names(dt))
    wv_not_dbl   <- wv_present[!vapply(wv_present,
                                       function(col) is.double(dt[[col]]),
                                       logical(1L))]
    if (length(wv_not_dbl) > 0L) {
      errors <- c(errors, paste0(
        "Welfare column(s) must be double (float64): ",
        paste(
          vapply(wv_not_dbl, function(col)
            paste0(col, " (", class(dt[[col]])[[1L]], ")"),
            character(1L)
          ),
          collapse = ", "
        )
      ))
    }
  }

  # §2.5 — welfare_type values -----------------------------------------------
  if ("welfare_type" %in% names(dt)) {
    invalid_wt <- setdiff(dt[, unique(welfare_type)], c("INC", "CON"))
    if (length(invalid_wt) > 0L) {
      errors <- c(errors, paste0(
        "welfare_type must be 'INC' or 'CON'; found: ",
        paste(invalid_wt, collapse = ", ")
      ))
    }
  }

  # §2.6 — country_code format -----------------------------------------------
  if ("country_code" %in% names(dt)) {
    bad_cc <- dt[!grepl("^[A-Z]{3}$", country_code), unique(country_code)]
    if (length(bad_cc) > 0L) {
      errors <- c(errors, paste0(
        "country_code does not match ^[A-Z]{3}$: ",
        paste(bad_cc, collapse = ", ")
      ))
    }
  }

  # §2.7 — Partition key consistency -----------------------------------------
  for (key in c("country_code", "surveyid_year", "welfare_type", "version")) {
    if (key %in% names(dt)) {
      n <- dt[, data.table::uniqueN(get(key))]
      if (n != 1L) {
        errors <- c(errors, paste0(
          "Partition key '", key, "' must be constant within one file ",
          "but has ", n, " distinct value(s)."
        ))
      }
    }
  }

  # §2.8 — Welfare column data quality ---------------------------------------
  if (!is.null(welfare_vars) && length(welfare_vars) > 0L) {
    wv_present <- intersect(welfare_vars, names(dt))
    survey_id  <- if ("pip_id" %in% names(dt)) dt[1L, pip_id] else "unknown"
    for (wc in wv_present) {
      n_zero <- dt[, sum(get(wc) == 0, na.rm = TRUE)]
      if (n_zero > 0L) {
        cli::cli_warn(
          "{n_zero} row(s) have {.field {wc}} == 0 in survey {.val {survey_id}}."
        )
      }
      if (!dt[, all(is.finite(get(wc)))]) {
        errors <- c(errors, paste0(
          wc, ": contains non-finite values (Inf / NaN / NA)."
        ))
      } else if (!dt[, all(get(wc) >= 0)]) {
        errors <- c(errors, paste0(
          wc, ": contains negative values."
        ))
      }
    }
  }

  # §2.9 — Weight quality ----------------------------------------------------
  if ("weight" %in% names(dt)) {
    if (dt[, any(is.na(weight))]) {
      errors <- c(errors, "weight: contains NA values.")
    } else if (!dt[, all(is.finite(weight))]) {
      errors <- c(errors, "weight: contains non-finite values (Inf / NaN).")
    } else if (!dt[, all(weight > 0)]) {
      errors <- c(errors, "weight: contains non-positive values (must be strictly > 0).")
    }
  }

  # --- Report all violations at once ----------------------------------------
  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Data does not conform to the piptm Arrow schema.",
      "!" = "{length(errors)} violation(s) found:",
      setNames(errors, rep("x", length(errors)))
    ))
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
#' ready for [write_survey_parquet()].
#'
#' **Design philosophy — "assert, don't fix"**: the only transformation
#' applied is injecting metadata attributes as columns (§1). All column lists
#' and type requirements are derived exclusively from `piptm::pip_arrow_schema()`
#' — nothing is hardcoded. After injection, the function asserts full schema
#' conformance and aborts with a complete violation report if anything is wrong.
#'
#' **Silent operations** (no error, no warning):
#' \itemize{
#'   \item Columns not in the schema and not in `welfare_vars` are dropped.
#'   \item Optional schema columns that are entirely `NA` are dropped.
#' }
#'
#' **Warning** (non-fatal):
#' \itemize{
#'   \item Welfare columns with no finite values at all are dropped and a
#'     warning is emitted. These correspond to PPP reference years with no
#'     valid deflation for this survey. The function aborts if no welfare
#'     column survives this step.
#' }
#'
#' The input `data.table` is **copied** before any modification; the caller's
#' object is never changed.
#'
#' @param data   A `data.table` as returned by
#'   `pipload::load_pip_deflated_data()`. Must carry `welfare_vars`,
#'   `ppp_sort`, `country_code`, `surveyid_year`, `welfare_type`, `vermast`,
#'   and `veralt` as attributes, plus `weight` and all welfare columns listed
#'   in `welfare_vars` as data columns. All columns must already be of the
#'   R type corresponding to their declaration in `piptm::pip_arrow_schema()`.
#' @param pip_id The canonical pip_id string for this survey file
#'   (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`), taken from the release inventory.
#'
#' @return A new `data.table` containing only schema-allowed columns and
#'   available welfare columns, with `welfare_vars` and `ppp_sort` preserved
#'   as attributes. Ready for [write_survey_parquet()].
#'
#' @seealso [write_survey_parquet()], [generate_arrow_dataset()]
#' @family arrow-prep
#' @export
#' @examples
#' \dontrun{
#' inv  <- pipload::load_pip_master_inventory()
#' pip  <- inv[pip_id == "ARG_2003_EPHC-S2_INC_ALL", pip_id]
#' defl <- pipload::load_pip_deflated_data(id_name = pip)
#' dt   <- prepare_for_arrow(defl, pip_id = pip)
#' }
prepare_for_arrow <- function(data, pip_id) {

  if (!data.table::is.data.table(data)) {
    cli::cli_abort(
      "{.arg data} must be a {.cls data.table}, not {.cls {class(data)[[1L]]}}."
    )
  }

  wv       <- attr(data, "welfare_vars")
  ppp_sort <- attr(data, "ppp_sort")

  if (is.null(wv) || length(wv) == 0L) {
    cli::cli_abort(c(
      "{.arg data} is missing a non-empty {.field welfare_vars} attribute.",
      "i" = "Pass data loaded via {.fn pipload::load_pip_deflated_data}."
    ))
  }

  # Work on a copy — caller's object is never modified
  dt <- data.table::copy(data)

  # ---- Step 1: inject metadata attributes as columns ----------------------
  inject_metadata_cols(dt, pip_id)

  # ---- Step 2: drop columns outside the allowed set (silent) --------------
  schema       <- piptm::pip_arrow_schema()
  
  allowed_cols <- c(names(schema$fields), wv)
  extra_cols   <- setdiff(names(dt), allowed_cols)
  if (length(extra_cols) > 0L) {
    dt[, (extra_cols) := NULL]
  }

  # ---- Step 3: drop optional schema columns that are entirely NA (silent) --
  optional_cols <- names(schema$fields)[
    !vapply(schema$fields, function(f) isTRUE(f$required), logical(1L))
  ]
  all_na_cols <- intersect(optional_cols, names(dt))
  all_na_cols <- all_na_cols[
    vapply(all_na_cols, function(col) dt[, all(is.na(get(col)))], logical(1L))
  ]
  if (length(all_na_cols) > 0L) {
    dt[, (all_na_cols) := NULL]
  }

  # ---- Step 4: drop welfare columns with no finite values (warn) -----------
  # Occurs when a PPP reference year has no valid deflation for this survey.
  # The welfare_vars attribute is updated to reflect surviving columns.
  # Aborts if no welfare column survives.
  wv_bad <- wv[vapply(wv, function(col) {
    col %in% names(dt) && !dt[, any(is.finite(get(col)))]
  }, logical(1L))]

  if (length(wv_bad) > 0L) {
    cli::cli_warn(c(
      "Dropping welfare column(s) with no finite values for survey {.val {pip_id}}:",
      setNames(wv_bad, rep("!", length(wv_bad)))
    ))
    dt[, (wv_bad) := NULL]
    wv <- setdiff(wv, wv_bad)
  }

  if (length(wv) == 0L) {
    cli::cli_abort(c(
      "All welfare columns were dropped for survey {.val {pip_id}}.",
      "i" = "No finite welfare values found in any deflated welfare column."
    ))
  }

  # ---- Restore attributes on output ----------------------------------------
  data.table::setattr(dt, "welfare_vars", wv)
  data.table::setattr(dt, "ppp_sort",     ppp_sort)

  # ---- Step 5: assert full schema conformance — abort listing all errors ---
  validate_schema_conformance(dt)

  dt[]
}
