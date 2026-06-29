# Pre-Arrow Validation & Preparation
# Spec:  docs/pre-arrow-cleaning-spec.md
# Schema: piptm::pip_arrow_schema()  (single source of truth)
# Plan:   .cg-docs/plans/2026-05-18-deflated-data-arrow-partitions.md  (Phase 1)
#
# Transforms a deflated survey data.table (from pipload::load_pip_deflated_data())
# into a schema-conformant data.table ready for arrow::write_parquet().
#
# Design philosophy — "assert, don't fix", with one controlled exception:
#   Factor columns declared as int32 in the piptm schema are converted to
#   integer using the recode_spec mapping bundled in inst/extdata/recode_spec.yml.
#   This is the only transformation beyond metadata injection. The mapping is
#   used in reverse (label → code) to guarantee correctness regardless of R's
#   internal factor level ordering. Any factor label not found in the mapping
#   causes a hard abort.
#
#   All other columns must already conform to the schema. If they do not,
#   the function aborts with a complete error report.
#
# Drop operations (with warnings):
#   - Columns not in schema and not in welfare_vars are dropped.
#   - Optional schema columns that are entirely NA are dropped.
#   - Welfare columns with no finite values are dropped.
#     Aborts if no welfare column survives.
#
# Exported functions
# ------------------
#   prepare_for_arrow()           — orchestrator
#
# Internal helpers
# ----------------
#   inject_metadata_cols()        — §1: dataset attributes → data.table columns
#   .read_recode_spec()           — reads inst/extdata/recode_spec.yml
#   .build_label_to_code_map()    — builds label→code reverse lookup for one variable
#   .convert_factors_to_integer() — §4b: factor cols → integer using recode_spec
#   .arrow_type_predicate()       — maps Arrow type → R type-checking predicate
#   validate_schema_conformance() — §5: collect all violations, abort if any


# ---------------------------------------------------------------------------
# Internal helper: read recode spec from inst/extdata
# ---------------------------------------------------------------------------

#' Read the recode spec YAML bundled with the package
#'
#' Reads `inst/extdata/recode_spec.yml` and returns the parsed list.
#' Called once per `prepare_for_arrow()` invocation — no caching needed
#' since the file is local and small.
#'
#' @return Named list — the full parsed recode spec.
#' @keywords internal
.read_recode_spec <- function() {
  path <- system.file("extdata", "recode_spec.yml", package = "pipdata")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(
      c(
        "Could not find {.file inst/extdata/recode_spec.yml}.",
        "i" = "Ensure the file is present in the installed package."
      )
    )
  }
  yaml::read_yaml(path)
}


# ---------------------------------------------------------------------------
# Internal helper: build label → code reverse map for one variable
# ---------------------------------------------------------------------------

#' Build a label-to-code reverse lookup for one recode_spec variable
#'
#' The recode_spec mapping is `code: label`. This function inverts it to
#' `label: code` so that factor character values can be looked up by label
#' to find the correct integer code.
#'
#' @param varname  Character scalar variable name (for error messages).
#' @param spec_var List entry from `recode_spec$variables[[varname]]`.
#'
#' @return Named integer vector where names are labels and values are codes.
#'   Returns `NULL` when no mapping is defined (e.g. numeric variables).
#' @keywords internal
.build_label_to_code_map <- function(varname, spec_var) {
  mapping <- spec_var$mapping
  if (is.null(mapping) || length(mapping) == 0L) {
    return(NULL)
  }

  codes  <- as.integer(names(mapping))
  labels <- as.character(unlist(mapping, use.names = FALSE))

  if (anyDuplicated(labels)) {
    cli::cli_abort(
      c(
        "Duplicate labels in recode_spec mapping for {.field {varname}}.",
        "i" = "Each label must map to exactly one code."
      )
    )
  }

  setNames(codes, labels)
}


# ---------------------------------------------------------------------------
# Internal helper: convert factor columns to integer using recode_spec
# ---------------------------------------------------------------------------

#' Convert factor columns to integer codes using the recode_spec mapping
#'
#' For each column in `dt` that is:
#'   (a) a factor, AND
#'   (b) declared as `int32` in the piptm schema, AND
#'   (c) has a `type: factor` entry with a `mapping` in the recode_spec
#'
#' the factor labels are mapped to integer codes using the recode_spec
#' `mapping` (inverted: label → code). Conversion is by reference.
#'
#' Aborts if any factor label in the data is not found in the mapping.
#' NA values are preserved as `NA_integer_`.
#'
#' @param dt          A `data.table` of survey microdata. Modified by reference.
#' @param schema      The piptm schema list from `piptm::pip_arrow_schema()`.
#' @param recode_spec The parsed recode spec list from `.read_recode_spec()`.
#'
#' @return `dt` invisibly (modified by reference).
#' @keywords internal
.convert_factors_to_integer <- function(dt, schema, recode_spec) {
  fields      <- schema$fields
  recode_vars <- recode_spec$variables

  # Identify columns that are: present in dt, factor, declared int32 in schema
  factor_cols <- names(fields)[vapply(names(fields), function(col) {
    col %in% names(dt) &&
      is.factor(dt[[col]]) &&
      identical(tolower(fields[[col]]$type$ToString()), "int32")
  }, logical(1L))]

  if (length(factor_cols) == 0L) return(invisible(dt))

  errors <- character(0L)

  for (col in factor_cols) {
    spec_var <- recode_vars[[col]]

    # No recode_spec entry or not a factor type — cannot convert safely
    if (is.null(spec_var) || !identical(as.character(spec_var$type), "factor")) {
      errors <- c(errors, paste0(
        col, ": is a factor in data and int32 in schema but has no factor ",
        "mapping in recode_spec. Cannot convert safely."
      ))
      next
    }

    label_to_code <- .build_label_to_code_map(col, spec_var)

    if (is.null(label_to_code)) {
      errors <- c(errors, paste0(
        col, ": recode_spec entry has no mapping. Cannot convert factor to integer."
      ))
      next
    }

    # Get the character values of the factor
    char_vals <- as.character(dt[[col]])

    # Check for labels not in the mapping
    unique_vals  <- unique(char_vals[!is.na(char_vals)])
    unknown_vals <- setdiff(unique_vals, names(label_to_code))
    if (length(unknown_vals) > 0L) {
      errors <- c(errors, paste0(
        col, ": factor label(s) not found in recode_spec mapping: ",
        paste(unknown_vals, collapse = ", ")
      ))
      next
    }

    # Convert: map each label to its code, preserve NA
    int_vals <- label_to_code[char_vals]
    int_vals[is.na(char_vals)] <- NA_integer_
    data.table::set(dt, j = col, value = as.integer(int_vals))
  }

  if (length(errors) > 0L) {
    cli::cli_abort(c(
      "Factor-to-integer conversion failed for {length(errors)} column(s).",
      setNames(errors, rep("x", length(errors)))
    ))
  }

  invisible(dt)
}


# ---------------------------------------------------------------------------
# Internal helper: Arrow type → R type predicate
# ---------------------------------------------------------------------------

#' Map an Arrow type object to an R type-checking predicate
#'
#' @param arrow_type An Arrow type object.
#' @return A predicate function or `NULL` when unrecognised.
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
#' `version` columns **by reference** from dataset attributes set by
#' `pipload::load_pip_deflated_data()`. No recoding or type casting applied.
#'
#' @param dt     A `data.table` of deflated survey microdata. Modified by reference.
#' @param pip_id The canonical pip_id string (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`).
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
# §5 — Schema conformance validation
# ---------------------------------------------------------------------------

#' Validate a data.table for full conformance with the piptm Arrow schema
#'
#' Collects all violations before aborting. Must be called after factor
#' conversion — all factor columns should already be integer by this point.
#'
#' @param dt A `data.table` with metadata injected, drops applied, and
#'   factor columns converted. Must carry a non-empty `welfare_vars` attribute.
#'
#' @return `TRUE` invisibly when all checks pass; aborts otherwise.
#' @keywords internal
validate_schema_conformance <- function(dt) {

  schema       <- piptm::pip_arrow_schema()
  fields       <- schema$fields
  welfare_vars <- attr(dt, "welfare_vars")

  errors <- character(0L)

  # §5.1 — Required schema columns present
  required_cols    <- names(fields)[
    vapply(fields, function(f) isTRUE(f$required), logical(1L))
  ]
  missing_required <- setdiff(required_cols, names(dt))
  if (length(missing_required) > 0L) {
    errors <- c(errors, paste0(
      "Missing required column(s): ", paste(missing_required, collapse = ", ")
    ))
  }

  # §5.2 — All welfare_vars columns present
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

  # §5.3 — Type conformance for schema columns present in dt
  schema_cols_present <- intersect(names(fields), names(dt))
  type_errors <- vapply(schema_cols_present, function(col) {
    predicate <- .arrow_type_predicate(fields[[col]]$type)
    if (is.null(predicate)) {
      return(paste0(
        col, ": unrecognised schema type '",
        fields[[col]]$type$ToString(), "' — cannot validate."
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

  # §5.4 — Welfare columns must be double
  if (!is.null(welfare_vars) && length(welfare_vars) > 0L) {
    wv_present <- intersect(welfare_vars, names(dt))
    wv_not_dbl <- wv_present[!vapply(wv_present,
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

  # §5.5 — welfare_type values
  if ("welfare_type" %in% names(dt)) {
    invalid_wt <- setdiff(dt[, unique(welfare_type)], c("INC", "CON"))
    if (length(invalid_wt) > 0L) {
      errors <- c(errors, paste0(
        "welfare_type must be 'INC' or 'CON'; found: ",
        paste(invalid_wt, collapse = ", ")
      ))
    }
  }

  # §5.6 — country_code format
  if ("country_code" %in% names(dt)) {
    bad_cc <- dt[!grepl("^[A-Z]{3}$", country_code), unique(country_code)]
    if (length(bad_cc) > 0L) {
      errors <- c(errors, paste0(
        "country_code does not match ^[A-Z]{3}$: ",
        paste(bad_cc, collapse = ", ")
      ))
    }
  }

  # §5.7 — Partition key consistency
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

  # §5.8 — Welfare column data quality
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

  # §5.9 — Weight quality
  if ("weight" %in% names(dt)) {
    if (dt[, any(is.na(weight))]) {
      errors <- c(errors, "weight: contains NA values.")
    } else if (!dt[, all(is.finite(weight))]) {
      errors <- c(errors, "weight: contains non-finite values (Inf / NaN).")
    } else if (!dt[, all(weight > 0)]) {
      errors <- c(errors, "weight: contains non-positive values (must be strictly > 0).")
    }
  }

  # --- Report all violations at once
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
#' **Steps applied in order:**
#' 1. Inject metadata attributes as columns (only structural transformation).
#' 2. Drop columns not in schema + welfare_vars (warn).
#' 3. Drop optional schema columns that are entirely NA (warn).
#' 4. Drop welfare columns with no finite values (warn); abort if none survive.
#' 4b. Convert factor columns declared as int32 in schema to integer using
#'     the recode_spec mapping in `inst/extdata/recode_spec.yml`. Aborts if
#'     any factor label is not found in the mapping.
#' 5. Assert full schema conformance — collect ALL violations, then abort.
#'
#' @param data   A `data.table` from `pipload::load_pip_deflated_data()`.
#' @param pip_id The canonical pip_id string (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`).
#'
#' @return A schema-conformant `data.table` with `welfare_vars` and `ppp_sort`
#'   preserved as attributes. Ready for [write_survey_parquet()].
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

  # ---- Step 1b: normalise welfare_type to INC/CON -------------------------
welfare_type_map <- c(
  "income"      = "INC",
  "inc"         = "INC",
  "consumption" = "CON",
  "con"         = "CON"
)
current_wt <- tolower(dt[1L, welfare_type])
if (current_wt %in% names(welfare_type_map)) {
  data.table::set(dt, j = "welfare_type",
                  value = welfare_type_map[[current_wt]])
}


  # ---- Step 2: drop columns outside the allowed set (warn) ----------------
  schema       <- piptm::pip_arrow_schema()
  allowed_cols <- c(names(schema$fields), wv)
  extra_cols   <- setdiff(names(dt), allowed_cols)
  if (length(extra_cols) > 0L) {
    cli::cli_warn(c(
      "Dropping column(s) not in the piptm Arrow schema for survey {.val {pip_id}}:",
      setNames(extra_cols, rep("!", length(extra_cols)))
    ))
    dt[, (extra_cols) := NULL]
  }

  # ---- Step 3: drop optional schema columns that are entirely NA (warn) ---
  optional_cols <- names(schema$fields)[
    !vapply(schema$fields, function(f) isTRUE(f$required), logical(1L))
  ]
  all_na_cols <- intersect(optional_cols, names(dt))
  all_na_cols <- all_na_cols[
    vapply(all_na_cols, function(col) dt[, all(is.na(get(col)))], logical(1L))
  ]
  if (length(all_na_cols) > 0L) {
    cli::cli_warn(c(
      "Dropping optional column(s) that are entirely NA for survey {.val {pip_id}}:",
      setNames(all_na_cols, rep("!", length(all_na_cols)))
    ))
    dt[, (all_na_cols) := NULL]
  }

  # ---- Step 4: drop welfare columns with no finite values (warn) ----------
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

  # ---- Step 4b: convert factor columns → integer using recode_spec --------
  recode_spec <- .read_recode_spec()
  .convert_factors_to_integer(dt, schema, recode_spec)

  # ---- Restore attributes on output ----------------------------------------
  data.table::setattr(dt, "welfare_vars", wv)
  data.table::setattr(dt, "ppp_sort",     ppp_sort)

  # ---- Step 5: assert full schema conformance — abort listing all errors ---
  validate_schema_conformance(dt)

  dt[]
}
