# Arrow Parquet Generation
# Plan:  .cg-docs/plans/2026-03-17-arrow-data-preparation.md  (Step 3 & 4)
# Schema: inst/schema/arrow-schema.json  (in {piptm})
#
# Responsibility: write schema-conformant, partitioned Parquet files to the
# Master Arrow Repository from already-prepared data.tables.
#
# The input data.table is assumed to have been processed by
# `prepare_for_arrow()` (arrow_prep.R), which handles metadata injection,
# type casting, breakdown dimension standardisation, and pre-write checks.
# These functions therefore focus only on I/O concerns: partition path
# construction, directory creation, filename derivation, and the write itself.
#
# Exported functions
# ------------------
#   write_survey_parquet()    — write one prepared data.table to Parquet
#   generate_arrow_dataset()  — batch-write a list of surveys
#
# Partition structure
# -------------------
#   <arrow_repo_path>/
#     country=<country_code>/
#       year=<surveyid_year>/
#         welfare=<welfare_type>/
#           <survey_id>-0.parquet
#
# Filename convention (from arrow-schema.json §filename_convention)
# -----------------------------------------------------------------
#   <survey_id>-0.parquet
#   where <survey_id> is taken directly from the data column (already the
#   canonical identifier injected by prepare_for_arrow / inject_metadata_cols).

# ---------------------------------------------------------------------------
# Internal constants — derived from piptm::pip_arrow_schema()
# ---------------------------------------------------------------------------

.SCHEMA_GEN        <- piptm::pip_arrow_schema()
.REQUIRED_COLS_GEN <- piptm::pip_required_cols()
.ALLOWED_COLS_GEN  <- piptm::pip_allowed_cols()

.GENDER_LEVELS_GEN <- .SCHEMA_GEN$levels$gender
.AREA_LEVELS_GEN   <- .SCHEMA_GEN$levels$area
.EDU_LEVELS_GEN    <- .SCHEMA_GEN$levels$education

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build the Arrow partition directory path for a single survey
#'
#' @param arrow_repo_path Root of the Arrow repository.
#' @param country_code    ISO3 country code (character scalar).
#' @param surveyid_year   Survey year (integer scalar).
#' @param welfare_type    Welfare type code — `"INC"` or `"CON"`.
#'
#' @return Absolute path string for the partition directory.
#' @keywords internal
.build_partition_dir <- function(arrow_repo_path,
                                 country_code,
                                 surveyid_year,
                                 welfare_type) {
  file.path(
    arrow_repo_path,
    paste0("country=", country_code),
    paste0("year=",    surveyid_year),
    paste0("welfare=", welfare_type)
  )
}

#' Derive the Parquet filename from a survey_id
#'
#' Follows the filename convention in arrow-schema.json:
#'   `<survey_id>-0.parquet`
#'
#' @param survey_id Full survey identifier string.
#'
#' @return Filename string (no directory component).
#' @keywords internal
.build_parquet_filename <- function(survey_id) {
  paste0(survey_id, "-0.parquet")
}

#' Validate a prepared data.table before writing to Parquet
#'
#' Performs all schema and data-quality checks specific to the generation
#' step. This is a lightweight guard — full pre-write validation should
#' already have been run by `prepare_for_arrow()`. Stops on hard failures;
#' warns on zero-welfare observations.
#'
#' @param dt A prepared `data.table`.
#'
#' @return `TRUE` invisibly when all checks pass.
#' @keywords internal
.validate_for_write <- function(dt) {

  # --- Required columns -------------------------------------------------------
  missing_cols <- setdiff(.REQUIRED_COLS_GEN, names(dt))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Required columns missing from input data: {.val {missing_cols}}"
    )
  }

  # --- No extra columns -------------------------------------------------------
  extra_cols <- setdiff(names(dt), .ALLOWED_COLS_GEN)
  if (length(extra_cols) > 0L) {
    cli::cli_abort(
      paste0(
        "Input contains column(s) not in the Arrow schema: ",
        "{.val {extra_cols}}. ",
        "Run {.fn prepare_for_arrow} first, or drop these columns manually."
      )
    )
  }

  # --- Partition key consistency (one unique value per file) ------------------
  for (key_col in c("country_code", "surveyid_year", "welfare_type")) {
    n_unique <- dt[, data.table::uniqueN(get(key_col))]
    if (n_unique != 1L) {
      cli::cli_abort(
        paste0(
          "Partition key {.field {key_col}} must be constant within one ",
          "Parquet file but found {n_unique} distinct value(s). ",
          "Split the data by survey before calling {.fn write_survey_parquet}."
        )
      )
    }
  }

  # --- Welfare type values ----------------------------------------------------
  wt_vals    <- dt[, unique(welfare_type)]
  invalid_wt <- setdiff(wt_vals, c("INC", "CON"))
  if (length(invalid_wt) > 0L) {
    cli::cli_abort(
      paste0(
        "welfare_type must be 'INC' or 'CON'; ",
        "found invalid value(s): {.val {invalid_wt}}"
      )
    )
  }

  # --- Country code format (ISO3 uppercase) -----------------------------------
  if (!dt[, all(grepl("^[A-Z]{3}$", country_code))]) {
    bad <- dt[!grepl("^[A-Z]{3}$", country_code), unique(country_code)]
    cli::cli_abort(
      "country_code does not match ISO3 format [A-Z]{{3}}: {.val {bad}}"
    )
  }

  # --- Welfare: warn on zeros; abort on negative / non-finite -----------------
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
      "welfare contains non-finite values (Inf / NaN / NA). ",
      "All welfare values must be finite."
    )
  }
  if (!dt[, all(welfare >= 0)]) {
    cli::cli_abort(
      "welfare contains negative values. Negative welfare is not permitted."
    )
  }

  # --- Weight: must be strictly positive and finite ---------------------------
  if (dt[, any(is.na(weight))]) {
    cli::cli_abort("weight contains NA values.")
  }
  if (!dt[, all(is.finite(weight))]) {
    cli::cli_abort("weight contains non-finite values (Inf / NaN).")
  }
  if (!dt[, all(weight > 0)]) {
    cli::cli_abort(
      "weight contains non-positive values. Weights must be strictly > 0."
    )
  }

  # --- Factor level conformance for optional breakdown dimensions -------------
  if ("gender" %in% names(dt)) {
    bad <- dt[!is.na(gender) & !gender %in% .GENDER_LEVELS_GEN,
              unique(as.character(gender))]
    if (length(bad) > 0L) {
      cli::cli_abort(
        paste0(
          "gender has values outside allowed levels ",
          "{.val {.GENDER_LEVELS_GEN}}: {.val {bad}}"
        )
      )
    }
  }
  if ("area" %in% names(dt)) {
    bad <- dt[!is.na(area) & !area %in% .AREA_LEVELS_GEN,
              unique(as.character(area))]
    if (length(bad) > 0L) {
      cli::cli_abort(
        paste0(
          "area has values outside allowed levels ",
          "{.val {.AREA_LEVELS_GEN}}: {.val {bad}}"
        )
      )
    }
  }
  if ("education" %in% names(dt)) {
    bad <- dt[!is.na(education) & !education %in% .EDU_LEVELS_GEN,
              unique(as.character(education))]
    if (length(bad) > 0L) {
      cli::cli_abort(
        paste0(
          "education has values outside allowed levels ",
          "{.val {.EDU_LEVELS_GEN}}: {.val {bad}}"
        )
      )
    }
  }
  if ("age" %in% names(dt)) {
    bad <- dt[!is.na(age) & (age < 0L | age > 130L), unique(age)]
    if (length(bad) > 0L) {
      cli::cli_abort("age values out of range [0, 130]: {.val {bad}}")
    }
  }

  invisible(TRUE)
}

#' Build the Arrow schema object for a specific set of columns
#'
#' Returns an `arrow::schema()` that covers exactly the columns present in
#' `col_names`. Optional breakdown columns are included only when present.
#' This ensures `write_parquet()` uses explicit, stable types rather than
#' inferring from the R object.
#'
#' @param col_names Character vector of column names in the data.table.
#'
#' @return An `arrow::Schema` object.
#' @keywords internal
.build_arrow_schema <- function(col_names) {
  fields <- lapply(
    intersect(names(.SCHEMA_GEN$fields), col_names),
    function(nm) arrow::field(nm, .SCHEMA_GEN$fields[[nm]]$type)
  )
  do.call(arrow::schema, fields)
}

# ---------------------------------------------------------------------------
# write_survey_parquet()
# ---------------------------------------------------------------------------

#' Write a single prepared survey data.table to a Parquet file
#'
#' Takes a schema-conformant, already-prepared `data.table` (output of
#' `prepare_for_arrow()`) and writes it to the correct partition directory
#' inside the Master Arrow Repository.
#'
#' Partition structure:
#' ```
#' <arrow_repo_path>/
#'   country=<country_code>/
#'     year=<surveyid_year>/
#'       welfare=<welfare_type>/
#'         <survey_id>-0.parquet
#' ```
#'
#' The function:
#' 1. Validates the input (required columns, factor levels, welfare/weight
#'    constraints).
#' 2. Derives the partition directory and filename from data values.
#' 3. Creates the partition directory if absent.
#' 4. Skips writing if the target file already exists (append-only model)
#'    unless `overwrite = TRUE`.
#' 5. Writes the Parquet file with an explicit Arrow schema (snappy
#'    compression).
#' 6. Returns a one-row summary `data.table`.
#'
#' @param dt            A prepared `data.table`. Must contain all required
#'   columns and only schema-allowed columns. Typically the output of
#'   `prepare_for_arrow()`.
#' @param arrow_repo_path  Absolute path to the root of the Master Arrow
#'   Repository. The directory must exist; partition subdirectories are
#'   created automatically.
#' @param overwrite     Logical. If `FALSE` (default), skip writing when the
#'   target Parquet file already exists and return a `"skipped"` status row.
#'   If `TRUE`, overwrite the existing file.
#'
#' @return A one-row `data.table` with columns:
#'   \describe{
#'     \item{`survey_id`}{Survey identifier.}
#'     \item{`country_code`}{ISO3 country code.}
#'     \item{`surveyid_year`}{Survey year.}
#'     \item{`welfare_type`}{"INC" or "CON".}
#'     \item{`file_path`}{Absolute path of the written Parquet file.}
#'     \item{`n_rows`}{Number of rows written (NA when skipped).}
#'     \item{`available_dimensions`}{Comma-separated list of breakdown
#'       dimension columns present (empty string when none).}
#'     \item{`status`}{"written", "skipped", or "error".}
#'     \item{`message`}{Error message when status is "error"; NA otherwise.}
#'   }
#'
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' result <- write_survey_parquet(BOL_2012, arrow_repo_path = "path/to/arrow")
#' }
write_survey_parquet <- function(dt,
                                 arrow_repo_path,
                                 overwrite = FALSE) {

  stopifnot(data.table::is.data.table(dt))
  stopifnot(is.character(arrow_repo_path), length(arrow_repo_path) == 1L)
  stopifnot(is.logical(overwrite), length(overwrite) == 1L)

  # Normalise path (resolve ~, relative paths, trailing slashes)
  arrow_repo_path <- normalizePath(arrow_repo_path, mustWork = FALSE)

  if (!dir.exists(arrow_repo_path)) {
    cli::cli_abort(
      "Arrow repository root does not exist: {.path {arrow_repo_path}}"
    )
  }

  # --- Validate input --------------------------------------------------------
  .validate_for_write(dt)

  # --- Extract scalar partition keys (validated to be unique above) ----------
  country_code   <- dt[1L, country_code]
  surveyid_year  <- dt[1L, surveyid_year]
  welfare_type   <- dt[1L, welfare_type]
  survey_id      <- dt[1L, survey_id]

  # --- Derive paths ----------------------------------------------------------
  partition_dir  <- .build_partition_dir(
    arrow_repo_path, country_code, surveyid_year, welfare_type
  )
  parquet_file   <- file.path(
    partition_dir, .build_parquet_filename(survey_id)
  )
  rel_path       <- file.path(
    paste0("country=", country_code),
    paste0("year=",    surveyid_year),
    paste0("welfare=", welfare_type),
    .build_parquet_filename(survey_id)
  )

  # --- Identify available breakdown dimensions present in this survey ---------
  dim_cols         <- intersect(c("gender", "area", "education", "age"), names(dt))
  avail_dimensions <- paste(dim_cols, collapse = ", ")

  # --- Build summary row skeleton (filled in below) --------------------------
  summary_base <- data.table::data.table(
    survey_id            = survey_id,
    country_code         = country_code,
    surveyid_year        = surveyid_year,
    welfare_type         = welfare_type,
    file_path            = parquet_file,
    n_rows               = NA_integer_,
    available_dimensions = avail_dimensions,
    status               = NA_character_,
    message              = NA_character_
  )

  # --- Skip if file exists and overwrite is FALSE ----------------------------
  if (!overwrite && file.exists(parquet_file)) {
    rlang::inform(
      paste0("Skipping existing file: ", rel_path)
    )
    summary_base[, `:=`(status = "skipped")]
    return(summary_base[])
  }

  # --- Create partition directory if needed ----------------------------------
  if (!dir.exists(partition_dir)) {
    dir.create(partition_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Build explicit Arrow schema for this survey's columns -----------------
  arrow_schema <- .build_arrow_schema(names(dt))

  # --- Write Parquet file ----------------------------------------------------
  # Convert to Arrow Table first so the schema is applied before writing.
  # arrow::write_parquet() does not accept a schema= argument directly;
  # the schema must be enforced at the Table level via arrow::as_arrow_table().
  write_result <- tryCatch(
    {
      arrow_table <- arrow::as_arrow_table(dt, schema = arrow_schema)
      arrow::write_parquet(
        x           = arrow_table,
        sink        = parquet_file,
        compression = "snappy"
      )
      "ok"
    },
    error = function(e) {
      conditionMessage(e)
    }
  )

  if (identical(write_result, "ok")) {
    summary_base[, `:=`(
      n_rows  = nrow(dt),
      status  = "written"
    )]
    rlang::inform(
      paste0(
        "Written [", nrow(dt), " rows] -> ", rel_path
      )
    )
  } else {
    # Clean up a potentially partial file
    if (file.exists(parquet_file)) {
      unlink(parquet_file)
    }
    summary_base[, `:=`(
      status  = "error",
      message = write_result
    )]
    rlang::warn(
      paste0("Failed to write ", rel_path, ": ", write_result)
    )
  }

  return(summary_base[])
}

# ---------------------------------------------------------------------------
# generate_arrow_dataset()
# ---------------------------------------------------------------------------

#' Batch-write a list of prepared survey data.tables to the Arrow repository
#'
#' Iterates over a named list of prepared `data.table` objects, calling
#' `write_survey_parquet()` for each one, and returns a consolidated summary.
#'
#' Each element must already be prepared (schema-conformant, all required
#' columns injected). Use `prepare_for_arrow()` to prepare raw {pipdata}
#' output before passing it here.
#'
#' Surveys that fail validation or write errors are recorded with
#' `status = "error"` in the summary — the batch does not abort early.
#'
#' @param survey_list     A named list of prepared `data.table` objects. Names
#'   are used only for progress messages; the actual `survey_id` is read from
#'   the data.
#' @param arrow_repo_path Absolute path to the root of the Master Arrow
#'   Repository.
#' @param overwrite       Logical. Passed through to `write_survey_parquet()`.
#'   Defaults to `FALSE` (skip existing files).
#'
#' @return A `data.table` with one row per survey and the same columns as the
#'   return value of `write_survey_parquet()`, plus:
#'   \describe{
#'     \item{`list_name`}{The name of the element in `survey_list`.}
#'   }
#'
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' surveys <- list(BOL_2012 = BOL_2012, PER_2019 = PER_2019)
#' summary <- generate_arrow_dataset(surveys, arrow_repo_path = "path/to/arrow")
#' }
generate_arrow_dataset <- function(survey_list,
                                   arrow_repo_path,
                                   overwrite = FALSE) {

  stopifnot(is.list(survey_list), length(survey_list) > 0L)
  stopifnot(is.character(arrow_repo_path), length(arrow_repo_path) == 1L)

  # Ensure names exist for informative progress messages
  if (is.null(names(survey_list))) {
    names(survey_list) <- paste0("survey_", seq_along(survey_list))
  }

  n_surveys <- length(survey_list)
  rlang::inform(
    paste0("Starting Arrow dataset generation: ", n_surveys, " survey(s).")
  )

  # --- Iterate and collect results -------------------------------------------
  # Use a pre-allocated list and rbindlist for efficiency.
  results <- vector("list", n_surveys)

  for (i in seq_along(survey_list)) {
    list_name <- names(survey_list)[[i]]
    dt        <- survey_list[[i]]

    rlang::inform(
      paste0("[", i, "/", n_surveys, "] Processing: ", list_name)
    )

    # Wrap in tryCatch to catch validation errors without aborting the batch
    row <- tryCatch(
      write_survey_parquet(
        dt             = dt,
        arrow_repo_path = arrow_repo_path,
        overwrite      = overwrite
      ),
      error = function(e) {
        # If .validate_for_write or directory creation aborts, record here
        data.table::data.table(
          survey_id            = tryCatch(dt[1L, survey_id],
                                         error = function(e2) NA_character_),
          country_code         = NA_character_,
          surveyid_year        = NA_integer_,
          welfare_type         = NA_character_,
          file_path            = NA_character_,
          n_rows               = NA_integer_,
          available_dimensions = NA_character_,
          status               = "error",
          message              = conditionMessage(e)
        )
      }
    )

    row[, list_name := list_name]
    results[[i]] <- row
  }

  # --- Consolidate results ---------------------------------------------------
  summary_dt <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

  # Move list_name to first position for readability
  data.table::setcolorder(
    summary_dt,
    c("list_name", setdiff(names(summary_dt), "list_name"))
  )

  # --- Print summary ---------------------------------------------------------
  n_written <- summary_dt[status == "written", .N]
  n_skipped <- summary_dt[status == "skipped", .N]
  n_errors  <- summary_dt[status == "error",   .N]

  rlang::inform(
    paste0(
      "Arrow dataset generation complete. ",
      "Written: ", n_written, "  |  ",
      "Skipped: ", n_skipped, "  |  ",
      "Errors: ",  n_errors
    )
  )

  if (n_errors > 0L) {
    rlang::warn(
      paste0(
        n_errors, " survey(s) failed. ",
        "Check the $message column in the returned summary for details."
      )
    )
  }

  return(summary_dt[])
}
