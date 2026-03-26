# Arrow Parquet Generation
# Plan:  .cg-docs/plans/2026-03-17-arrow-data-preparation.md  (Step 3 & 4)
# Schema: inst/schema/arrow-schema.json  (in {piptm})
#
# Responsibility: write schema-conformant, partitioned Parquet files to the
# Master Arrow Repository from already-prepared data.tables.
#
# The input data.table MUST have been processed by `prepare_for_arrow()`
# (arrow_prep.R) before any function in this file is called. That function
# handles metadata injection, type casting, breakdown dimension
# standardisation, and pre-write checks. The functions here focus only on
# I/O concerns: partition path construction, directory creation, filename
# derivation, and the write itself.
#
# Typical single-survey workflow
# ------------------------------
#   raw  <- pipload::load_pip_data("BOL", 2012, "EH",
#              welfare_type = "CON", module = "ALL", metadata = FALSE)
#   meta <- pipload::load_pip_data("BOL", 2012, "EH",
#              welfare_type = "CON", module = "ALL", metadata = TRUE)
#   dt     <- prepare_for_arrow(raw, meta)          # arrow_prep.R
#   result <- write_survey_parquet(dt)
#
# Typical batch workflow
# ----------------------
#   inv     <- pipload::load_pip_release_inventory()
#   results <- generate_arrow_dataset(inv, module = "ALL")
#
#   # Exact country-year pairs
#   results <- generate_arrow_dataset(
#     inv,
#     surveys = c(ARG = 2003, BOL = 2020, COL = 2010, IDN = 2019),
#     module  = "ALL"
#   )
#
#   # Explicit survey ID character vector (inventory loaded internally)
#   results <- generate_arrow_dataset("ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL")
#
# Data loading note
# -----------------
# `generate_arrow_dataset()` always resolves the physical file to load via
# the `pip_id` column of the release inventory — NOT by parsing the
# `survey_id` string. This ensures the correct versioned `.qs2` file is
# read. When `survey_ids` is a character vector the inventory is loaded
# automatically to perform this resolution.
#
# See also: prepare_for_arrow() in arrow_prep.R — mandatory preprocessing
#           step before write_survey_parquet().
#
# Exported functions
# ------------------
#   write_survey_parquet()      — write one prepared data.table to Parquet
#   survey_ids_from_inventory() — filter an inventory to a character vector
#                                 of survey IDs
#   generate_arrow_dataset()    — batch-write a list of surveys
#
# Partition structure
# -------------------
#   <arrow_repo_path>/
#     country=<country_code>/
#       year=<surveyid_year>/
#         welfare=<welfare_type>/
#           <pip_id>-0.parquet
#
# Filename convention (from arrow-schema.json §filename_convention)
# -----------------------------------------------------------------
#   <pip_id>-0.parquet
#   where <pip_id> is the value stored in the data column `survey_id`.
#   prepare_for_arrow() / inject_metadata_cols() write the pip_id (e.g.
#   "ARG_2003_EPHC-S2_INC_ALL") — NOT the raw survey_id string — into that
#   column, ensuring each welfare-type file gets a unique filename.

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
  # Intersect keeping the order of col_names (i.e. the column order of the
  # data.table), NOT the order of the schema definition.  arrow::as_arrow_table()
  # matches schema fields positionally, so the schema order must match the
  # data column order exactly to avoid "field at index N has name X != Y" errors.
  matched <- intersect(col_names, names(.SCHEMA_GEN$fields))
  fields  <- lapply(
    matched,
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
#' [prepare_for_arrow()]) and writes it to the correct partition directory
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
#' @param dt A `data.table` produced by \strong{[prepare_for_arrow()]}.
#'   Must contain all required columns and only schema-allowed columns.
#'   \strong{Do not pass raw survey data directly — passing unprepared data
#'   will produce misleading validation errors. Always call
#'   [prepare_for_arrow()] first.}
#' @param arrow_repo_path Absolute path to the root of the Master Arrow
#'   Repository. The directory must exist; partition subdirectories are
#'   created automatically.
#' @param overwrite Logical. If `FALSE` (default), skip writing when the
#'   target Parquet file already exists and return a `"skipped"` status row.
#'   This makes re-runs safe — only missing surveys are written. If `TRUE`,
#'   overwrite the existing file.
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
#' @seealso [prepare_for_arrow()] for the mandatory preprocessing step.
#'   [generate_arrow_dataset()] for batch processing.
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' raw    <- pipload::load_pip_data("BOL", 2012, "EH", metadata = FALSE)
#' meta   <- pipload::load_pip_data("BOL", 2012, "EH", metadata = TRUE)
#' dt     <- prepare_for_arrow(raw, meta)
#' result <- write_survey_parquet(dt, arrow_repo_path = "path/to/arrow")
#' }
write_survey_parquet <- function(dt,
                                 arrow_repo_path = getOption("pipdata.arrow_repo"),
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


#' Extract survey IDs from a PIP release inventory
#'
#' Convenience helper to produce the `survey_ids` vector expected by
#' [generate_arrow_dataset()] directly from the inventory object returned by
#' [pipload::load_pip_release_inventory()].
#'
#' @param inventory A `data.table` returned by
#'   [pipload::load_pip_release_inventory()]. Must contain a `survey_id`
#'   column.
#' @param surveys Optional named integer vector specifying exact
#'   country-year pairs to include. Names must be ISO3 country codes and
#'   values must be survey years (e.g. `c(ARG = 2003, BOL = 2020)`). When
#'   supplied, only rows matching **both** a name and its corresponding value
#'   are retained — cross-product matching is never performed. When `NULL`
#'   (default) the country/year filter is not applied and all rows pass
#'   through (subject to `module` and `welfare_type` filters).
#' @param module Optional character vector of module codes to subset (e.g.
#'   `"ALL"`, `"BIN"`, `"GROUP"`). `NULL` (default) returns all modules.
#' @param welfare_type Optional character vector of welfare type codes
#'   (`"INC"`, `"CON"`) to subset. `NULL` (default) returns all.
#'
#' @return A character vector of survey IDs.
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' inv <- pipload::load_pip_release_inventory()
#' # All surveys
#' ids <- survey_ids_from_inventory(inv)
#' # All BOL surveys
#' ids <- survey_ids_from_inventory(inv, surveys = c(BOL = 2020))
#' # Exact country-year pairs
#' ids <- survey_ids_from_inventory(
#'   inv,
#'   surveys = c(ARG = 2003, BOL = 2020, COL = 2010, IDN = 2019),
#'   module  = "ALL"
#' )
#' }
survey_ids_from_inventory <- function(inventory,
                                      surveys      = NULL,
                                      module       = NULL,
                                      welfare_type = NULL) {

  if (!data.table::is.data.table(inventory)) {
    cli::cli_abort(
      "{.arg inventory} must be a {.cls data.table}, not {.cls {class(inventory)}}."
    )
  }
  if (!"survey_id" %in% names(inventory)) {
    cli::cli_abort(
      "{.arg inventory} must contain a {.field survey_id} column."
    )
  }

  dt <- data.table::copy(inventory)

  # --- surveys filter: paired country-year matching -------------------------
  if (!is.null(surveys)) {
    if (!is.numeric(surveys) && !is.integer(surveys)) {
      cli::cli_abort(
        "{.arg surveys} must be a named integer/numeric vector (e.g. {.code c(ARG = 2003, BOL = 2020)})."
      )
    }
    if (is.null(names(surveys)) || any(nchar(names(surveys)) == 0L)) {
      cli::cli_abort(
        "Every element of {.arg surveys} must be named with an ISO3 country code."
      )
    }
    for (col in c("country_code", "surveyid_year")) {
      if (!col %in% names(dt)) {
        cli::cli_abort(
          "{.arg inventory} must contain a {.field {col}} column to use {.arg surveys} filter."
        )
      }
    }
    # Build a lookup key for each pair and filter rows that match any pair
    cc  <- toupper(names(surveys))
    yr  <- as.integer(surveys)
    # Create a temporary join key in the inventory copy
    dt[, .pair_key := paste0(toupper(country_code), "_", surveyid_year)]
    keep_keys <- paste0(cc, "_", yr)
    dt <- dt[.pair_key %in% keep_keys]
    dt[, .pair_key := NULL]
  }

  if (!is.null(module)) {
    if (!is.character(module)) {
      cli::cli_abort("{.arg module} must be a character vector.")
    }
    if (!"module" %in% names(dt)) {
      cli::cli_abort(
        "{.arg inventory} must contain a {.field module} column to filter by module."
      )
    }
    mod <- toupper(module)
    dt  <- dt[toupper(get("module")) %in% mod]
  }

  if (!is.null(welfare_type)) {
    if (!is.character(welfare_type)) {
      cli::cli_abort("{.arg welfare_type} must be a character vector.")
    }
    if (!"welfare_type" %in% names(dt)) {
      cli::cli_abort(
        "{.arg inventory} must contain a {.field welfare_type} column to filter by welfare type."
      )
    }
    wt <- toupper(welfare_type)
    dt <- dt[welfare_type %in% wt]
  }

  if (nrow(dt) == 0L) {
    cli::cli_warn("No surveys match the given filters. Returning empty vector.")
  }

  dt[["survey_id"]]
}


#' Parse a survey ID string into its component parts
#'
#' Splits a canonical PIP survey ID (e.g.
#' `"BOL_2012_EH_V02_M_V08_A_GMD_ALL"`) into named components for use with
#' [pipload::load_pip_data()].
#'
#' @param survey_id A single survey ID string.
#'
#' @return A named list with elements `country_code`, `surveyid_year`,
#'   `survey_acronym`, `vermast`, `veralt`, `collection`, `module`.
#' @keywords internal
.parse_survey_id <- function(survey_id) {
  # Expected full format: COUNTRY_YEAR_ACRONYM_VMAST_M_VALT_A_COLLECTION_MODULE
  # e.g.                  BOL_2012_EH_V02_M_V08_A_GMD_ALL
  # Minimum required:     COUNTRY_YEAR_ACRONYM
  parts <- strsplit(survey_id, "_", fixed = TRUE)[[1L]]

  if (length(parts) < 3L) {
    cli::cli_abort(c(
      "Cannot parse survey ID {.val {survey_id}}.",
      "i" = "Expected at minimum: COUNTRY_YEAR_ACRONYM (e.g. {.val BOL_2012_EH}).",
      "i" = "Full format: COUNTRY_YEAR_ACRONYM_VMAST_M_VALT_A_COLLECTION_MODULE."
    ))
  }

  list(
    country_code   = parts[[1L]],
    surveyid_year  = as.integer(parts[[2L]]),
    survey_acronym = parts[[3L]],
    # Optional components — NULL when not present (load_pip_data accepts NULL)
    vermast        = if (length(parts) >= 4L) tolower(parts[[4L]]) else NULL,
    veralt         = if (length(parts) >= 6L) tolower(parts[[6L]]) else NULL,
    collection     = if (length(parts) >= 8L) parts[[8L]]          else NULL,
    module         = if (length(parts) >= 9L) parts[[9L]]          else "ALL"
  )
}


#' Batch-write surveys to the Arrow repository
#'
#' Accepts either a character vector of survey IDs or a release inventory
#' `data.table` (from [pipload::load_pip_release_inventory()]). For each
#' survey, data is loaded via [pipload::load_pip_data()], prepared via
#' [prepare_for_arrow()], and written via [write_survey_parquet()]. Memory
#' from each survey is freed before the next is loaded.
#'
#' @param survey_ids Character vector of survey IDs, or a `data.table`
#'   inventory from [pipload::load_pip_release_inventory()]. When a
#'   `data.table` is supplied it is passed through
#'   [survey_ids_from_inventory()] automatically.
#' @param arrow_repo_path Absolute path to the root of the Master Arrow
#'   Repository.
#' @param overwrite Logical. Passed to [write_survey_parquet()]. If `FALSE`
#'   (default), surveys whose Parquet file already exists are skipped — making
#'   it safe to re-run the function without duplicating work. Set to `TRUE`
#'   only when you need to overwrite existing files.
#' @param where Passed to [pipload::load_pip_data()]. One of `"release"`
#'   (default) or `"master"`.
#' @param version Release version string passed to
#'   [pipload::load_pip_data()]. `NULL` uses the latest available.
#' @param surveys Optional named integer vector of exact country-year pairs to
#'   include (e.g. `c(ARG = 2003, BOL = 2020)`). Names are ISO3 country codes,
#'   values are survey years. Pairs are matched jointly — no cross-product.
#'   `NULL` (default) applies no country/year filter. Only used when
#'   `survey_ids` is an inventory `data.table`.
#' @param module Optional character vector of module codes to subset (e.g.
#'   `"ALL"`) — only used when `survey_ids` is an inventory `data.table`.
#' @param welfare_type Optional welfare type filter (`"INC"`, `"CON"`) — only
#'   used when `survey_ids` is an inventory `data.table`.
#'
#' @return A `data.table` with one row per survey and columns: `survey_id`,
#'   `status` (`"written"`, `"skipped"`, or `"error"`), `n_rows`,
#'   `available_dimensions`, `file_path`, `message`.
#' @seealso [prepare_for_arrow()] for the preprocessing applied to each
#'   survey internally. [write_survey_parquet()] for single-survey writes.
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' inv <- pipload::load_pip_release_inventory()
#'
#' # All surveys in inventory (use with care — very large)
#' results <- generate_arrow_dataset(inv)
#'
#' # All ALL-module surveys
#' results <- generate_arrow_dataset(inv, module = "ALL")
#'
#' # Exact country-year pairs, ALL module
#' results <- generate_arrow_dataset(
#'   inv,
#'   surveys = c(ARG = 2003, BOL = 2020, COL = 2010, IDN = 2019),
#'   module  = "ALL"
#' )
#'
#' # Explicit character vector of survey IDs
#' results <- generate_arrow_dataset("BOL_2012_EH_V02_M_V08_A_GMD_ALL")
#' }
generate_arrow_dataset <- function(survey_ids,
                                   arrow_repo_path = getOption("pipdata.arrow_repo"),
                                   overwrite    = FALSE,
                                   where        = "release",
                                   version      = NULL,
                                   surveys      = NULL,
                                   module       = NULL,
                                   welfare_type = NULL) {

  # --- Normalise survey_ids input -------------------------------------------
  # Keep the full inventory — we need its columns to call load_pip_data()
  # correctly without string parsing.
  inventory <- NULL

  if (data.table::is.data.table(survey_ids)) {
    inventory  <- data.table::copy(survey_ids)
    survey_ids <- survey_ids_from_inventory(
      inventory,
      surveys      = surveys,
      module       = module,
      welfare_type = welfare_type
    )
  }

  # --- Input validation -----------------------------------------------------
  if (!is.character(survey_ids) || length(survey_ids) < 1L) {
    cli::cli_abort("{.arg survey_ids} must be a non-empty character vector.")
  }
  if (!is.character(arrow_repo_path) || length(arrow_repo_path) != 1L) {
    cli::cli_abort("{.arg arrow_repo_path} must be a single string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    cli::cli_abort("{.arg overwrite} must be a single logical value.")
  }
  where <- match.arg(where, c("release", "master"))

  # If survey_ids was a character vector (not an inventory), load the
  # inventory now so we can look up the correct load arguments per pip_id.
  if (is.null(inventory)) {
    cli::cli_inform("Loading release inventory to resolve pip_id(s)...")
    inventory <- pipload::load_pip_release_inventory()
  }

  # --- Resolve pip_ids from the inventory -----------------------------------
  # Each survey_id may map to multiple pip_id (e.g. INC + CON versions).
  # Each pip_id corresponds to one physical .qs2 file and one Parquet file.
  pip_rows <- inventory[
    !is.na(pip_id) & survey_id %in% survey_ids,
    .(survey_id, pip_id, country_code, surveyid_year,
      survey_acronym, vermast, veralt, collection, module,
      welfare_type)
  ]

  if (nrow(pip_rows) == 0L) {
    cli::cli_abort(
      "None of the supplied survey IDs could be matched to a pip_id in the inventory."
    )
  }

  unmatched <- setdiff(survey_ids, pip_rows$survey_id)
  if (length(unmatched) > 0L) {
    cli::cli_warn(
      "These survey IDs had no pip_id in the inventory and will be skipped: {.val {unmatched}}"
    )
  }

  n_total <- nrow(pip_rows)
  cli::cli_inform("Processing {n_total} pip_id(s).")

  results <- vector("list", n_total)

  for (i in seq_len(n_total)) {
    row_i       <- pip_rows[i]
    pip_id_i    <- row_i$pip_id
    survey_id_i <- row_i$survey_id

    cli::cli_inform("[{i}/{n_total}] {pip_id_i}")

    results[[i]] <- tryCatch({

      # Use inventory columns directly — no string parsing
      raw <- pipload::load_pip_data(
        country_code   = row_i$country_code,
        surveyid_year  = row_i$surveyid_year,
        survey_acronym = row_i$survey_acronym,
        vermast        = row_i$vermast,
        veralt         = row_i$veralt,
        collection     = row_i$collection,
        module         = row_i$module,
        welfare_type   = row_i$welfare_type,
        where          = where,
        version        = version,
        metadata       = FALSE
      )

      meta <- pipload::load_pip_data(
        country_code   = row_i$country_code,
        surveyid_year  = row_i$surveyid_year,
        survey_acronym = row_i$survey_acronym,
        vermast        = row_i$vermast,
        veralt         = row_i$veralt,
        collection     = row_i$collection,
        module         = row_i$module,
        welfare_type   = row_i$welfare_type,
        where          = where,
        version        = version,
        metadata       = TRUE
      )

      dt <- prepare_for_arrow(raw, meta, pip_id = pip_id_i)
      rm(raw, meta)

      result_row <- write_survey_parquet(
        dt              = dt,
        arrow_repo_path = arrow_repo_path,
        overwrite       = overwrite
      )

      rm(dt)
      gc()

      result_row

    }, error = function(e) {
      data.table::data.table(
        survey_id            = survey_id_i,
        country_code         = row_i$country_code,
        surveyid_year        = row_i$surveyid_year,
        welfare_type         = row_i$welfare_type,
        file_path            = NA_character_,
        n_rows               = NA_integer_,
        available_dimensions = NA_character_,
        status               = "error",
        message              = conditionMessage(e)
      )
    })
  }

  data.table::rbindlist(results, fill = TRUE)
}
