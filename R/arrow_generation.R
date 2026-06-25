# ---------------------------------------------------------------------------
# Internal constants — derived from piptm::pip_arrow_schema()
# Initialised in .onLoad() (aaa.R) to avoid cyclic namespace issues at
# package load time. Do not call piptm:: at top level here.
# ---------------------------------------------------------------------------

.SCHEMA_GEN        <- NULL
.REQUIRED_COLS_GEN <- NULL
.ALLOWED_COLS_GEN  <- NULL
.OPTIONAL_DIMS_GEN <- NULL

# Lazy accessors — return the cached global when initialised by .onLoad(),
# otherwise fall back to calling piptm:: directly. This makes the arrow
# generation functions robust in load_all() development sessions where
# piptm may not be on the search path yet.
.get_schema <- function() {
  if (is.null(.SCHEMA_GEN)) piptm::pip_arrow_schema() else .SCHEMA_GEN
}
.get_required_cols <- function() {
  if (is.null(.REQUIRED_COLS_GEN)) piptm::pip_required_cols() else .REQUIRED_COLS_GEN
}
.get_allowed_cols <- function() {
  if (is.null(.ALLOWED_COLS_GEN)) piptm::pip_allowed_cols() else .ALLOWED_COLS_GEN
}
.get_optional_dims <- function() {
  if (!is.null(.OPTIONAL_DIMS_GEN)) {
    return(.OPTIONAL_DIMS_GEN)
  }
  if (exists("pip_optional_dims", envir = asNamespace("piptm"), inherits = FALSE)) {
    get("pip_optional_dims", envir = asNamespace("piptm"))()
  } else {
    piptm:::pip_optional_dims()
  }
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build the Arrow partition directory path for a single survey
#'
#' @param arrow_repo_path Root of the Arrow repository.
#' @param country_code    ISO3 country code (character scalar).
#' @param surveyid_year   Survey year (integer scalar).
#' @param welfare_type    Welfare type code — `"INC"` or `"CON"`.
#' @param version         Combined version string, e.g. `"v01_v04"` (character
#'   scalar). Derived from `paste0(tolower(vermast), "_", tolower(veralt))`.
#'
#' @return Absolute path string for the partition directory.
#' @keywords internal
.build_partition_dir <- function(arrow_repo_path,
                                 country_code,
                                 surveyid_year,
                                 welfare_type,
                                 version) {
  file.path(
    arrow_repo_path,
    paste0("country_code=",  country_code),
    paste0("surveyid_year=", surveyid_year),
    paste0("welfare_type=",  welfare_type),
    paste0("version=",       version)
  )
}

#' Derive the Parquet filename from a pip_id
#'
#' Follows the filename convention in arrow-schema.json:
#'   `<pip_id>-0.parquet`
#'
#' @param pip_id File-level survey identifier string (e.g. `"BOL_2020_EH_INC_ALL"`).
#'
#' @return Filename string (no directory component).
#' @keywords internal
.build_parquet_filename <- function(pip_id) {
  paste0(pip_id, "-0.parquet")
}

#' Extract welfare type code from a pip_id string
#'
#' The pip_id schema is `COUNTRY_YEAR_ACRONYM_(INC|CON)_(ALL|GPWG)`. The
#' welfare token is always the second-to-last underscore-delimited segment.
#'
#' Utility helper — `welfare_type` is read directly from the inventory in
#' [generate_arrow_dataset()]. This function is retained for ad-hoc use
#' when only a pip_id string is available.
#'
#' @param pip_id A single pip_id string (e.g. `"ARG_2003_EPHC-S2_INC_ALL"`).
#'
#' @return `"INC"` or `"CON"` (character scalar).
#' @keywords internal
.extract_welfare_from_pip_id <- function(pip_id) {
  parts <- strsplit(pip_id, "_", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) {
    cli::cli_abort(
      "Cannot extract welfare type from pip_id {.val {pip_id}}: too few segments."
    )
  }
  parts[[length(parts) - 1L]]
}

#' Build the Arrow schema object for a specific set of columns
#'
#' Returns an `arrow::schema()` that covers exactly the columns present in
#' `col_names`. Welfare columns (`^welfare_`) are always `float64`. All other
#' columns are looked up in the base schema from `piptm::pip_arrow_schema()`.
#' Optional breakdown columns are included only when present.
#'
#' This ensures `write_parquet()` uses explicit, stable types rather than
#' inferring from the R object. The schema is built in `col_names` order so
#' that `arrow::as_arrow_table()` positional matching succeeds.
#'
#' @param col_names Character vector of column names in the data.table.
#'
#' @return An `arrow::Schema` object.
#' @keywords internal
.build_arrow_schema <- function(col_names) {
  schema <- .get_schema()

  fields <- lapply(col_names, function(nm) {
    if (nm %in% names(schema$fields)) {
      arrow::field(nm, schema$fields[[nm]]$type)
    } else if (grepl("^welfare_", nm)) {
      # Dynamic welfare columns (welfare_lcu, welfare_ppp_*) — always float64
      arrow::field(nm, arrow::float64())
    } else {
      NULL
    }
  })

  fields <- Filter(Negate(is.null), fields)
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
#' No validation is performed here. Full schema conformance validation is the
#' responsibility of [prepare_for_arrow()], which must always be called before
#' this function. If execution reaches this function, the data is assumed
#' to be guaranteed conformant.
#'
#' Partition structure:
#' ```
#' <arrow_repo_path>/
#'   country_code=<country_code>/
#'     surveyid_year=<surveyid_year>/
#'       welfare_type=<welfare_type>/
#'         version=<version>/
#'           <pip_id>-0.parquet
#' ```
#'
#' The function:
#' 1. Derives the partition directory and filename from data values.
#' 2. Creates the partition directory if absent.
#' 3. Skips writing if the target file already exists (append-only model)
#'    unless `overwrite = TRUE`.
#' 4. Writes the Parquet file with an explicit Arrow schema (snappy
#'    compression).
#' 5. Returns a one-row summary `data.table`.
#'
#' @param dt A `data.table` produced by \strong{[prepare_for_arrow()]}.
#'   \strong{Do not pass raw or unprepared data — always call
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
#'     \item{`pip_id`}{File-level survey identifier (e.g. "BOL_2020_EH_INC_ALL").}
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
#' @seealso [prepare_for_arrow()] for the mandatory preprocessing and
#'   validation step. [generate_arrow_dataset()] for batch processing.
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' defl   <- pipload::load_pip_deflated_data(id_name = "BOL_2012_EH_CON_ALL")
#' dt     <- prepare_for_arrow(defl, pip_id = "BOL_2012_EH_CON_ALL")
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

  # --- Extract scalar partition keys -----------------------------------------
  country_code  <- dt[1L, country_code]
  surveyid_year <- dt[1L, surveyid_year]
  welfare_type  <- dt[1L, welfare_type]
  version       <- dt[1L, version]
  pip_id_val    <- dt[1L, pip_id]

  # --- Derive paths ----------------------------------------------------------
  partition_dir <- .build_partition_dir(
    arrow_repo_path, country_code, surveyid_year, welfare_type, version
  )
  parquet_file  <- file.path(
    partition_dir, .build_parquet_filename(pip_id_val)
  )
  rel_path      <- file.path(
    paste0("country_code=",  country_code),
    paste0("surveyid_year=", surveyid_year),
    paste0("welfare_type=",  welfare_type),
    paste0("version=",       version),
    .build_parquet_filename(pip_id_val)
  )

  # --- Identify available breakdown dimensions present in this survey --------
  dim_cols         <- intersect(.get_optional_dims(), names(dt))
  avail_dimensions <- paste(dim_cols, collapse = ", ")

  # --- Build summary row skeleton --------------------------------------------
  summary_base <- data.table::data.table(
    pip_id               = pip_id_val,
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
    cli::cli_inform(paste0("Skipping existing file: ", rel_path))
    summary_base[, `:=`(status = "skipped")]
    return(summary_base[])
  }

  # --- Create partition directory if needed ----------------------------------
  if (!dir.exists(partition_dir)) {
    dir.create(partition_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # --- Build explicit Arrow schema for this survey's columns -----------------
  arrow_schema <- .build_arrow_schema(names(dt))

  # Embed ppp_sort in schema metadata so generate_release_manifest() recovers
  # the authoritative value (from attr(dt, "ppp_sort"), set by pipload) rather
  # than inferring it from column names. NA / NULL ppp_sort (legacy surveys)
  # is simply omitted — the manifest reader will fall back to NA_integer_.
  ppp_sort_val <- attr(dt, "ppp_sort")
  if (!is.null(ppp_sort_val) && !is.na(ppp_sort_val)) {
    arrow_schema <- arrow_schema$WithMetadata(
      list(ppp_sort = as.character(as.integer(ppp_sort_val)))
    )
  }

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
    error = function(e) conditionMessage(e)
  )

  if (identical(write_result, "ok")) {
    summary_base[, `:=`(n_rows = nrow(dt), status = "written")]
    cli::cli_inform(paste0("Written [", nrow(dt), " rows] -> ", rel_path))
  } else {
    if (file.exists(parquet_file)) unlink(parquet_file)
    summary_base[, `:=`(status = "error", message = write_result)]
    cli::cli_warn(paste0("Failed to write ", rel_path, ": ", write_result))
  }

  return(summary_base[])
}


# ---------------------------------------------------------------------------
# survey_ids_from_inventory()
# ---------------------------------------------------------------------------

#' Extract survey IDs from a PIP master inventory
#'
#' Convenience helper to produce the `survey_ids` vector expected by
#' [generate_arrow_dataset()] directly from the inventory object returned by
#' [pipload::load_pip_master_inventory()].
#'
#' @param inventory A `data.table` returned by
#'   [pipload::load_pip_master_inventory()]. Must contain a `survey_id` column.
#' @param surveys Optional named integer vector specifying exact country-year
#'   pairs to include. Names must be ISO3 country codes and values must be
#'   survey years (e.g. `c(ARG = 2003, BOL = 2020)`). When supplied, only rows
#'   matching **both** a name and its corresponding value are retained —
#'   cross-product matching is never performed. When `NULL` (default) no
#'   country/year filter is applied.
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
#' inv <- pipload::load_pip_master_inventory()
#' ids <- survey_ids_from_inventory(inv, surveys = c(ARG = 2003, BOL = 2020), module = "ALL")
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
    cli::cli_abort("{.arg inventory} must contain a {.field survey_id} column.")
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
    cc        <- toupper(names(surveys))
    yr        <- as.integer(surveys)
    dt[, .pair_key := paste0(toupper(country_code), "_", surveyid_year)]
    keep_keys <- paste0(cc, "_", yr)
    dt        <- dt[.pair_key %in% keep_keys]
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
    dt <- dt[toupper(get("module")) %in% toupper(module)]
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
    dt <- dt[welfare_type %in% toupper(welfare_type)]
  }

  if (nrow(dt) == 0L) {
    cli::cli_warn("No surveys match the given filters. Returning empty vector.")
  }

  dt[["survey_id"]]
}


# ---------------------------------------------------------------------------
# .parse_survey_id()  — internal utility
# ---------------------------------------------------------------------------

#' Parse a survey ID string into its component parts
#'
#' Splits a canonical PIP survey ID (e.g. `"BOL_2012_EH_V02_M_V08_A_GMD_ALL"`)
#' into named components.
#'
#' @param survey_id A single survey ID string.
#'
#' @return A named list with elements `country_code`, `surveyid_year`,
#'   `survey_acronym`, `vermast`, `veralt`, `collection`, `module`.
#' @keywords internal
.parse_survey_id <- function(survey_id) {
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
    vermast        = if (length(parts) >= 4L) tolower(parts[[4L]]) else NULL,
    veralt         = if (length(parts) >= 6L) tolower(parts[[6L]]) else NULL,
    collection     = if (length(parts) >= 8L) parts[[8L]]          else NULL,
    module         = if (length(parts) >= 9L) parts[[9L]]          else "ALL"
  )
}


# ---------------------------------------------------------------------------
# generate_arrow_dataset()
# ---------------------------------------------------------------------------

#' Batch-write surveys to the Arrow repository
#'
#' Accepts either a character vector of survey IDs or a master inventory
#' `data.table` (from [pipload::load_pip_master_inventory()]). For each
#' survey, deflated data is loaded via
#' `pipload::load_pip_deflated_data(id_name = pip_id)`, prepared and validated
#' via [prepare_for_arrow()], and written via [write_survey_parquet()]. Memory
#' from each survey is freed before the next is loaded.
#'
#' @param survey_ids Character vector of survey IDs, or a `data.table`
#'   inventory from [pipload::load_pip_master_inventory()]. When a
#'   `data.table` is supplied it is passed through
#'   [survey_ids_from_inventory()] automatically.
#' @param arrow_repo_path Absolute path to the root of the Master Arrow
#'   Repository.
#' @param overwrite Logical. Passed to [write_survey_parquet()]. If `FALSE`
#'   (default), surveys whose Parquet file already exists are skipped. Set to
#'   `TRUE` only when you need to overwrite existing files.
#' @param surveys Optional named integer vector of exact country-year pairs
#'   (e.g. `c(ARG = 2003, BOL = 2020)`). Only used when `survey_ids` is an
#'   inventory `data.table`.
#' @param module Optional character vector of module codes (e.g. `"ALL"`).
#'   Only used when `survey_ids` is an inventory `data.table`.
#' @param welfare_type Optional welfare type filter (`"INC"`, `"CON"`).
#'   Only used when `survey_ids` is an inventory `data.table`.
#'
#' @return A `data.table` with one row per pip_id and columns: `pip_id`,
#'   `country_code`, `surveyid_year`, `welfare_type`, `file_path`, `n_rows`,
#'   `available_dimensions`, `status` (`"written"`, `"skipped"`, or `"error"`),
#'   `message`.
#' @seealso [prepare_for_arrow()] for the preprocessing and validation applied
#'   to each survey. [write_survey_parquet()] for single-survey writes.
#' @family arrow-generation
#' @export
#' @examples
#' \dontrun{
#' inv <- pipload::load_pip_master_inventory()
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
                                   surveys      = NULL,
                                   module       = NULL,
                                   welfare_type = NULL) {

  # --- Normalise survey_ids input -------------------------------------------
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

  # If survey_ids was a character vector (not an inventory), load the
  # inventory now so we can look up the correct pip_id(s) per survey_id.
  if (is.null(inventory)) {
    cli::cli_inform("Loading release inventory to resolve pip_id(s)...")
    inventory <- pipload::load_pip_master_inventory()
  }

  # --- Resolve pip_ids from the inventory -----------------------------------
  # Each survey_id may map to multiple pip_ids (e.g. INC + CON versions).
  # welfare_type is read from the inventory column — fail loudly if absent.
  if (!"welfare_type" %in% names(inventory)) {
    cli::cli_abort(c(
      "{.arg inventory} is missing required column {.field welfare_type}.",
      "i" = "Both {.fn pipload::load_pip_master_inventory} and
             {.fn pipload::load_pip_release_inventory} provide this column."
    ))
  }

  pip_rows <- inventory[
    !is.na(pip_id) & survey_id %in% survey_ids,
    .(survey_id, pip_id, country_code, surveyid_year,
      welfare_type, survey_acronym, vermast, veralt, collection, module)
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

    cli::cli_inform("[{i}/{n_total}] {pip_id_i}")

    results[[i]] <- tryCatch({

      raw <- pipload::load_pip_deflated_data(id_name = pip_id_i)
      dt  <- prepare_for_arrow(raw, pip_id = pip_id_i)
      rm(raw)

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
        pip_id               = pip_id_i,
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
