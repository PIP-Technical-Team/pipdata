# Manifest Generation
# Plan:  .cg-docs/plans/2026-03-17-arrow-data-preparation.md  (Phase 0D, Step 7)
# Brainstorm: .cg-docs/brainstorms/2026-03-17-multi-release-manifest-architecture.md
# Schema: inst/schema/arrow-schema.json  (in {piptm}, canonical reference)
#
# Generates a release manifest JSON that serves as the reproducibility
# contract between {pipdata} (data producer) and {piptm} (data consumer).
# Each PIP PROD release has exactly one manifest file.
#
# The manifest records:
#   - which survey versions belong to a release
#   - the exact Parquet file path for each survey
#   - which breakdown dimensions are present in each Parquet file
#
# Manifest JSON structure (see project-context.md §Release Manifest):
#   {
#     "release_id": "20260206",
#     "arrow_root": "<path>",
#     "created_at": "<ISO 8601>",
#   "surveys": [ ... one entry per pip_id ... ]
#   }
#   where file_path follows: country_code=<cc>/surveyid_year=<yr>/welfare_type=<wt>/version=<ver>/<pip_id>-0.parquet
#
# Workflow
# --------
#   1. Load release inventory (data.table from pipload::load_pip_release_inventory)
#   2. For each pip_id in the inventory:
#      a. Derive the expected Parquet file path from partition conventions
#      b. Read Parquet schema to discover available breakdown dimensions
#      c. Build a manifest survey entry
#   3. Assemble and write manifest JSON
#   4. Optionally write current_release.json pointer
#
# Exported functions
# ------------------
#   discover_parquet_dimensions()  — read schema of one Parquet, return dim names
#   build_manifest_entry()         — build one survey JSON entry from inventory row
#   generate_release_manifest()    — assemble + write the full manifest

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Derive the relative Parquet path for a pip_id
#'
#' Internal helper used only during manifest generation to locate the physical
#' Parquet file for existence checks and dimension introspection. The derived
#' path is **not** stored in the manifest JSON — the manifest stores partition
#' filter keys instead.
#'
#' Follows the 4-level partition convention established in arrow-schema.json:
#'   `country_code=<cc>/surveyid_year=<yr>/welfare_type=<wt>/version=<version>/<pip_id>-0.parquet`
#'
#' @param country_code  ISO3 country code (character scalar).
#' @param surveyid_year Survey year (integer scalar).
#' @param welfare_type  "INC" or "CON" (character scalar).
#' @param version       Combined version string, e.g. `"v01_v04"` (character
#'   scalar). Derived from `paste0(tolower(vermast), "_", tolower(veralt))`.
#' @param pip_id        Canonical pip_id string, e.g. "COL_2010_ECH_V01_M_V02_A_INC".
#'
#' @return Relative path string (forward slashes).
#' @keywords internal
.derive_parquet_path <- function(country_code,
                                 surveyid_year,
                                 welfare_type,
                                 version,
                                 pip_id) {
  paste(
    paste0("country_code=",  country_code),
    paste0("surveyid_year=", surveyid_year),
    paste0("welfare_type=",  welfare_type),
    paste0("version=",       version),
    paste0(pip_id, "-0.parquet"),
    sep = "/"
  )
}

#' Optional breakdown dimension column names (canonical order)
#'
#' @return Character vector of the optional breakdown dimension column names.
#' @keywords internal
.manifest_dim_cols <- function() {
  c("gender", "area", "educat4", "educat5", "educat7", "age")
}

# ---------------------------------------------------------------------------
# discover_parquet_dimensions()
# ---------------------------------------------------------------------------

#' Discover available breakdown dimensions in a Parquet file
#'
#' Reads only the file schema (no row data) and returns the names of optional
#' breakdown dimension columns that are present. The canonical optional
#' dimensions are: `gender`, `area`, `educat4`, `educat5`, `educat7`, `age`.
#'
#' @param file_path Absolute path to a `.parquet` file.
#'
#' @return Character vector of dimension column names present in the file, or
#'   `character(0)` when none are found. Returns `NA_character_` (length 1) if
#'   the file cannot be read, so callers can distinguish "no dims" from
#'   "unreadable file".
#'
#' @family manifest-generation
#' @export
#' @examples
#' \dontrun{
#' dims <- discover_parquet_dimensions("path/to/COL_2010_ECH_V01_M_V02_A_INC-0.parquet")
#' # e.g. c("gender", "area", "educat4", "educat5")
#' }
discover_parquet_dimensions <- function(file_path) {
  stopifnot(is.character(file_path), length(file_path) == 1L)

  schema <- tryCatch(
    arrow::open_dataset(file_path)$schema,
    error = function(e) NULL
  )

  if (is.null(schema)) {
    rlang::warn(
      paste0("Cannot read Parquet schema: ", file_path)
    )
    return(NA_character_)
  }

  actual_cols <- names(schema)
  intersect(.manifest_dim_cols(), actual_cols)
}

# ---------------------------------------------------------------------------
# build_manifest_entry()
# ---------------------------------------------------------------------------

#' Build a single manifest survey entry from inventory fields
#'
#' Constructs the list structure for one survey entry in the manifest JSON.
#' Does not perform file I/O — all information comes from the inventory row
#' and the pre-computed `dimensions` vector.
#'
#' The entry stores four **partition filter keys** (`country_code`, `year`,
#' `welfare_type`, `version`) that `{piptm}` uses with Arrow's native
#' partition pushdown via `open_dataset() |> dplyr::filter()`. No physical
#' file path is stored — the manifest is therefore portable across environments.
#'
#' @param country_code   ISO3 country code. Partition filter key.
#' @param surveyid_year  Survey year (integer). Partition filter key.
#' @param welfare_type   `"INC"` or `"CON"`. Partition filter key.
#' @param survey_id      Full DLW survey identifier (e.g.
#'   `"COL_2010_ECH_V01_M_V02_A_GMD_ALL"`).
#' @param survey_acronym Short survey name (e.g. `"ECH"`).
#' @param version        Combined version string, e.g. `"v01_v02"`. Partition
#'   filter key. Derived from
#'   `paste0(tolower(vermast), "_", tolower(veralt))`.
#' @param module         Processing module (e.g. `"ALL"`).
#' @param pip_id         Canonical pip_id (e.g. `"COL_2010_ECH_V01_M_V02_A_INC"`).
#' @param dimensions     Character vector of breakdown dimension column names
#'   available in this survey's Parquet file (e.g. `c("gender", "area")`).
#'   Use `character(0)` when none are present. Known universe:
#'   `gender`, `area`, `educat4`, `educat5`, `educat7`, `age`.
#' @param reporting_level Reporting level for the survey (e.g. `"national"`,
#'   `"urban"`, `"rural"`). Currently a placeholder (`"national"`) — will be
#'   sourced from `release_inventory$reporting_level` once that column is
#'   added to the inventory.
#'
#' @return A named list with 10 fields suitable for JSON serialisation via
#'   [jsonlite::toJSON()]: `pip_id`, `survey_id`, `country_code`, `year`,
#'   `welfare_type`, `version`, `survey_acronym`, `module`, `dimensions`,
#'   `reporting_level`.
#'
#' @family manifest-generation
#' @export
#' @examples
#' entry <- build_manifest_entry(
#'   country_code   = "COL",
#'   surveyid_year  = 2010L,
#'   welfare_type   = "INC",
#'   survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
#'   survey_acronym = "ECH",
#'   version        = "v01_v02",
#'   module         = "ALL",
#'   pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
#'   dimensions     = c("gender", "area")
#' )
build_manifest_entry <- function(country_code,
                                 surveyid_year,
                                 welfare_type,
                                 survey_id,
                                 survey_acronym,
                                 version,
                                 module,
                                 pip_id,
                                 dimensions,
                                 # TODO: remove default once inventory column is available
                                 reporting_level = "national") {
  list(
    pip_id          = as.character(pip_id),
    survey_id       = as.character(survey_id),
    country_code    = as.character(country_code),
    year            = as.integer(surveyid_year),
    welfare_type    = as.character(welfare_type),
    version         = as.character(version),
    survey_acronym  = as.character(survey_acronym),
    module          = as.character(module),
    reporting_level = as.character(reporting_level),
    dimensions      = as.character(dimensions)
  )
}

# ---------------------------------------------------------------------------
# generate_release_manifest()
# ---------------------------------------------------------------------------

#' Generate a PIP release manifest JSON
#'
#' Builds a release manifest by scanning the Arrow repository for each
#' survey in the provided inventory, discovering which breakdown dimensions
#' are available, and writing a JSON manifest file. Optionally updates the
#' `current_release.json` pointer file.
#'
#' The manifest follows the schema documented in `project-context.md`:
#'
#' ```json
#' {
#'   "release": "20260206",
#'   "generated_at": "2026-02-06T12:00:00Z",
#'   "entries": [
#'     {
#'       "pip_id": "COL_2010_ECH_V01_M_V02_A_INC",
#'       "survey_id": "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
#'       "country_code": "COL",
#'       "year": 2010,
#'       "welfare_type": "INC",
#'       "version": "v01_v02",
#'       "survey_acronym": "ECH",
#'       "module": "ALL",
#'       "dimensions": ["gender", "area", "educat4"]
#'     }
#'   ]
#' }
#' ```
#'
#' Surveys whose Parquet file does not exist in `arrow_root` are recorded as
#' warnings and **excluded** from the manifest (they cannot be validated or
#' loaded downstream). A summary of written, skipped, and failed entries is
#' returned invisibly.
#'
#' @param release          Character scalar. The PIP release identifier, e.g.
#'   `"20260206"`. Used as the `release` field in the JSON.
#' @param arrow_root       Absolute path to the root of the Master Arrow
#'   Repository. All survey Parquet files are expected under this directory.
#'   Not stored in the manifest output — the consumer configures this
#'   independently via the `PIPTM_ARROW_ROOT` environment variable.
#' @param release_inventory A `data.table` from
#'   [pipload::load_pip_release_inventory()] (or equivalent). Must contain
#'   columns: `survey_id`, `pip_id`, `country_code`, `surveyid_year`,
#'   `welfare_type`, `survey_acronym`, `vermast`, `veralt`, `module`.
#'   Rows with `NA` `pip_id` are silently excluded (they have no Parquet file).
#' @param output_path      Absolute path for the output manifest JSON file,
#'   e.g. `"//server/manifests/manifest_20260206.json"`. The parent directory
#'   must exist.
#' @param set_as_current   Logical. If `TRUE` (default `FALSE`), writes or
#'   overwrites `current_release.json` in the same directory as `output_path`
#'   to point to this release.
#'
#' @return A `data.table` with one row per inventory row (excluding `NA`
#'   `pip_id` rows) and columns:
#'   \describe{
#'     \item{`pip_id`}{Canonical pip_id string.}
#'     \item{`file_path`}{Absolute path of the Parquet file checked (not in JSON).}
#'     \item{`status`}{"included", "missing", or "unreadable".}
#'     \item{`dimensions`}{Comma-separated dimension names (empty
#'       string when none; `NA` when file is missing/unreadable).}
#'     \item{`message`}{Informational note or `NA`.}
#'   }
#'   Returned invisibly.
#'
#' @family manifest-generation
#' @export
#' @examples
#' \dontrun{
#' inv <- pipload::load_pip_release_inventory()
#' generate_release_manifest(
#'   release           = "20260206",
#'   arrow_root        = "//server/pip/arrow",
#'   release_inventory = inv,
#'   output_path       = "//server/manifests/manifest_20260206.json",
#'   set_as_current    = TRUE
#' )
#' }
generate_release_manifest <- function(release,
                                      arrow_root = getOption("pipdata.arrow_repo"),
                                      release_inventory,
                                      output_path = getOption("pipdata.manifest_root"),
                                      set_as_current = FALSE) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.character(release),     length(release)     == 1L, !is.na(release),
    is.character(arrow_root),  length(arrow_root)  == 1L, !is.na(arrow_root),
    is.character(output_path), length(output_path) == 1L, !is.na(output_path),
    is.logical(set_as_current), length(set_as_current) == 1L
  )
  if (!data.table::is.data.table(release_inventory)) {
    cli::cli_abort(
      "{.arg release_inventory} must be a {.cls data.table}."
    )
  }

  required_inv_cols <- c(
    "survey_id", "pip_id", "country_code", "surveyid_year",
    "welfare_type", "survey_acronym", "vermast", "veralt", "module"
    # TODO: add "reporting_level" once inventory column is available
  )
  missing_inv_cols <- setdiff(required_inv_cols, names(release_inventory))
  if (length(missing_inv_cols) > 0L) {
    cli::cli_abort(
      "release_inventory is missing required column(s): {.field {missing_inv_cols}}"
    )
  }

  arrow_root <- normalizePath(arrow_root, mustWork = FALSE)
  if (!dir.exists(arrow_root)) {
    cli::cli_abort(
      "Arrow repository root does not exist: {.path {arrow_root}}"
    )
  }

  # Expand directory path to a file path before deriving output_dir
  if (dir.exists(output_path)) {
    output_path <- file.path(output_path, paste0("manifest_", release, ".json"))
  }

  output_dir <- dirname(output_path)

  if (!dir.exists(output_dir)) {
    cli::cli_abort(
      "Output directory does not exist: {.path {output_dir}}"
    )
  }

  # --- Filter to rows with a valid pip_id ------------------------------------
  inv <- release_inventory[!is.na(pip_id), .SD, .SDcols = required_inv_cols]

  n_total <- nrow(inv)
  if (n_total == 0L) {
    cli::cli_abort(
      "No rows with a valid {.field pip_id} found in {.arg release_inventory}."
    )
  }

  cli::cli_inform(
    "Building manifest for release {.val {release}} ({n_total} pip_id(s))."
  )

  # --- Process each pip_id ---------------------------------------------------
  # Pre-allocate result columns
  pip_ids_out  <- inv$pip_id
  file_paths   <- character(n_total)
  statuses     <- character(n_total)
  avail_dims   <- character(n_total)
  messages     <- character(n_total)
  survey_entries <- vector("list", n_total)

  for (i in seq_len(n_total)) {
    row_i     <- inv[i]
    pip_id_i  <- row_i$pip_id

    # Derive version from vermast / veralt (same construction as arrow_prep.R)
    version_i  <- paste0(tolower(row_i$vermast), "_", tolower(row_i$veralt))

    # Derive the relative path (used only for file existence check and dimension
    # introspection — NOT stored in the manifest output)
    rel_path_i <- .derive_parquet_path(
      country_code  = row_i$country_code,
      surveyid_year = row_i$surveyid_year,
      welfare_type  = row_i$welfare_type,
      version       = version_i,
      pip_id        = pip_id_i
    )

    # Absolute path for file existence check and schema read
    abs_path_i <- file.path(arrow_root, rel_path_i)

    file_paths[i] <- abs_path_i

    # --- File existence check -------------------------------------------------
    if (!file.exists(abs_path_i)) {
      statuses[i]  <- "missing"
      avail_dims[i] <- NA_character_
      messages[i]  <- paste0("Parquet file not found: ", rel_path_i)
      rlang::warn(messages[i])
      next
    }

    # --- Dimension discovery --------------------------------------------------
    dims_i <- discover_parquet_dimensions(abs_path_i)

    if (length(dims_i) == 1L && is.na(dims_i)) {
      # File exists but schema could not be read
      statuses[i]  <- "unreadable"
      avail_dims[i] <- NA_character_
      messages[i]  <- paste0("Cannot read Parquet schema: ", rel_path_i)
      next
    }

    avail_dims[i] <- paste(dims_i, collapse = ", ")
    statuses[i]   <- "included"
    messages[i]   <- NA_character_

    # --- Build survey entry ---------------------------------------------------
    survey_entries[[i]] <- build_manifest_entry(
      country_code    = row_i$country_code,
      surveyid_year   = row_i$surveyid_year,
      welfare_type    = row_i$welfare_type,
      survey_id       = row_i$survey_id,
      survey_acronym  = row_i$survey_acronym,
      version         = version_i,
      module          = row_i$module,
      pip_id          = pip_id_i,
      dimensions      = dims_i,
      # TODO: replace placeholder with row_i$reporting_level once inventory column is available
      reporting_level = "national"
    )
  }

  # --- Summarise results ------------------------------------------------------
  n_included   <- sum(statuses == "included",   na.rm = TRUE)
  n_missing    <- sum(statuses == "missing",    na.rm = TRUE)
  n_unreadable <- sum(statuses == "unreadable", na.rm = TRUE)

  cli::cli_inform(c(
    "i" = "Survey processing complete:",
    " " = "Included:   {n_included}",
    " " = "Missing:    {n_missing}",
    " " = "Unreadable: {n_unreadable}"
  ))

  if (n_included == 0L) {
    cli::cli_abort(
      "No surveys could be included in the manifest — no Parquet files were found."
    )
  }

  # --- Assemble manifest JSON -------------------------------------------------
  survey_list <- Filter(Negate(is.null), survey_entries)

  manifest <- list(
    release      = release,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    entries      = survey_list
  )

  manifest_json <- jsonlite::toJSON(manifest, pretty = TRUE, auto_unbox = TRUE)

  # --- Write manifest file ----------------------------------------------------
  tryCatch(
    writeLines(manifest_json, con = output_path),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to write manifest to {.path {output_path}}",
          "x" = conditionMessage(e)
        )
      )
    }
  )

  cli::cli_inform(
    c("v" = "Manifest written: {.path {output_path}} ({n_included} surveys).")
  )

  # --- Optionally write current_release.json ----------------------------------
  if (isTRUE(set_as_current)) {
    .write_current_release(release_id = release, output_dir = output_dir)
  }

  # --- Return summary ---------------------------------------------------------
  summary_dt <- data.table::data.table(
    pip_id     = pip_ids_out,
    file_path  = file_paths,
    status     = statuses,
    dimensions = avail_dims,
    message    = messages
  )

  invisible(summary_dt)
}

# ---------------------------------------------------------------------------
# .write_current_release()   — internal pointer writer
# ---------------------------------------------------------------------------

#' Write or overwrite current_release.json
#'
#' Creates a small JSON pointer file in `output_dir` that records the
#' currently active PROD release. {piptm} reads this at startup to determine
#' the default release.
#'
#' File content:
#' ```json
#' {
#'   "current_release": "20260206",
#'   "updated_at": "2026-02-06T12:00:00Z"
#' }
#' ```
#'
#' @param release_id  Character scalar. Release identifier to set as current.
#' @param output_dir  Directory in which `current_release.json` is written.
#'
#' @return Path to the written file, invisibly.
#' @keywords internal
.write_current_release <- function(release_id, output_dir) {
  pointer <- list(
    current_release = release_id,
    updated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  pointer_path <- file.path(output_dir, "current_release.json")
  pointer_json <- jsonlite::toJSON(pointer, pretty = TRUE, auto_unbox = TRUE)

  tryCatch(
    writeLines(pointer_json, con = pointer_path),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to write current_release.json to {.path {pointer_path}}",
          "x" = conditionMessage(e)
        )
      )
    }
  )

  cli::cli_inform(
    c("v" = "current_release.json updated: {.path {pointer_path}}")
  )

  invisible(pointer_path)
}
