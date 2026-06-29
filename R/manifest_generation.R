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
#   - which breakdown dimensions are present in each Parquet file
#   - country and region metadata for each survey
#   - total number of observations per survey
#   - number of non-missing observations per dimension column
#
# Manifest JSON structure (see project-context.md §Release Manifest):
#   {
#     "release": "20260206",
#     "generated_at": "<ISO 8601>",
#     "entries": [ ... one entry per pip_id ... ]
#   }
#   where file_path follows:
#     country_code=<cc>/surveyid_year=<yr>/welfare_type=<wt>/version=<ver>/<pip_id>-0.parquet
#
# Workflow
# --------
#   1. Load release inventory (data.table from pipload::load_pip_release_inventory)
#   2. Load country_list auxiliary data once for country/region metadata lookup
#   3. For each pip_id in the inventory:
#      a. Derive the expected Parquet file path from partition conventions
#      b. Read Parquet schema to discover available breakdown dimensions
#      c. Read Parquet data to compute n_obs and per-dimension non-NA counts
#      d. Look up country_name, region_name, region_code from country_list
#      e. Build a manifest survey entry
#   4. Assemble and write manifest JSON
#   5. Optionally write current_release.json pointer
#
# Exported functions
# ------------------
#   discover_parquet_dimensions()   — read schema of one Parquet, return dim names
#   discover_parquet_welfare_cols() — read schema of one Parquet, return welfare col names
#   discover_parquet_obs_counts()   — read data of one Parquet, return n_obs + dim counts
#   build_manifest_entry()          — build one survey JSON entry from inventory row
#   generate_release_manifest()     — assemble + write the full manifest


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
#' @param version       Combined version string, e.g. `"v01_v04"`.
#' @param pip_id        Canonical pip_id string.
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

#' Optional dimension column names — thin wrapper around [piptm::pip_optional_dims()]
#'
#' @return Character vector of optional field names in schema definition order.
#' @keywords internal
.manifest_dim_cols <- function() piptm::pip_optional_dims()


# ---------------------------------------------------------------------------
# discover_parquet_dimensions()
# ---------------------------------------------------------------------------

#' Discover available breakdown dimensions in a Parquet file
#'
#' Reads only the file schema (no row data) and returns the names of optional
#' breakdown dimension columns that are present.
#'
#' @param file_path Absolute path to a `.parquet` file.
#'
#' @return Character vector of dimension column names present in the file, or
#'   `character(0)` when none are found. Returns `NA_character_` (length 1) if
#'   the file cannot be read.
#'
#' @family manifest-generation
#' @export
discover_parquet_dimensions <- function(file_path) {
  stopifnot(is.character(file_path), length(file_path) == 1L)

  schema <- tryCatch(
    arrow::open_dataset(file_path)$schema,
    error = function(e) NULL
  )

  if (is.null(schema)) {
    rlang::warn(paste0("Cannot read Parquet schema: ", file_path))
    return(NA_character_)
  }

  intersect(.manifest_dim_cols(), names(schema))
}


# ---------------------------------------------------------------------------
# discover_parquet_welfare_cols()
# ---------------------------------------------------------------------------

#' Discover welfare column names in a Parquet file
#'
#' Reads only the file schema (no row data) and returns the names of welfare
#' columns present: `welfare_lcu` and any `welfare_ppp_*` columns.
#'
#' @param file_path Absolute path to a `.parquet` file.
#'
#' @return Character vector of welfare column names, or `character(0)` when
#'   none are found.
#'
#' @family manifest-generation
#' @export
discover_parquet_welfare_cols <- function(file_path) {
  stopifnot(is.character(file_path), length(file_path) == 1L)

  schema <- tryCatch(
    arrow::open_dataset(file_path)$schema,
    error = function(e) NULL
  )

  if (is.null(schema)) return(character(0))

  grep("^welfare_lcu$|^welfare_ppp_", names(schema), value = TRUE)
}


# ---------------------------------------------------------------------------
# discover_parquet_obs_counts()
# ---------------------------------------------------------------------------

#' Discover observation counts in a Parquet file
#'
#' Reads the Parquet file and returns:
#' - the total number of rows (`n_obs`)
#' - for each dimension column present, the number of non-NA observations
#'
#' Only the dimension columns are read from disk — welfare and required
#' columns are excluded — so this is efficient even for large files.
#'
#' @param file_path  Absolute path to a `.parquet` file.
#' @param dim_cols   Character vector of dimension column names to count.
#'   Typically the output of [discover_parquet_dimensions()]. When
#'   `character(0)`, only `n_obs` is returned.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`n_obs`}{Integer. Total number of rows in the file.
#'       `NA_integer_` if the file cannot be read.}
#'     \item{`dimensions_n_obs`}{Named integer vector. Non-NA row count
#'       for each dimension column. Empty named integer vector when no
#'       dimension columns are present or the file cannot be read.}
#'   }
#'
#' @family manifest-generation
#' @export
#' @examples
#' \dontrun{
#' counts <- discover_parquet_obs_counts(
#'   "path/to/PHL_2012_FIES_INC_ALL-0.parquet",
#'   dim_cols = c("gender", "area", "lstatus")
#' )
#' counts$n_obs
#' counts$dimensions_n_obs
#' }
discover_parquet_obs_counts <- function(file_path, dim_cols = character(0)) {
  stopifnot(is.character(file_path), length(file_path) == 1L)
  stopifnot(is.character(dim_cols))

  empty_result <- list(
    n_obs            = NA_integer_,
    dimensions_n_obs = setNames(integer(0), character(0))
  )

  # Read only the columns we need — one required col for row count +
  # dimension cols for non-NA counts. Using "weight" as the anchor column
  # for n_obs since it is always present and never NA.
  cols_to_read <- unique(c("weight", dim_cols))

  dt <- tryCatch(
    arrow::read_parquet(file_path, col_select = dplyr::all_of(cols_to_read)),
    error = function(e) {
      rlang::warn(paste0(
        "Could not read Parquet file for obs counts: ", file_path,
        "\n  ", conditionMessage(e)
      ))
      NULL
    }
  )

  if (is.null(dt)) return(empty_result)

  n_obs <- nrow(dt)

  if (length(dim_cols) == 0L) {
    return(list(
      n_obs            = as.integer(n_obs),
      dimensions_n_obs = setNames(integer(0), character(0))
    ))
  }

  # Count non-NA observations per dimension column
  present_dim_cols <- intersect(dim_cols, names(dt))
  dim_counts <- vapply(present_dim_cols, function(col) {
    as.integer(sum(!is.na(dt[[col]])))
  }, integer(1L))

  list(
    n_obs            = as.integer(n_obs),
    dimensions_n_obs = dim_counts
  )
}


# ---------------------------------------------------------------------------
# build_manifest_entry()
# ---------------------------------------------------------------------------

#' Build a single manifest survey entry from inventory fields
#'
#' Constructs the list structure for one survey entry in the manifest JSON.
#' Does not perform file I/O — all information comes from the inventory row,
#' the pre-computed `dimensions` vector, and lookup results.
#'
#' @param country_code   ISO3 country code. Partition filter key.
#' @param surveyid_year  Survey year (integer). Partition filter key.
#' @param welfare_type   `"INC"` or `"CON"`. Partition filter key.
#' @param survey_id      Full DLW survey identifier.
#' @param survey_acronym Short survey name (e.g. `"ECH"`).
#' @param version        Combined version string. Partition filter key.
#' @param module         Processing module (e.g. `"ALL"`).
#' @param pip_id         Canonical pip_id string.
#' @param dimensions     Character vector of breakdown dimension column names
#'   available in this survey's Parquet file.
#' @param welfare_vars   Character vector of welfare column names present.
#' @param ppp_sort       Integer PPP base year. `NA_integer_` for legacy surveys.
#' @param country_name   Full country name. `NA_character_` when not available.
#' @param region_name    World Bank region name. `NA_character_` when not available.
#' @param region_code    World Bank region code. `NA_character_` when not available.
#' @param n_obs          Integer. Total number of observations in the Parquet file.
#'   `NA_integer_` when not available.
#' @param dimensions_n_obs Named integer vector. Non-NA observation count per
#'   dimension column. Empty named integer vector when not available.
#'
#' @return A named list with 16 fields suitable for JSON serialisation.
#'
#' @family manifest-generation
#' @export
build_manifest_entry <- function(country_code,
                                 surveyid_year,
                                 welfare_type,
                                 survey_id,
                                 survey_acronym,
                                 version,
                                 module,
                                 pip_id,
                                 dimensions,
                                 welfare_vars       = character(0),
                                 ppp_sort           = NA_integer_,
                                 country_name       = NA_character_,
                                 region_name        = NA_character_,
                                 region_code        = NA_character_,
                                 n_obs              = NA_integer_,
                                 dimensions_n_obs   = setNames(integer(0), character(0))) {
  list(
    pip_id           = as.character(pip_id),
    survey_id        = as.character(survey_id),
    country_code     = as.character(country_code),
    country_name     = as.character(country_name),
    region_name      = as.character(region_name),
    region_code      = as.character(region_code),
    year             = as.integer(surveyid_year),
    welfare_type     = as.character(welfare_type),
    version          = as.character(version),
    survey_acronym   = as.character(survey_acronym),
    module           = as.character(module),
    dimensions       = as.character(dimensions),
    welfare_vars     = as.character(welfare_vars),
    ppp_sort         = if (is.null(ppp_sort) || (length(ppp_sort) == 1L && is.na(ppp_sort)))
                         NA_integer_ else as.integer(ppp_sort),
    n_obs            = if (is.null(n_obs) || (length(n_obs) == 1L && is.na(n_obs)))
                         NA_integer_ else as.integer(n_obs),
    dimensions_n_obs = as.list(dimensions_n_obs)
  )
}


# ---------------------------------------------------------------------------
# generate_release_manifest()
# ---------------------------------------------------------------------------

#' Generate a PIP release manifest JSON
#'
#' Builds a release manifest by scanning the Arrow repository for each
#' survey in the provided inventory, discovering which breakdown dimensions
#' are available, computing observation counts, looking up country and region
#' metadata, and writing a JSON manifest file.
#'
#' @param release          Character scalar. The PIP release identifier.
#' @param arrow_root       Absolute path to the root of the Master Arrow Repository.
#' @param release_inventory A `data.table` from [pipload::load_pip_release_inventory()].
#'   Must contain: `survey_id`, `pip_id`, `country_code`, `surveyid_year`,
#'   `welfare_type`, `survey_acronym`, `vermast`, `veralt`, `module`.
#' @param output_path      Absolute path for the output manifest JSON file.
#' @param set_as_current   Logical. If `TRUE`, writes `current_release.json`.
#'
#' @return A `data.table` summary with columns: `pip_id`, `file_path`,
#'   `status`, `dimensions`, `message`. Returned invisibly.
#'
#' @family manifest-generation
#' @export
generate_release_manifest <- function(release,
                                      arrow_root        = getOption("pipdata.arrow_repo"),
                                      release_inventory,
                                      output_path       = getOption("pipdata.manifest_root"),
                                      set_as_current    = FALSE) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.character(release),     length(release)     == 1L, !is.na(release),
    is.character(arrow_root),  length(arrow_root)  == 1L, !is.na(arrow_root),
    is.character(output_path), length(output_path) == 1L, !is.na(output_path),
    is.logical(set_as_current), length(set_as_current) == 1L
  )
  if (!data.table::is.data.table(release_inventory)) {
    cli::cli_abort("{.arg release_inventory} must be a {.cls data.table}.")
  }

  required_inv_cols <- c(
    "survey_id", "pip_id", "country_code", "surveyid_year",
    "welfare_type", "survey_acronym", "vermast", "veralt", "module"
  )
  missing_inv_cols <- setdiff(required_inv_cols, names(release_inventory))
  if (length(missing_inv_cols) > 0L) {
    cli::cli_abort(
      "release_inventory is missing required column(s): {.field {missing_inv_cols}}"
    )
  }

  arrow_root <- normalizePath(arrow_root, mustWork = FALSE)
  if (!dir.exists(arrow_root)) {
    cli::cli_abort("Arrow repository root does not exist: {.path {arrow_root}}")
  }

  if (dir.exists(output_path)) {
    output_path <- file.path(output_path, paste0("manifest_", release, ".json"))
  }

  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Output directory does not exist: {.path {output_dir}}")
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

  # --- Load country metadata once before the loop ----------------------------
  country_list <- tryCatch(
    pipload::load_aux_data("country_list"),
    error = function(e) {
      cli::cli_warn(c(
        "Could not load country_list auxiliary data: {conditionMessage(e)}.",
        "i" = "country_name, region_name and region_code will be NA."
      ))
      NULL
    }
  )

  # --- Process each pip_id ---------------------------------------------------
  pip_ids_out    <- inv$pip_id
  file_paths     <- character(n_total)
  statuses       <- character(n_total)
  avail_dims     <- character(n_total)
  messages       <- character(n_total)
  survey_entries <- vector("list", n_total)

  for (i in seq_len(n_total)) {
    row_i    <- inv[i]
    pip_id_i <- row_i$pip_id

    version_i <- paste0(tolower(row_i$vermast), "_", tolower(row_i$veralt))

    rel_path_i <- .derive_parquet_path(
      country_code  = row_i$country_code,
      surveyid_year = row_i$surveyid_year,
      welfare_type  = row_i$welfare_type,
      version       = version_i,
      pip_id        = pip_id_i
    )

    abs_path_i    <- file.path(arrow_root, rel_path_i)
    file_paths[i] <- abs_path_i

    # --- File existence check -------------------------------------------------
    if (!file.exists(abs_path_i)) {
      statuses[i]   <- "missing"
      avail_dims[i] <- NA_character_
      messages[i]   <- paste0("Parquet file not found: ", rel_path_i)
      rlang::warn(messages[i])
      next
    }

    # --- Dimension discovery --------------------------------------------------
    dims_i <- discover_parquet_dimensions(abs_path_i)

    if (length(dims_i) == 1L && is.na(dims_i)) {
      statuses[i]   <- "unreadable"
      avail_dims[i] <- NA_character_
      messages[i]   <- paste0("Cannot read Parquet schema: ", rel_path_i)
      next
    }

    avail_dims[i] <- paste(dims_i, collapse = ", ")
    statuses[i]   <- "included"
    messages[i]   <- NA_character_

    # --- Welfare column discovery ---------------------------------------------
    welfare_vars_i <- discover_parquet_welfare_cols(abs_path_i)

    # --- Observation counts ---------------------------------------------------
    obs_counts_i     <- discover_parquet_obs_counts(abs_path_i, dim_cols = dims_i)
    n_obs_i          <- obs_counts_i$n_obs
    dimensions_n_obs_i <- obs_counts_i$dimensions_n_obs

    # --- ppp_sort from Parquet schema metadata --------------------------------
    parquet_schema_i <- tryCatch(
      arrow::read_parquet(abs_path_i, as_data_frame = FALSE)$schema,
      error = function(e) NULL
    )
    ppp_sort_i <- parquet_schema_i$metadata$r$attributes$ppp_sort

    # --- Country metadata lookup ----------------------------------------------
    cl_row <- if (!is.null(country_list))
      country_list[country_code == row_i$country_code][1L]
    else
      NULL

    country_name_i <- if (!is.null(cl_row) && nrow(cl_row) > 0L)
      as.character(cl_row$country_name) else NA_character_

    region_name_i  <- if (!is.null(cl_row) && nrow(cl_row) > 0L)
      as.character(cl_row$region) else NA_character_

    region_code_i  <- if (!is.null(cl_row) && nrow(cl_row) > 0L)
      as.character(cl_row$region_code) else NA_character_

    # --- Build survey entry ---------------------------------------------------
    survey_entries[[i]] <- build_manifest_entry(
      country_code     = row_i$country_code,
      surveyid_year    = row_i$surveyid_year,
      welfare_type     = row_i$welfare_type,
      survey_id        = row_i$survey_id,
      survey_acronym   = row_i$survey_acronym,
      version          = version_i,
      module           = row_i$module,
      pip_id           = pip_id_i,
      dimensions       = dims_i,
      welfare_vars     = welfare_vars_i,
      ppp_sort         = ppp_sort_i,
      country_name     = country_name_i,
      region_name      = region_name_i,
      region_code      = region_code_i,
      n_obs            = n_obs_i,
      dimensions_n_obs = dimensions_n_obs_i
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
      cli::cli_abort(c(
        "Failed to write manifest to {.path {output_path}}",
        "x" = conditionMessage(e)
      ))
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
      cli::cli_abort(c(
        "Failed to write current_release.json to {.path {pointer_path}}",
        "x" = conditionMessage(e)
      ))
    }
  )

  cli::cli_inform(
    c("v" = "current_release.json updated: {.path {pointer_path}}")
  )

  invisible(pointer_path)
}
