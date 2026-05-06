#' Validate a single cleaned survey for deflation
#'
#' @param dt A cleaned survey `data.table` with class `pipmd` or `pipgd`.
#' @return Invisibly `TRUE` if valid; aborts with an informative error otherwise.
#' @noRd
.validate_deflation_input <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    cli::cli_abort(
      "Input must be a {.cls data.table}, not {.cls {class(dt)[1]}}.",
      class = c("validate_deflation_input", "piperr")
    )
  }
  if (!any(c("pipmd", "pipgd") %in% class(dt))) {
    cli::cli_abort(
      "Input must have class {.cls pipmd} or {.cls pipgd}.",
      class = c("validate_deflation_input", "piperr")
    )
  }
  required_cols <- c("welfare", "weight")
  missing_cols  <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Input is missing required columns: {.field {missing_cols}}.",
      class = c("validate_deflation_input", "piperr")
    )
  }
  required_attrs <- c(
    "survey_id",
    "country_code",
    "surveyid_year",
    "survey_acronym",
    "reporting_level",
    "ppp_data_level",
    "cpi_data_level"
  )
  missing_attrs <- setdiff(required_attrs, names(attributes(dt)))
  if (length(missing_attrs) > 0L) {
    cli::cli_abort(
      "Input is missing required attributes: {.field {missing_attrs}}.",
      class = c("validate_deflation_input", "piperr")
    )
  }
  invisible(TRUE)
}

#' Load CPI, PPP, and population metadata for a survey from stamp
#'
#' Uses the master inventory to resolve the metadata stamp version that
#' corresponds to the given `pip_id` and optional data version, then loads
#' the metadata from stamp and returns the CPI, PPP, and population as
#' named numeric vectors (the format produced by [pd_aux_attr()]).
#'
#' @param pip_id Character scalar. The survey identifier (e.g.
#'   `"CHN_2015_CHIP_INC_D1"`).
#' @param version Character scalar or `NULL`. If `NULL`, the most recent
#'   inventory entry for `pip_id` (by `created_at_metadata`) is used. When
#'   supplied, it must match the `content_hash_data` column in the inventory.
#' @return A named list with elements `cpi`, `ppp`, and `pop`, each a named
#'   numeric vector as stored in the `pip_meta` stamp alias.
#' @noRd
.load_deflation_aux <- function(pip_id, version = NULL) {
  inv <- pipload::load_pip_master_inventory()

  row <- inv[inv$pip_id == pip_id, ]
  if (nrow(row) == 0L) {
    cli::cli_abort(
      paste0(
        "No inventory entry found for {.val {pip_id}}. ",
        "Run pd_process_data() first to populate the master inventory."
      ),
      class = c("load_deflation_aux", "piperr")
    )
  }

  if (!is.null(version)) {
    row <- row[row$content_hash_data == version, ]
    if (nrow(row) == 0L) {
      cli::cli_abort(
        "No inventory entry for {.val {pip_id}} at data version {.val {version}}.",
        class = c("load_deflation_aux", "piperr")
      )
    }
  } else {
    # Sort descending on created_at_metadata (ISO timestamp) so head() reliably
    # selects the most recent inventory entry.
    row <- row[order(row$created_at_metadata, decreasing = TRUE), ]
    row <- utils::head(row, 1L)
  }

  if (!"content_hash_metadata" %in% names(row)) {
    cli::cli_abort(
      paste0(
        "Master inventory is missing 'content_hash_metadata' column. ",
        "Re-run update_pip_inventory() to rebuild the inventory."
      ),
      class = c("load_deflation_aux", "piperr")
    )
  }

  # content_hash_metadata in the inventory is stamp's *content hash*, not its
  # internal version_id. Resolve the correct version_id by listing available
  # pip_meta versions via pip_read() — this uses the same stamp path resolution
  # that the subsequent load call will use, avoiding registry mismatches from
  # calling stamp::st_versions() directly on the raw UNC path.
  meta_content_hash <- row$content_hash_metadata[[1L]]

  avail_meta <- pipload::pip_read(
    id = pip_id,
    alias = "pip_meta",
    version = "available"
  )
  idx <- which(avail_meta$content_hash == meta_content_hash)
  if (length(idx) == 0L) {
    # The exact hash from the master inventory is no longer present in stamp
    # (artifact was replaced by a subsequent pd_process_data() run).
    # Abort if nothing is available at all; otherwise warn and use row 1
    # (pip_read(..., "available") returns rows newest-first, so row 1 is latest).
    if (nrow(avail_meta) == 0L) {
      cli::cli_abort(
        paste0(
          "No fallback version available for {.val {pip_id}} in the ",
          "{.val pip_meta} alias. Re-run pd_process_data() to rebuild metadata."
        ),
        class = c("load_deflation_aux", "piperr")
      )
    }
    cli::cli_warn(
      paste0(
        "Could not find a stamp version matching content hash ",
        "{.val {meta_content_hash}} for {.val {pip_id}}. ",
        "The artifact may have been replaced by a newer run. ",
        "Falling back to the most recent available version."
      ),
      class = c("load_deflation_aux_stale_hash", "pipwrn")
    )
    idx <- 1L
  }
  meta_version <- avail_meta$version_id[[idx[[1L]]]]

  meta <- pipload::pip_read(
    id = pip_id,
    alias = "pip_meta",
    version = meta_version
  )

  list(cpi = meta$cpi, ppp = meta$ppp, pop = meta$pop)
}

#' Deflation of welfare using auxiliary data
#'
#' Deflates a single cleaned survey `data.table`. Two input modes:
#'
#' - **Mode A** (`dt`): pass the cleaned survey directly. When
#'   `cpi`/`ppp`/`pop` are `NULL`, auxiliary metadata is loaded automatically
#'   from stamp via the master inventory.
#' - **Mode B** (`pip_id`): pass a survey identifier and optional stamp
#'   version. The survey and metadata are both loaded automatically.
#'
#' To deflate many surveys in a batch, use the future `pd_deflate_pipeline()`
#' wrapper (tracked in the roadmap as `deflate-pipeline-wrapper`), which
#' calls `pd_deflation()` for each survey in an inventory.
#'
#' @param dt A single cleaned survey `data.table` (class `pipmd` or `pipgd`),
#'   or `NULL` when `pip_id` is given instead.
#' @param cpi Named numeric vector of CPI values (as returned by
#'   [pd_aux_attr()]), or a `data.table` from `pipload::pip_load_aux("cpi")`
#'   for the legacy interface. `NULL` triggers inventory-based loading.
#' @param ppp Named numeric vector of PPP values (as returned by
#'   [pd_aux_attr()]), or a `data.table` from `pipload::pip_load_aux("ppp")`
#'   for the legacy interface. `NULL` triggers inventory-based loading.
#' @param pop Named numeric vector of population values (as returned by
#'   [pd_aux_attr()]), or a `data.table` from `pipload::pip_load_aux("pop")`
#'   for the legacy interface. `NULL` triggers inventory-based loading.
#' @param pip_id Character scalar. Survey identifier for Mode B (load from
#'   stamp). Ignored when `dt` is provided.
#' @param version Character scalar or `NULL`. Stamp version used when loading
#'   the survey (Mode B) or resolving the metadata version from the master
#'   inventory.
#'
#' @return The input survey `data.table` augmented with `welfare_lcu` and
#'   `welfare_ppp_*` columns. Returns `NA` when deflation fails (error is
#'   logged via `log_failure()`).
#' @export
#'
#' @note `pd_deflation()` is a single-survey deflation helper. When
#'   `cpi`/`ppp`/`pop` are `NULL` (the default), it resolves the matching
#'   metadata version from the master inventory and loads CPI/PPP/pop
#'   automatically. All package-level environment access uses the unified
#'   `.pipdataenv` via accessor helpers (`pd_env_set()`, `pd_env_get()`,
#'   `pd_env_rm()`).
#'
#' @family pd_process_data pipeline
#'
#' @examples
#' \dontrun{
#' # Mode A: pass survey directly, aux loaded automatically from master inventory
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#' pfw <- pipload::pip_load_aux("pfw")
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' ls  <- pd_cpfw_merge(gd, pfw)
#' x   <- pd_dlw_clean(gd)[["CHN_2015_CHIP_INC_D1"]]
#' pd_deflation(x)
#'
#' # Legacy Mode A: explicit aux tables
#' ppp <- pipload::pip_load_aux("ppp")
#' cpi <- pipload::pip_load_aux("cpi")
#' pop <- pipload::pip_load_aux("pop")
#' pd_deflation(x, cpi = cpi, ppp = ppp, pop = pop)
#'
#' # Mode B: load by survey id
#' pd_deflation(pip_id = "CHN_2015_CHIP_INC_D1")
#' }
pd_deflation <- function(
  dt = NULL,
  cpi = NULL,
  ppp = NULL,
  pop = NULL,
  pip_id = NULL,
  version = NULL
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input resolution   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Mode B: load single survey from stamp by pip_id
  if (is.null(dt)) {
    if (is.null(pip_id)) {
      cli::cli_abort(
        "Either {.arg dt} or {.arg pip_id} must be provided.",
        class = c("pd_deflation", "piperr")
      )
    }
    dt <- pipload::pip_read(id = pip_id, alias = "pip", version = version)
    # stamp round-trips strip the pip S3 class prefix — restore it.
    # Prefer assign_pipclass() (reads the `module` column); fall back to
    # inferring from the pip_id last segment when module was dropped on save.
    if ("module" %in% names(dt)) {
      dt <- pipload::assign_pipclass(dt)
    } else {
      pip_module <- utils::tail(strsplit(pip_id, "_", fixed = TRUE)[[1L]], 1L)
      dt <- if (grepl("GROUP", pip_module, ignore.case = TRUE)) {
        pipload::as_pipgd(dt)
      } else {
        pipload::as_pipmd(dt)
      }
    }
  }

  # Resolve pip_id from survey attributes when not supplied by the caller.
  # Construct using the same logic as cache_id():
  #   {country_code}_{surveyid_year}_{survey_acronym}_{INC|CON}_{module}
  if (is.null(pip_id)) {
    required_id_attrs <- c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "welfare_type",
      "module"
    )
    missing_id_attrs <- setdiff(required_id_attrs, names(attributes(dt)))
    if (length(missing_id_attrs) > 0L) {
      cli::cli_abort(
        "Cannot construct {.arg pip_id}: missing attributes {.field {missing_id_attrs}}.",
        class = c("pd_deflation", "piperr")
      )
    }
    wt_map <- c(income = "INC", consumption = "CON")
    wt <- wt_map[attr(dt, "welfare_type")]
    pip_id <- paste(
      attr(dt, "country_code"),
      attr(dt, "surveyid_year"),
      attr(dt, "survey_acronym"),
      wt,
      attr(dt, "module"),
      sep = "_"
    )
  }

  # Determine whether aux was provided explicitly (legacy path)
  use_legacy <- !is.null(cpi) && !is.null(ppp) && !is.null(pop)

  if (use_legacy) {
    if (data.table::is.data.table(ppp)) {
      ppp <- ppp_to_wide(ppp = ppp)
    }
    if (data.table::is.data.table(cpi) && "cpi2005_SM21" %in% names(cpi)) {
      data.table::setnames(cpi, "cpi2005_SM21", "cpi2005") # temporal solution
    }
  } else {
    aux <- .load_deflation_aux(pip_id = pip_id, version = version)
    cpi <- aux$cpi
    ppp <- aux$ppp
    pop <- aux$pop
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Deflate   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  deflation(dt, cpi = cpi, ppp = ppp, pop = pop)
}

#' Deflation of welfare using auxiliary data (lower level)
#'
#' @param dt data.table of cleaned DLW survey from `wbpip_clean`
#' @inheritParams pd_deflation
#' @param ... extra arguments
#'
#' @return data.table
#' @export
deflation <- function(dt,  cpi, ppp, pop,...) {
  UseMethod("deflation")
}

#' Deflation of welfare for micro data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
deflation.pipmd <- function(dt, cpi, ppp, pop, ...) {
  .validate_deflation_input(dt)
  safe_deflation(dt, cpi, ppp, pop, .deflation_pipmd_core)
}

#' Deflation of welfare for group data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
deflation.pipgd <- function(dt, cpi, ppp, pop, ...) {
  .validate_deflation_input(dt)
  safe_deflation(dt, cpi, ppp, pop, .deflation_pipgd_core)
}

#' Shared tryCatch scaffold for deflation S3 methods
#'
#' Sets the `log_survey_id` environment variable, wraps `deflation_fn` in a
#' tryCatch, and returns `NA` with a log entry on failure.
#'
#' @param dt A cleaned survey `data.table`.
#' @param cpi CPI aux (named numeric vector or `data.table`).
#' @param ppp PPP aux (named numeric vector or `data.table`).
#' @param pop Population aux (named numeric vector or `data.table`).
#' @param deflation_fn A function accepting `(dt, cpi, ppp, pop)` that
#'   performs the actual deflation logic.
#' @return The result of `deflation_fn`, or `NA` if it errors.
#' @noRd
safe_deflation <- function(dt, cpi, ppp, pop, deflation_fn) {
  pd_env_set("log_survey_id", attr(dt, "survey_id"))
  on.exit(pd_env_rm("log_survey_id"))

  tryCatch(
    expr = deflation_fn(dt, cpi, ppp, pop),
    error = function(cnd) {
      survey_id <- pd_env_get("log_survey_id")
      cli::cli_alert("The survey {survey_id} was skipped")
      log_failure(cnd)
      NA
    }
  )
}

#' Core deflation logic for micro-data surveys
#'
#' @param dt Cleaned `pipmd` data.table (copy will be made internally).
#' @param cpi CPI aux (named numeric vector or `data.table`).
#' @param ppp PPP aux (named numeric vector or wide `data.table` after
#'   [ppp_to_wide()]).
#' @param pop Population aux (named numeric vector or `data.table`).
#' @return Deflated `data.table` with `welfare_lcu` and `welfare_ppp_*`
#'   columns plus factor-formatted character columns.
#' @noRd
.deflation_pipmd_core <- function(dt, cpi, ppp, pop) {
  dt_c <- data.table::copy(dt)
  cpi <- if (data.table::is.data.table(cpi)) data.table::copy(cpi) else cpi
  ppp <- if (data.table::is.data.table(ppp)) data.table::copy(ppp) else ppp
  pop <- if (data.table::is.data.table(pop)) data.table::copy(pop) else pop

  dt_c <- restore_data_level_cols(dt_c)
  dt_c <- add_rep_lvl(dt_c)
  dt_c <- add_aux(dt_c, ppp, cpi)
  dt_c <- welfare_lcu(dt_c)
  dt_c <- deflate_wlf(dt_c)

  if (length(dt_c[, unique(reporting_level)]) > 1L) {
    dt_c <- adjust_population(dt_c, pop)
  }

  char_to_fct(dt_c)
}

#' Core deflation logic for grouped-data surveys
#'
#' @inheritParams .deflation_pipmd_core
#' @return Deflated `data.table` with `welfare_lcu` and `welfare_ppp_*`
#'   columns plus factor-formatted character columns.
#' @noRd
.deflation_pipgd_core <- function(dt, cpi, ppp, pop) {
  dt_c <- data.table::copy(dt)
  cpi <- if (data.table::is.data.table(cpi)) data.table::copy(cpi) else cpi
  ppp <- if (data.table::is.data.table(ppp)) data.table::copy(ppp) else ppp

  dt_c <- restore_data_level_cols(dt_c)
  dt_c <- add_rep_lvl(dt_c)
  dt_c <- add_aux(dt_c, ppp, cpi)
  dt_c <- welfare_lcu(dt_c)
  dt_c <- deflate_wlf(dt_c)

  char_to_fct(dt_c)
}


#' Restore data-level columns from attributes
#'
#' When surveys are round-tripped through stamp, `ppp_data_level`,
#' `cpi_data_level`, and `pop_data_level` are stored as object attributes
#' rather than columns. This function materialises them as constant columns so
#' that `add_ppp()`, `add_cpi()`, and `adjust_population()` can join on them.
#'
#' @param dt A `data.table`.
#' @return `dt` with any missing `*_data_level` columns added.
#' @note Mutates `dt` by reference via `:=`. Pass `data.table::copy(dt)` if
#'   the original must be preserved.
#' @keywords internal
restore_data_level_cols <- function(dt) {
  level_attrs <- c("ppp_data_level", "cpi_data_level", "pop_data_level")
  for (col in level_attrs) {
    if (!col %in% names(dt)) {
      val <- attr(dt, col)
      if (!is.null(val)) {
        dt[, (col) := val]
      }
    }
  }
  dt
}

#' Convert PPP data from `pipload` to wide format
#'
#' @param ppp data frame with ppp data from `pipload::pip_load_aux("ppp")`
#'
#' @return data.table with PPP values to wide format based on versioning
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' ppp <-  pipload::pip_load_aux("ppp")
#' x   <-  ppp_to_wide(ppp)
#' names(x)
#' }
ppp_to_wide <- function(ppp) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Identify ppp versions --------

  ppp[,
      ppp_version := {
        x <- paste0("ppp_", ppp_year, "_", release_version, "_", adaptation_version)
        x <- gsub("_v", "_0", x )
      }
  ]

  ppp_v <- ppp[, unique(ppp_version)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Transfer ppp data.table from long to wide --------

  ppp <- dcast(ppp,
               formula = country_code + ppp_data_level ~ ppp_version,
               value.var = "ppp",
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add all ppp_version to attributes --------

  setattr(ppp, "ppp_versions", ppp_v)

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(ppp)

}

#' Identify reporting level from data_level variables
#'
#' @param dt data.table
#'
#' @return data.table with reporting_level variable
#' @keywords internal
add_rep_lvl <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dl_var        <- grep("data_level", names(dt), value = TRUE) # data_level vars

  if (length(dl_var) == 0L) {
    # data_level info is stored as attributes (stamp round-trip strips columns)
    dt_attrs <- attributes(dt)
    rep_lvl <- if (!is.null(dt_attrs$ppp_data_level)) {
      dt_attrs$ppp_data_level
    } else {
      dt_attrs$cpi_data_level
    }
    if (is.null(rep_lvl)) {
      cli::cli_abort(
        "Cannot determine reporting level: no {.val data_level} columns or attributes found in {.arg dt}.",
        class = c("add_rep_lvl", "piperr")
      )
    }
    dt[, reporting_level := rep_lvl]
  } else {
    ordered_level <- purrr::map_dbl(dl_var, ~ get_ordered_level(dt, .x))
    report_lvl_cpfw <- as.numeric(attributes(dt)$reporting_level)
    select_var <- dl_var[ordered_level == report_lvl_cpfw]
    dt[, reporting_level := get(select_var[1])]
  }

  setorder(dt, reporting_level)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Add auxiliary data for deflation
#'
#' @inheritParams pd_deflation
#' @return data.table
#' @keywords internal
add_aux <- function(dt, ppp ,cpi) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      ### Merge ppp ---------

      dt <- add_ppp(dt, ppp)

      ### Merge cpi ---------

      dt <- add_cpi(dt, cpi)

      ### Check and add base years

      dt <- cpi_ppp_years(dt, ppp)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Merge survey with PPP
#'
#' Accepts either a named numeric vector (format produced by [pd_aux_attr()])
#' or a wide `data.table` (legacy format from [ppp_to_wide()]).
#'
#' Named vector names follow the pattern
#' `ppp_{ppp_year}_{release_version}_{adaptation_version}_{reporting_level}`,
#' e.g. `"ppp_2017_01_01_national"`. Each unique version becomes a column in
#' `dt` with the matching value looked up via `ppp_data_level`.
#'
#' @param dt A cleaned survey `data.table` with a `ppp_data_level` column.
#' @param ppp Named numeric vector or wide `data.table`.
#' @return `dt` augmented with one column per PPP version and a `ppp_versions`
#'   attribute listing the version names.
#' @keywords internal
add_ppp <- function(dt, ppp) {

  if (data.table::is.data.table(ppp)) {
    # Legacy data.table path: ppp already in wide format from ppp_to_wide()
    ppp_c <- ppp[ppp$country_code == attributes(dt)$country_code$values]
    dt <- joyn::merge(
      dt,
      ppp_c,
      by = "ppp_data_level",
      match_type = "m:1",
      keep = "left",
      reportvar = FALSE,
      verbose = FALSE
    )
    return(dt)
  }

  # Named-vector path: names = "{ppp_version}_{reporting_level}"
  # ppp_version = "ppp_{year}_{rel}_{adapt}" (exactly 4 underscore-separated
  # segments), so split at position 5 to separate version from reporting level.
  nm <- names(ppp)
  parts <- strsplit(nm, "_", fixed = TRUE)
  ppp_versions <- vapply(
    parts,
    function(p) paste(p[seq_len(4L)], collapse = "_"),
    character(1L)
  )
  report_levels <- vapply(
    parts,
    function(p) paste(p[-seq_len(4L)], collapse = "_"),
    character(1L)
  )

  unique_versions <- unique(ppp_versions)
  for (v in unique_versions) {
    idx <- ppp_versions == v
    lev_map <- stats::setNames(ppp[idx], report_levels[idx])
    dt[, (v) := lev_map[ppp_data_level]]
  }

  data.table::setattr(dt, "ppp_versions", unique_versions)
  return(dt)
}

#' Merge survey with CPI
#'
#' Accepts either a named numeric vector (format produced by [pd_aux_attr()])
#' or a `data.table` (legacy format from `pipload::pip_load_aux("cpi")`).
#'
#' Named vector names follow the pattern `{cpi_year}_{reporting_level}`,
#' e.g. `"2017_national"`. Each unique year becomes a `cpiYYYY` column in `dt`
#' with the matching value looked up via `cpi_data_level`.
#'
#' @param dt A cleaned survey `data.table` with a `cpi_data_level` column.
#' @param cpi Named numeric vector or `data.table`.
#' @return `dt` augmented with one `cpiYYYY` column per base year and a
#'   `cpi_years` attribute listing the year strings.
#' @keywords internal
add_cpi <- function(dt, cpi) {

  if (data.table::is.data.table(cpi)) {
    # Legacy data.table path
    con <- attr(dt, "country_code")
    svy_year <- attr(dt, "surveyid_year")
    svy_acr <- attr(dt, "survey_acronym")
    cpi_c <- cpi[
      country_code == con & survey_year == svy_year & survey_acronym == svy_acr
    ]
    cpi_vars <- grep("^cpi[0-9]{4}$", names(cpi_c), value = TRUE)
    cpi_years <- gsub("cpi([0-9]+)", "\\1", cpi_vars) |> unique() |> sort()
    data.table::setattr(dt, "cpi_years", cpi_years)
    cpi_to_keep <- c("cpi_data_level", cpi_vars)
    cpi_c <- cpi_c[, ..cpi_to_keep]
    dt <- joyn::merge(
      dt,
      cpi_c,
      by = "cpi_data_level",
      match_type = "m:1",
      keep = "left",
      reportvar = FALSE,
      verbose = FALSE
    )
    return(dt)
  }

  # Named-vector path: names = "{cpi_year}_{reporting_level}"
  # cpi_year is a 4-digit integer, always the first underscore-delimited segment.
  nm <- names(cpi)
  cpi_years <- sub("^([0-9]+)_.*$", "\\1", nm)
  report_levels <- sub("^[0-9]+_(.+)$", "\\1", nm)

  unique_years <- unique(cpi_years)
  data.table::setattr(dt, "cpi_years", unique_years)

  for (yr in unique_years) {
    col <- paste0("cpi", yr)
    idx <- cpi_years == yr
    lev_map <- stats::setNames(cpi[idx], report_levels[idx])
    dt[, (col) := lev_map[cpi_data_level]]
  }

  return(dt)
}

#' Identify base years for deflation
#'
#' Compares available CPI and PPP years (from `dt` attributes set by
#' [add_cpi()] and [add_ppp()]) and sets a `base_years` attribute on `dt`.
#' When `ppp` is a named numeric vector the PPP versions are read from the
#' `ppp_versions` attribute of `dt` (set by [add_ppp()]); when `ppp` is a
#' `data.table` the versions come from its own `ppp_versions` attribute.
#'
#' @param dt A `data.table` that has already been processed by [add_cpi()] and
#'   [add_ppp()].
#' @param ppp Named numeric vector or wide PPP `data.table` (used only to
#'   locate the `ppp_versions` attribute).
#' @return `dt` with a `base_years` attribute.
#' @keywords internal
cpi_ppp_years <- function(dt, ppp) {
  tryCatch(
    expr = {
      # Named-vector path: ppp_versions attribute was placed on dt by add_ppp().
      # Legacy DT path: ppp_versions attribute lives on the ppp data.table.
      ppp_versions <- attr(dt, "ppp_versions")
      if (is.null(ppp_versions)) {
        ppp_versions <- attr(ppp, "ppp_versions")
      }

      if (is.null(ppp_versions)) {
        cli::cli_abort(
          "Cannot determine ppp_versions. Ensure add_ppp() ran before cpi_ppp_years()."
        )
      }

      ppp_years <- gsub("ppp_([0-9]+)(.*)", "\\1", ppp_versions) |>
        unique() |>
        sort()

      cpi_years <- attributes(dt)$cpi_years

      if (setequal(cpi_years, ppp_years)) {
        data.table::setattr(dt, "base_years", cpi_years)
      } else {
        base_ys <- intersect(cpi_years, ppp_years)
        data.table::setattr(dt, "base_years", base_ys)
        piperr(
          message = paste0(
            "CPI and PPP years available do NOT match. ",
            "Only the intersect will be used: {.field {base_ys}}"
          ),
          name = "cpi_ppp"
        )
      }
    },
    cpi_ppp = function(cnd) {
      log_failure(cnd)
    }
  )

  return(dt)
}


#' Create welfare_lcu variable
#'
#' @inheritParams pd_deflation
#'
#' @return data.table
#' @keywords internal
welfare_lcu <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt[,
     welfare_lcu := welfare
  ]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}


#' Deflate welfare vector
#'
#' @param dt data.table with welfare LCU
#'
#' @return data.table
#' @keywords internal
deflate_wlf <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  base_years <- attributes(dt)$base_years

  dt_c <- copy(dt) # Fix copy

  # get_welfare_ppp mutates dt_c in-place via data.table := assignment.
  # purrr::walk is used instead of purrr::map to make the side-effect
  # intent explicit and avoid silently discarding the return values.
  purrr::walk(base_years, get_welfare_ppp, dt = dt_c)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  return(dt_c)

}

#' Defate welfare variable to PPP values
#'
#' @param dt_wlcu data.table with welfare variable called `welfare_lcu`
#' @param base_year numeric: base year
#'
#' @return data.table with welfare in PPP values
#' @noRd
get_welfare_ppp <- function(dt_wlcu, base_year) {

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  cpiv     <- paste0("cpi", base_year)

  ppp_vars  <- grep("^ppp_[0-9]{4}", names(dt_wlcu), value = TRUE)
  ppp_pat   <- paste0("^ppp_", base_year)
  ppp_vars  <- grep(ppp_pat, names(dt_wlcu), value = TRUE)

  welf_vars <- glue("welfare_{ppp_vars}")

  dt_wlcu[,
     (welf_vars) := lapply(.SD, \(v) {
       wbpip::deflate_welfare_mean(
         welfare_mean = welfare_lcu,
         ppp          = v,
         cpi          = get(cpiv)
       )
     }),
     .SDcols = ppp_vars]

  dt_wlcu <- dt_wlcu[,
           ..welf_vars]


  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(dt_wlcu)
}

#' Scale subnational population weights to national accounts (WDI).
#'
#' Accepts either a named numeric vector (format produced by [pd_aux_attr()])
#' or a `data.table` (legacy format from `pipload::pip_load_aux("pop")`).
#'
#' Named vector names follow the pattern `{year}_{reporting_level}`,
#' e.g. `"2015_national"`. For each reporting level in `df`, the entry with
#' the closest year to `survey_year` is used as the WDI population figure;
#' when multiple entries tie on distance they are inverse-distance-weighted.
#'
#' Helper moved here from pd_add_pip_vars.R (archived 2026-04-30) since
#' pd_deflation.R is the only active caller. Not exported.
#'
#' @param df  A `data.table` with columns `country_code`, `survey_year`,
#'   `reporting_level`, and `weight`. The caller passes a copy so
#'   reference-semantics mutation does not affect the source object.
#' @param pop Named numeric vector or a `data.table` with columns
#'   `country_code`, `year`, `pop_data_level`, and `pop`.
#' @return `df` with `weight` rescaled to match the WDI population figure.
#' @noRd
adjust_population <- function(df, pop) {

  if (data.table::is.data.table(pop)) {
    # Legacy data.table path — original implementation.
    spop <- df[,
      .(weight = sum(weight, na.rm = TRUE)),
      by = c("country_code", "survey_year", "reporting_level")
    ]
    # Rename pop_data_level → reporting_level for a clean two-key join
    # (avoids by.x/by.y recycling warning in joyn 0.3.0).
    pop_r <- data.table::copy(pop)
    data.table::setnames(pop_r, "pop_data_level", "reporting_level")
    dpop <- joyn::inner_join(
      x = pop_r,
      y = spop,
      by = c("country_code", "reporting_level"),
      relationship = "many-to-one",
      reportvar = FALSE
    )
    dpop <-
      dpop[,
        diff_year := abs(year - survey_year)
      ][,
        .SD[diff_year == min(diff_year)],
        by = reporting_level
      ][,
        wght := data.table::fifelse(diff_year == 0, 1, 1 / diff_year)
      ]
    fact <-
      dpop[,
        lapply(.SD, stats::weighted.mean, w = wght),
        by = "reporting_level",
        .SDcols = c("pop", "weight")
      ][,
        pop_fact := pop / weight
      ][,
        c("pop", "weight") := NULL
      ]
    df <- joyn::left_join(
      x = df,
      y = fact,
      by = "reporting_level",
      relationship = "many-to-one",
      reportvar = FALSE
    )
    df[, weight := weight * pop_fact]
    return(df)
  }

  # Named-vector path: names = "{year}_{reporting_level}"
  survey_year <- df$survey_year[[1L]]
  nm <- names(pop)
  pop_years <- as.integer(sub("^([0-9]+)_.*$", "\\1", nm))
  pop_levels <- sub("^[0-9]+_(.+)$", "\\1", nm)

  spop <- df[,
    .(weight = sum(weight, na.rm = TRUE)),
    by = "reporting_level"
  ]

  fact_rows <- lapply(spop$reporting_level, function(rl) {
    idx <- pop_levels == rl
    if (!any(idx)) {
      return(NULL)
    }
    yrs <- pop_years[idx]
    vals <- pop[idx]
    diffs <- abs(yrs - survey_year)
    min_d <- min(diffs)
    keep <- diffs == min_d
    if (sum(keep) == 1L || min_d == 0L) {
      pop_val <- vals[which(keep)[[1L]]]
    } else {
      wts <- 1 / diffs[keep]
      pop_val <- stats::weighted.mean(vals[keep], w = wts)
    }
    sw <- spop[spop$reporting_level == rl, weight]
    data.table::data.table(reporting_level = rl, pop_fact = pop_val / sw)
  })

  fact <- data.table::rbindlist(Filter(Negate(is.null), fact_rows))

  if (nrow(fact) == 0L) {
    cli::cli_abort(
      "No population data found for reporting levels in survey.",
      class = c("adjust_population", "piperr")
    )
  }

  df <- joyn::left_join(
    x = df,
    y = fact,
    by = "reporting_level",
    relationship = "many-to-one",
    reportvar = FALSE
  )
  df[, weight := weight * pop_fact]
  return(df)
}
