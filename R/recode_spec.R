# ── Internal constants ────────────────────────────────────────────────────────

.known_recode_types <- c(
  "range_clamp",
  "binary_map",
  "haven_labels",
  "binned_from_continuous",
  "quantile_from_continuous"
)

# ── Loaders ───────────────────────────────────────────────────────────────────

#' @keywords internal
load_package_recode_spec <- function() {
  spec_path <- system.file("extdata", "recode_spec.yml", package = "pipdata")
  if (!file.exists(spec_path)) {
    cli::cli_abort(
      c(
        "recode_spec.yml not found in inst/extdata/",
        "i" = "Expected path: {.path {spec_path}}"
      ),
      class = c("recode_spec_missing", "piperr")
    )
  }
  spec <- yaml::read_yaml(spec_path)
  validate_recode_spec(spec)
  spec
}

#' @keywords internal
load_stamp_recode_spec <- function(alias = "pip_inv", verbose = FALSE) {
  tryCatch(
    pipload::pip_read(
      id      = "recode_spec",
      format  = "qs2",
      alias   = alias,
      verbose = verbose
    ),
    error = function(e) NULL
  )
}

# ── Validation ────────────────────────────────────────────────────────────────

#' Validate recode spec schema
#' @keywords internal
validate_recode_spec <- function(spec) {
  if (is.null(spec$schema_version)) {
    cli::cli_abort(
      "recode_spec missing {.field schema_version}",
      class = c("recode_spec_invalid", "piperr")
    )
  }

  for (var_name in names(spec$variables)) {
    rule <- spec$variables[[var_name]]

    if (is.null(rule$type)) {
      cli::cli_abort(
        "Variable {.field {var_name}} missing {.field type}",
        class = c("recode_spec_invalid", "piperr")
      )
    }
    if (is.null(rule$recode_type)) {
      cli::cli_abort(
        "Variable {.field {var_name}} missing {.field recode_type}",
        class = c("recode_spec_invalid", "piperr")
      )
    }
    if (!rule$recode_type %in% .known_recode_types) {
      cli::cli_abort(
        c(
          "Variable {.field {var_name}} has unknown recode_type {.val {rule$recode_type}}",
          "i" = "Known types: {.val {(.known_recode_types)}}"
        ),
        class = c("recode_spec_invalid", "piperr")
      )
    }

    switch(rule$recode_type,
      range_clamp = {
        if (is.null(rule$valid_range)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (range_clamp) missing {.field valid_range}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
      },
      binary_map = {
        if (is.null(rule$mapping)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (binary_map) missing {.field mapping}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
        if (length(rule$mapping) != 2L) {
          cli::cli_abort(
            c(
              "Variable {.field {var_name}} (binary_map) must have exactly 2 mapping entries",
              "x" = "Found {length(rule$mapping)}: {.val {names(rule$mapping)}}"
            ),
            class = c("recode_spec_invalid", "piperr")
          )
        }
      },
      haven_labels = {
        if (is.null(rule$mapping)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (haven_labels) missing {.field mapping}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
      },
      binned_from_continuous = {
        if (is.null(rule$source_column)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (binned_from_continuous) missing {.field source_column}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
        if (is.null(rule$bin_rules)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (binned_from_continuous) missing {.field bin_rules}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
        if (is.null(rule$mapping)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (binned_from_continuous) missing {.field mapping}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
      },
      quantile_from_continuous = {
        if (is.null(rule$source_column)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (quantile_from_continuous) missing {.field source_column}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
        if (is.null(rule$mapping)) {
          cli::cli_abort(
            "Variable {.field {var_name}} (quantile_from_continuous) missing {.field mapping}",
            class = c("recode_spec_invalid", "piperr")
          )
        }
      }
    )
  }

  TRUE
}

# ── Sync ──────────────────────────────────────────────────────────────────────

#' Sync recode spec from package to stamp
#'
#' Compares the package YAML (`inst/extdata/recode_spec.yml`) to the latest
#' stamp version. If different (or no stamp version exists), saves a new
#' version. Returns the active spec and its stamp version_id.
#'
#' @param alias Stamp alias. Default: `"pip_inv"`.
#' @param verbose Logical; show sync messages?
#' @return Named list: `spec` (full recode spec list), `version_id` (character).
#' @keywords internal
sync_recode_spec <- function(alias = "pip_inv", verbose = TRUE) {
  pkg_spec   <- load_package_recode_spec()
  stamp_spec <- load_stamp_recode_spec(alias = alias, verbose = FALSE)

  if (is.null(stamp_spec)) {
    if (verbose) cli::cli_alert_info("No recode_spec in stamp. Saving first version...")

    ventry <- pipload::pip_write(
      pkg_spec,
      id      = "recode_spec",
      format  = "qs2",
      alias   = alias,
      verbose = verbose
    )

    if (verbose) cli::cli_alert_success("Created recode_spec (version: {ventry$version_id})")
    return(list(spec = pkg_spec, version_id = ventry$version_id))
  }

  pkg_hash   <- digest::digest(pkg_spec,   algo = "xxhash64")
  stamp_hash <- digest::digest(stamp_spec, algo = "xxhash64")

  if (identical(pkg_hash, stamp_hash)) {
    cat        <- stamp::st_catalog_query(alias = alias)
    recode_row <- cat[grepl("recode_spec", cat$path, fixed = TRUE), ]
    version_id <- if (nrow(recode_row) > 0L) recode_row$version_id[[1L]] else NA_character_

    if (verbose) cli::cli_alert_info("recode_spec unchanged (version: {version_id})")
    return(list(spec = stamp_spec, version_id = version_id))
  }

  if (verbose) cli::cli_alert_warning("recode_spec changed. Saving new version...")

  ventry <- pipload::pip_write(
    pkg_spec,
    id      = "recode_spec",
    format  = "qs2",
    alias   = alias,
    verbose = verbose
  )

  if (verbose) cli::cli_alert_success("Saved recode_spec (version: {ventry$version_id})")
  return(list(spec = pkg_spec, version_id = ventry$version_id))
}

# ── Typed handlers ────────────────────────────────────────────────────────────

#' @keywords internal
recode_range <- function(dt, var_name, valid_range) {
  if (!var_name %in% names(dt)) return(invisible(dt))

  min_val <- valid_range[[1L]]
  max_val <- valid_range[[2L]]

  data.table::set(dt, j = var_name, value = as.double(dt[[var_name]]))

  x <- dt[[var_name]]
  data.table::set(dt, j = var_name,
    value = data.table::fcase(
      x < min_val,                 NA_real_,
      x >= min_val & x <= max_val, x,
      x > max_val,                 NA_real_,
      default = NA_real_
    )
  )
  invisible(dt)
}

#' @keywords internal
recode_binary <- function(dt, var_name, mapping) {
  if (!var_name %in% names(dt)) return(invisible(dt))

  keys <- as.integer(names(mapping))
  vals <- as.character(unlist(mapping, use.names = FALSE))

  x <- dt[[var_name]]
  data.table::set(dt, j = var_name,
    value = data.table::fcase(
      x == keys[[1L]], vals[[1L]],
      x == keys[[2L]], vals[[2L]],
      default = NA_character_
    )
  )
  invisible(dt)
}

#' @keywords internal
recode_haven <- function(dt, var_name, mapping) {
  if (!var_name %in% names(dt)) return(invisible(dt))

  keys <- as.integer(names(mapping))
  vals <- as.character(unlist(mapping, use.names = FALSE))

  data.table::set(dt, j = var_name,
    value = vals[match(dt[[var_name]], keys)]
  )
  invisible(dt)
}

#' @keywords internal
recode_binned <- function(dt, var_name, source_col, bin_rules, mapping) {
  if (!source_col %in% names(dt)) return(invisible(dt))

  keys <- as.integer(names(mapping))
  vals <- as.character(unlist(mapping, use.names = FALSE))

  data.table::set(dt, j = var_name, value = NA_character_)

  for (rule in bin_rules) {
    bin_label <- vals[match(rule$bin, keys)]
    cond_expr <- parse(text = rule$condition)[[1L]]
    rows_idx  <- dt[eval(cond_expr), which = TRUE]
    if (length(rows_idx) > 0L) {
      data.table::set(dt, i = rows_idx, j = var_name, value = bin_label)
    }
  }

  invisible(dt)
}

#' @keywords internal
recode_quantile <- function(dt, var_name, source_col, mapping, weight_col = NULL) {
  if (!source_col %in% names(dt)) return(invisible(dt))

  n_groups <- length(mapping)
  keys     <- as.integer(names(mapping))
  vals     <- as.character(unlist(mapping, use.names = FALSE))

  x <- dt[[source_col]]
  w <- if (!is.null(weight_col) && weight_col %in% names(dt)) {
    dt[[weight_col]]
  } else {
    rep(1, length(x))
  }

  q_upper <- wbpip::md_compute_quantiles(
    welfare    = x,
    weight     = w,
    n_quantile = n_groups
  )

  # q_upper[n_groups] is the max value; replace with Inf so cut() assigns all
  breaks <- c(-Inf, q_upper[-n_groups], Inf)
  codes  <- as.integer(cut(x, breaks = unique(breaks),
                           labels = FALSE, include.lowest = TRUE))
  data.table::set(dt, j = var_name,
    value = vals[match(codes, keys)]
  )
  invisible(dt)
}

# ── Structural modifier ───────────────────────────────────────────────────────

#' Normalise subnatid column hierarchy
#'
#' Shifts existing `subnatidN` columns up by one (`subnatid1` → `subnatid2`,
#' etc.) then renames `subnatid` → `subnatid1`. No-op if no plain `subnatid`
#' column exists. Called explicitly in `dlw_clean.pipmd()` before
#' `apply_recode_spec()` — structural renames that are not variable-level
#' recodes live here, not in the YAML spec.
#'
#' @param dt data.table
#' @return `dt` modified by reference via `setnames()`
#' @keywords internal
shift_subnatid <- function(dt) {
  if (!"subnatid" %in% colnames(dt)) return(invisible(dt))

  subnatid_cols <- grep("^subnatid[0-9]+$", colnames(dt), value = TRUE)

  if (length(subnatid_cols) > 0L) {
    nums    <- as.integer(gsub("subnatid", "", subnatid_cols))
    max_num <- max(nums)
    for (i in seq(max_num, 1L, by = -1L)) {
      old_nm <- paste0("subnatid", i)
      new_nm <- paste0("subnatid", i + 1L)
      if (old_nm %in% colnames(dt)) {
        data.table::setnames(dt, old_nm, new_nm)
      }
    }
  }

  data.table::setnames(dt, "subnatid", "subnatid1")
  invisible(dt)
}

# ── Dispatcher ────────────────────────────────────────────────────────────────

#' Apply recode specification to a data.table
#'
#' Reads the recode spec from stamp (synced once upstream by [sync_recode_spec()])
#' and applies all matching rules to `dt`.
#'
#' **Replace-type recodes** (`range_clamp`, `binary_map`, `haven_labels`): if
#' `source_column` differs from `var_name`, the source column is **renamed** to
#' `var_name` after the recode (dropping the source). Example: `urban → area`,
#' `male → gender`.
#'
#' **Derive-type recodes** (`binned_from_continuous`,
#' `quantile_from_continuous`): the source column is preserved and `var_name`
#' is added as a new column. Example: `age` stays, `age_group` is added.
#'
#' The stamp `version_id` of the active spec is attached as attribute
#' `"recode_spec_version_id"` on the returned `dt`.
#'
#' @param dt data.table with DLW survey data.
#' @param alias Stamp alias. Default: `"pip_inv"`.
#' @param verbose Logical. Default: `TRUE`.
#' @return `dt` (modified by reference) with attribute `recode_spec_version_id`.
#' @export
apply_recode_spec <- function(dt, alias = "pip_inv", verbose = TRUE) {
  stamp_spec <- load_stamp_recode_spec(alias = alias, verbose = FALSE)
  if (is.null(stamp_spec)) {
    cli::cli_abort(
      c(
        "No recode_spec found in stamp.",
        "i" = "Call {.fn sync_recode_spec} before processing surveys."
      ),
      class = c("recode_spec_missing", "piperr")
    )
  }
  spec <- stamp_spec$variables

  cat        <- stamp::st_catalog_query(alias = alias)
  recode_row <- cat[grepl("recode_spec", cat$path, fixed = TRUE), ]
  version_id <- if (nrow(recode_row) > 0L) recode_row$version_id[[1L]] else NA_character_

  .replace_types <- c("range_clamp", "binary_map", "haven_labels")

  recoded_vars <- character(0L)

  for (var_name in names(spec)) {
    rule       <- spec[[var_name]]
    actual_col <- if (!is.null(rule$source_column)) rule$source_column else var_name

    if (!actual_col %in% names(dt)) next

    switch(rule$recode_type,
      range_clamp =
        recode_range(dt, actual_col, rule$valid_range),
      binary_map =
        recode_binary(dt, actual_col, rule$mapping),
      haven_labels =
        recode_haven(dt, actual_col, rule$mapping),
      binned_from_continuous =
        recode_binned(dt, var_name, actual_col, rule$bin_rules, rule$mapping),
      quantile_from_continuous =
        recode_quantile(dt, var_name, actual_col, rule$mapping,
                        weight_col = rule$weight_col)
    )

    if (rule$recode_type %in% .replace_types &&
        !is.null(rule$source_column) &&
        rule$source_column != var_name &&
        rule$source_column %in% names(dt)) {
      data.table::setnames(dt, old = rule$source_column, new = var_name)
    }

    if (identical(rule$type, "factor") && var_name %in% names(dt)) {
      lvls <- if (!is.null(rule$mapping)) {
        as.character(unlist(rule$mapping, use.names = FALSE))
      } else {
        NULL
      }
      data.table::set(dt, j = var_name,
        value = factor(dt[[var_name]], levels = lvls)
      )
    }

    recoded_vars <- c(recoded_vars, var_name)
  }

  if (verbose && length(recoded_vars) > 0L) {
    pipfun::log_info(
      sprintf(
        "Recoded %d variable(s): %s (spec version: %s)",
        length(recoded_vars),
        paste(recoded_vars, collapse = ", "),
        version_id
      ),
      name = "pipdata_log"
    )
  }

  data.table::setattr(dt, "recode_spec_version_id", version_id)
  dt
}

# ── Utilities ─────────────────────────────────────────────────────────────────

#' Export recode spec from stamp to YAML
#'
#' @param path Output file path. `NULL` prints to console.
#' @param version Stamp `version_id`. `NULL` uses latest.
#' @param alias Stamp alias.
#' @export
export_recode_spec_yaml <- function(path = NULL, version = NULL, alias = "pip_inv") {
  spec <- pipload::pip_read(
    "recode_spec",
    format  = "qs2",
    alias   = alias,
    version = version,
    verbose = FALSE
  )
  if (is.null(path)) {
    cat(yaml::as.yaml(spec))
  } else {
    yaml::write_yaml(spec, path)
    cli::cli_alert_success("Exported to: {.path {path}}")
  }
  invisible(spec)
}

#' List recode_spec versions from stamp catalog
#'
#' @param alias Stamp alias.
#' @export
list_recode_spec_versions <- function(alias = "pip_inv") {
  cat <- stamp::st_catalog_query(alias = alias)
  cat[grepl("recode_spec", cat$path, fixed = TRUE), ]
}

#' Compare two recode_spec versions (or one version vs. package YAML)
#'
#' @param version1 `version_id` to compare from.
#' @param version2 `version_id` to compare to. `NULL` compares to package YAML.
#' @param alias Stamp alias.
#' @export
diff_recode_spec <- function(version1, version2 = NULL, alias = "pip_inv") {
  spec1 <- pipload::pip_read(
    "recode_spec",
    format  = "qs2",
    alias   = alias,
    version = version1,
    verbose = FALSE
  )

  if (is.null(version2)) {
    spec2  <- load_package_recode_spec()
    label2 <- "package (inst/extdata/recode_spec.yml)"
  } else {
    spec2  <- pipload::pip_read(
      "recode_spec",
      format  = "qs2",
      alias   = alias,
      version = version2,
      verbose = FALSE
    )
    label2 <- version2
  }

  h1 <- digest::digest(spec1, algo = "xxhash64")
  h2 <- digest::digest(spec2, algo = "xxhash64")

  if (identical(h1, h2)) {
    cli::cli_alert_success("No differences between {version1} and {label2}")
  } else {
    cli::cli_alert_warning("Differences detected between {version1} and {label2}")
    cli::cli_inform("\n--- Version: {version1} ---")
    cat(yaml::as.yaml(spec1))
    cli::cli_inform("\n--- Version: {label2} ---")
    cat(yaml::as.yaml(spec2))
  }

  invisible(list(spec1 = spec1, spec2 = spec2, identical = identical(h1, h2)))
}
