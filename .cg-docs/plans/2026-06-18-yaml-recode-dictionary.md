---
created: "2026-06-18"
updated: "2026-07-22"
status: "in-progress"
priority: "P2"
tags: ["data-quality", "refactoring", "recode", "validation", "stamp", "yaml"]
---

# Stamp-Based Variable Recode Dictionary with YAML Editing

## Problem

The current variable recoding logic in `pd_dlw_clean.R` is hardcoded across four functions (`recode_edu()`, `recode_gndr()`, `recode_age()`, `add_area()`). Each function contains repeated patterns:
- Column existence checks (`if ("varname" %in% colnames(dt))`)
- Type coercion (`as.double()`, `as.character()`)
- Conditional remapping (`fcase()`, `haven::as_factor()`)

Adding new variables or modifying recode rules requires:
1. Editing R source code
2. Updating function logic
3. Rebuilding the package
4. Writing new tests

This tight coupling makes the pipeline inflexible and hard to maintain.

## Proposed Solution

### Architecture

Replace hardcoded recode functions with a **declarative YAML specification**:

```
Source of truth: inst/extdata/recode_spec.yml (in package, git-tracked — already created)
Versioned history: pip_inv alias (stamp, auto-synced on pipeline run)
```

Key design decisions:
- **Package YAML is source of truth**: Edit `inst/extdata/recode_spec.yml` directly in VS Code or RStudio — all changes are tracked in git
- **No dedicated editor function**: Modify the YAML file directly; commit to git as usual
- **Auto-sync on pipeline run**: `sync_recode_spec()` compares package YAML to latest stamp version; if different (or no stamp version exists), saves a new version
- **Fast loading**: Stamp stores R list (qs2 format) for instant pipeline loading
- **Generic dispatcher**: Single `apply_recode_spec()` replaces `add_area()`, `recode_edu()`, `recode_gndr()`, and `recode_age()` in `dlw_clean.pipmd()`
- **Replace source columns for in-place recodes**: For `range_clamp`, `binary_map`, and `haven_labels` types, if `source_column` differs from `var_name`, the source column is **renamed** to the target — `urban` is dropped, `area` is kept; `male` is dropped, `gender` is kept
- **Preserve source columns for derived recodes**: For `binned_from_continuous` and `quantile_from_continuous`, the source column is preserved and a new target column is added
- **Five typed handlers**: `recode_range()`, `recode_binary()`, `recode_haven()`, `recode_binned()`, `recode_quantile()`; all use `data.table::set()` (no tidy-eval operators)
- **Weighted quantiles**: `recode_quantile()` accepts an optional `weight_col` field from the YAML; when present it uses a weighted CDF to compute break points (the `weight` column, already formatted by `format_wgt()`, is the expected source)
- **Structural column modifications as separate functions**: `shift_subnatid()` normalises `subnatid` → `subnatid1` and shifts any existing numbered subnatid columns; called explicitly in `dlw_clean.pipmd()` before `apply_recode_spec()` — structural renames that are not variable-level recodes live here, not in the spec
- **Strict validation**: `validate_recode_spec()` rejects unknown recode types and enforces type-specific constraints (e.g., `binary_map` must have exactly 2 mapping entries)
- **Inventory tracking**: `build_pip_inventory()` adds `version_id_recode_spec` via catalog query (Option B)

### Specification Schema

The full specification lives in `inst/extdata/recode_spec.yml` (already created and committed). Five recode types are supported:

| recode_type | Required fields | Source column behavior |
|---|---|---|
| `range_clamp` | `valid_range: [min, max]` | Modified in-place; renamed if `source_column` differs |
| `binary_map` | `mapping` (exactly 2 entries) | Modified in-place; renamed if `source_column` differs |
| `haven_labels` | `mapping` (N entries) | Explicit label lookup using YAML mapping; renamed if `source_column` differs |
| `binned_from_continuous` | `source_column`, `bin_rules`, `mapping` | New column added; source column preserved |
| `quantile_from_continuous` | `source_column`, `mapping`; optional `weight_col` | New column added; source column preserved |

For `binned_from_continuous`, `bin_rules` is a list of `{bin: int, condition: "expr"}` entries where `condition` is a character string evaluated in the data.table context (column names must match as they appear in `dt` at the time `apply_recode_spec()` runs).

### Implementation

#### Phase 1: Core Infrastructure

**1. Package YAML loaders**

```r
#' @keywords internal
load_package_recode_spec <- function() {
  spec_path <- system.file("extdata", "recode_spec.yml", package = "pipdata")
  if (!file.exists(spec_path)) {
    cli::cli_abort(
      c("recode_spec.yml not found in inst/extdata/",
        "i" = "Expected path: {.path {spec_path}}"),
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
      id = "recode_spec",
      format = "qs2",
      alias = alias,
      verbose = verbose
    ),
    error = function(e) NULL
  )
}
```

**2. Validation**

```r
.known_recode_types <- c(
  "range_clamp",
  "binary_map",
  "haven_labels",
  "binned_from_continuous",
  "quantile_from_continuous"
)

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
        c("Variable {.field {var_name}} has unknown recode_type {.val {rule$recode_type}}",
          "i" = "Known types: {.val {.known_recode_types}}"),
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
            c("Variable {.field {var_name}} (binary_map) must have exactly 2 mapping entries",
              "x" = "Found {length(rule$mapping)}: {.val {names(rule$mapping)}}"),
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
```

**3. Auto-sync function**

Uses `stamp::st_catalog_query()` to retrieve the current version_id — no hardcoded filename assumptions.

```r
#' Sync recode spec from package to stamp
#'
#' Compares package YAML to latest stamp version. If different (or no stamp
#' version exists), saves new version to stamp. Returns the spec and version_id.
#'
#' @param alias Stamp alias. Default: "pip_inv".
#' @param verbose Logical; show sync messages?
#' @return List with: spec (the recode spec to use), version_id (stamp version)
#' @keywords internal
sync_recode_spec <- function(alias = "pip_inv", verbose = TRUE) {
  pkg_spec <- load_package_recode_spec()
  stamp_spec <- load_stamp_recode_spec(alias = alias, verbose = FALSE)

  if (is.null(stamp_spec)) {
    if (verbose) cli::cli_alert_info("No recode_spec in stamp. Saving first version...")

    ventry <- pipload::pip_write(
      pkg_spec,
      id = "recode_spec",
      format = "qs2",
      alias = alias,
      verbose = verbose
    )

    if (verbose) cli::cli_alert_success("Created recode_spec (version: {ventry$version_id})")
    return(list(spec = pkg_spec, version_id = ventry$version_id))
  }

  pkg_hash   <- digest::digest(pkg_spec,   algo = "xxhash64")
  stamp_hash <- digest::digest(stamp_spec, algo = "xxhash64")

  if (identical(pkg_hash, stamp_hash)) {
    # Unchanged: retrieve version_id via catalog query
    cat <- stamp::st_catalog_query(alias = alias)
    recode_row <- cat[grepl("recode_spec", cat$path, fixed = TRUE), ]
    version_id <- if (nrow(recode_row) > 0L) recode_row$version_id[[1L]] else NA_character_

    if (verbose) cli::cli_alert_info("recode_spec unchanged (version: {version_id})")
    return(list(spec = stamp_spec, version_id = version_id))

  } else {
    if (verbose) cli::cli_alert_warning("recode_spec changed. Saving new version...")

    ventry <- pipload::pip_write(
      pkg_spec,
      id = "recode_spec",
      format = "qs2",
      alias = alias,
      verbose = verbose
    )

    if (verbose) cli::cli_alert_success("Saved recode_spec (version: {ventry$version_id})")
    return(list(spec = pkg_spec, version_id = ventry$version_id))
  }
}
```

**4. Typed recode handlers**

All handlers modify `dt` by reference via `data.table::set()`. No tidy-eval (`!!`, `get()`) is used.

```r
#' Recode numeric variable with range clamping
#' @keywords internal
recode_range <- function(dt, var_name, valid_range) {
  if (!var_name %in% names(dt)) return(invisible(dt))

  min_val <- valid_range[[1L]]
  max_val <- valid_range[[2L]]

  data.table::set(dt, j = var_name, value = as.double(dt[[var_name]]))

  x <- dt[[var_name]]
  data.table::set(dt, j = var_name,
    value = data.table::fcase(
      x < min_val,                  NA_real_,
      x >= min_val & x <= max_val,  x,
      x > max_val,                  NA_real_,
      default = NA_real_
    )
  )
  invisible(dt)
}

#' Recode binary variable to character labels
#' mapping must have exactly 2 entries (enforced by validate_recode_spec)
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

#' Recode integer-coded variable to character labels using YAML mapping
#' Uses explicit YAML mapping (not haven::as_factor) — behavior is spec-driven
#' and does not depend on haven labels being embedded in the data.
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

#' Create a new binned-category variable from a continuous source
#'
#' Conditions in bin_rules must reference the source column by its name as it
#' appears in dt at call time. Source column is preserved; var_name is added.
#' Conditions are evaluated in the data.table context via eval(parse(...)).
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

#' Create a new quantile-group variable from a continuous source
#'
#' Uses wbpip::md_compute_quantiles() for group assignments. When weight_col
#' is supplied (e.g. "weight" after format_wgt()), the quantile boundaries
#' are survey-weight-adjusted. Source column is preserved; var_name is added.
#'
#' wbpip::md_compute_quantiles() returns a vector of n_groups values (welfare
#' at the top of each group). The last value is dropped and Inf is used as
#' the upper bound so cut() assigns all values to a valid group.
#' @keywords internal
recode_quantile <- function(dt, var_name, source_col, mapping, weight_col = NULL) {
  if (!source_col %in% names(dt)) return(invisible(dt))

  n_groups <- length(mapping)
  keys <- as.integer(names(mapping))
  vals <- as.character(unlist(mapping, use.names = FALSE))

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

  # q_upper[n_groups] is the max value; replace with Inf for cut()
  breaks <- c(-Inf, q_upper[-n_groups], Inf)
  codes  <- as.integer(cut(x, breaks = unique(breaks),
                           labels = FALSE, include.lowest = TRUE))
  data.table::set(dt, j = var_name,
    value = vals[match(codes, keys)]
  )
  invisible(dt)
}
```

**5. Structural column modifier — `shift_subnatid()`**

Handles the `subnatid` → `subnatid1` normalisation that was previously embedded in
`add_area.pipmd()`. Called explicitly in `dlw_clean.pipmd()` before `apply_recode_spec()`.

```r
#' Normalise subnatid column hierarchy
#'
#' Shifts existing subnatidN columns up by one (subnatid1 -> subnatid2, etc.)
#' then renames subnatid -> subnatid1. No-op if no plain subnatid column exists.
#'
#' @param dt data.table
#' @return dt (modified by reference via setnames)
#' @keywords internal
shift_subnatid <- function(dt) {
  if (!"subnatid" %in% colnames(dt)) return(invisible(dt))

  subnatid_cols <- grep("^subnatid[0-9]+$", colnames(dt), value = TRUE)

  if (length(subnatid_cols) > 0L) {
    nums    <- as.integer(gsub("subnatid", "", subnatid_cols))
    max_num <- max(nums)
    # Rename from largest to smallest to avoid conflicts
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
```

**6. Generic dispatcher**

```r
#' Apply recode specification to data.table
#'
#' Syncs package YAML to stamp (if changed), then applies all matching rules.
#'
#' For range_clamp, binary_map, haven_labels: if source_column differs from
#' var_name, the source column is renamed to var_name (dropping the source).
#' Example: urban -> area, male -> gender.
#'
#' For binned_from_continuous, quantile_from_continuous: source column is
#' preserved and var_name is added as a new column.
#' Example: age stays, age_group is added.
#'
#' @param dt data.table with DLW survey data
#' @param alias Stamp alias. Default: "pip_inv".
#' @param verbose Logical. Default: TRUE.
#' @return data.table with attr "recode_spec_version_id"
#' @export
apply_recode_spec <- function(dt, alias = "pip_inv", verbose = TRUE) {
  sync_result <- sync_recode_spec(alias = alias, verbose = verbose)
  spec        <- sync_result$spec$variables
  version_id  <- sync_result$version_id

  # Types that rename source -> target vs. types that add a new column
  .replace_types <- c("range_clamp", "binary_map", "haven_labels")
  .derive_types  <- c("binned_from_continuous", "quantile_from_continuous")

  recoded_vars <- character(0L)

  for (var_name in names(spec)) {
    rule       <- spec[[var_name]]
    actual_col <- rule$source_column %||% var_name

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
      # Unknown types already rejected by validate_recode_spec at load time
    )

    # For replace-type recodes: rename source_column to var_name
    # This drops the source column (e.g. urban -> area, male -> gender)
    if (rule$recode_type %in% .replace_types &&
        !is.null(rule$source_column) &&
        rule$source_column != var_name &&
        rule$source_column %in% names(dt)) {
      data.table::setnames(dt, old = rule$source_column, new = var_name)
    }

    recoded_vars <- c(recoded_vars, var_name)
  }

  if (verbose && length(recoded_vars) > 0L) {
    pipfun::log_info(
      sprintf("Recoded %d variables: %s (spec version: %s)",
              length(recoded_vars),
              paste(recoded_vars, collapse = ", "),
              version_id),
      name = "pipdata_log"
    )
  }

  data.table::setattr(dt, "recode_spec_version_id", version_id)
  dt
}
```

**7. Modify `dlw_clean.pipmd()`**

Replace `add_area()`, `recode_edu()`, `recode_gndr()`, and `recode_age()` with
`shift_subnatid()` + `apply_recode_spec()`. `format_wgt()` must run before
`apply_recode_spec()` so that the `weight` column is available for `recode_quantile()`.

```r
dlw_clean.pipmd <- function(df, ...) {
  md <- copy(df)

  md <- shift_subnatid(md)  # normalise subnatid columns (structural rename)
  md <- format_wgt(md)      # weight column must exist before apply_recode_spec
  md <- format_wlf(md)

  # Replaces add_area(), recode_edu(), recode_gndr(), recode_age()
  # Spec lives in inst/extdata/recode_spec.yml; auto-synced to stamp on change
  md <- apply_recode_spec(md, verbose = getOption("pipdata.verbose", TRUE))

  md <- wbpip_clean(md)
  md <- pip_vars(md)

  return(md)
}
```

**`add_area.pipgd()` is not affected**: `dlw_clean.pipgd()` continues to call `add_area()`.

#### Phase 2: Migration & Testing

**1. YAML spec**

`inst/extdata/recode_spec.yml` already exists with the full variable set — no creation step needed.

To modify recode rules:

```bash
# 1. Edit directly
#    inst/extdata/recode_spec.yml

# 2. Commit (git-tracked)
git add inst/extdata/recode_spec.yml
git commit -m "feat(recode): add new variable rule"

# 3. Run pipeline — auto-sync detects the change automatically
#    i recode_spec changed. Saving new version...
#    v Saved recode_spec (version: abc123)
#    i Recoded 25 variables: educy, age, ... (spec version: abc123)

# 4. Inspect history
git log -p inst/extdata/recode_spec.yml
```

**2. Inventory integration (Option B — catalog query)**

In `build_pip_inventory()`, after assembling `new_versions`, query the `pip_inv` catalog
to get the current `recode_spec` version and add it as a column:

```r
# Query pip_inv catalog for recode_spec version
recode_cat <- stamp::st_catalog_query(alias = "pip_inv")
recode_row <- recode_cat[grepl("recode_spec", recode_cat$path, fixed = TRUE), ]

recode_spec_version <- if (nrow(recode_row) > 0L) {
  recode_row$version_id[[1L]]
} else {
  NA_character_
}

new_versions[, version_id_recode_spec := recode_spec_version]
```

Add `"version_id_recode_spec"` to the `ordered_cols` vector in `build_pip_inventory()` (after the release version columns).

**3. Utility functions for inspecting stamp history**

```r
#' Export current recode spec from stamp to YAML (or print to console)
#' @param path Output file path. NULL prints to console.
#' @param version Stamp version_id. NULL uses latest.
#' @param alias Stamp alias.
#' @export
export_recode_spec_yaml <- function(path = NULL, version = NULL, alias = "pip_inv") {
  spec <- pipload::pip_read("recode_spec", format = "qs2", alias = alias,
                             version = version, verbose = FALSE)
  if (is.null(path)) {
    cat(yaml::as.yaml(spec))
  } else {
    yaml::write_yaml(spec, path)
    cli::cli_alert_success("Exported to: {.path {path}}")
  }
  invisible(spec)
}

#' List recode_spec versions from stamp catalog
#' @param alias Stamp alias.
#' @export
list_recode_spec_versions <- function(alias = "pip_inv") {
  cat <- stamp::st_catalog_query(alias = alias)
  cat[grepl("recode_spec", cat$path, fixed = TRUE), ]
}

#' Compare two recode_spec versions (or one version vs. package YAML)
#' @param version1 version_id to compare from
#' @param version2 version_id to compare to. NULL compares to package YAML.
#' @param alias Stamp alias.
#' @export
diff_recode_spec <- function(version1, version2 = NULL, alias = "pip_inv") {
  spec1 <- pipload::pip_read("recode_spec", format = "qs2", alias = alias,
                              version = version1, verbose = FALSE)

  if (is.null(version2)) {
    spec2  <- load_package_recode_spec()
    label2 <- "package (inst/extdata/recode_spec.yml)"
  } else {
    spec2  <- pipload::pip_read("recode_spec", format = "qs2", alias = alias,
                                 version = version2, verbose = FALSE)
    label2 <- version2
  }

  h1 <- digest::digest(spec1, algo = "xxhash64")
  h2 <- digest::digest(spec2, algo = "xxhash64")

  if (identical(h1, h2)) {
    cli::cli_alert_success("No differences between {version1} and {label2}")
  } else {
    cli::cli_alert_warning("Differences detected between {version1} and {label2}")
    cli::cli_inform("\\n--- Version: {version1} ---")
    cat(yaml::as.yaml(spec1))
    cli::cli_inform("\\n--- Version: {label2} ---")
    cat(yaml::as.yaml(spec2))
  }

  invisible(list(spec1 = spec1, spec2 = spec2, identical = identical(h1, h2)))
}
```

**4. Comprehensive tests**

Uses `local_mocked_bindings()` (testthat 3.x). Internal package functions are mocked
without `.package`; external package functions use `.package = "pkg"`.

```r
# tests/testthat/test-recode-spec.R

test_that("validate_recode_spec() rejects unknown recode_type", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      foo = list(type = "numeric", recode_type = "unsupported_type")
    )
  )
  expect_error(validate_recode_spec(spec), class = "recode_spec_invalid")
})

test_that("validate_recode_spec() rejects binary_map with != 2 entries", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      x = list(type = "factor", recode_type = "binary_map",
                mapping = list(`1` = "a", `2` = "b", `3` = "c"))
    )
  )
  expect_error(validate_recode_spec(spec), class = "recode_spec_invalid")
})

test_that("validate_recode_spec() catches missing schema_version", {
  spec <- list(variables = list(
    x = list(type = "numeric", recode_type = "range_clamp", valid_range = c(0, 50))
  ))
  expect_error(validate_recode_spec(spec), class = "recode_spec_invalid")
})

test_that("sync_recode_spec() saves new version when stamp is empty", {
  pkg_spec <- list(schema_version = "1.0", variables = list())
  local_mocked_bindings(
    load_package_recode_spec = function() pkg_spec,
    load_stamp_recode_spec   = function(...) NULL
  )
  local_mocked_bindings(
    pip_write = function(...) list(version_id = "new_v1"),
    .package = "pipload"
  )
  result <- sync_recode_spec(verbose = FALSE)
  expect_equal(result$version_id, "new_v1")
})

test_that("sync_recode_spec() reuses stamp version when unchanged", {
  same_spec <- list(schema_version = "1.0", variables = list())
  local_mocked_bindings(
    load_package_recode_spec = function() same_spec,
    load_stamp_recode_spec   = function(...) same_spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path = "recode_spec.qs2", version_id = "existing_v1"
    ),
    .package = "stamp"
  )
  result <- sync_recode_spec(verbose = FALSE)
  expect_equal(result$version_id, "existing_v1")
})

test_that("recode_range() clamps values and converts to double", {
  dt <- data.table::data.table(educy = c(-1, 0, 25, 50, 60))
  recode_range(dt, "educy", list(0, 50))
  expect_equal(dt$educy, c(NA, 0, 25, 50, NA))
  expect_type(dt$educy, "double")
})

test_that("recode_binary() maps exactly 2 values; unmapped -> NA", {
  dt <- data.table::data.table(male = c(1L, 0L, NA_integer_, 9L))
  recode_binary(dt, "male", list(`1` = "male", `0` = "female"))
  expect_equal(dt$male, c("male", "female", NA_character_, NA_character_))
})

test_that("recode_haven() uses YAML mapping, not haven labels", {
  dt <- data.table::data.table(lstatus = c(1L, 2L, 3L, 9L))
  mapping <- list(`1` = "Employed", `2` = "Unemployed", `3` = "Not-in-labor force")
  recode_haven(dt, "lstatus", mapping)
  expect_equal(dt$lstatus,
    c("Employed", "Unemployed", "Not-in-labor force", NA_character_))
})

test_that("recode_binned() creates new column and preserves source", {
  dt <- data.table::data.table(age = c(5, 18, 40, 70))
  bin_rules <- list(
    list(bin = 1L, condition = "age >= 0 & age <= 14"),
    list(bin = 2L, condition = "age >= 15 & age <= 24"),
    list(bin = 3L, condition = "age >= 25 & age <= 64"),
    list(bin = 4L, condition = "age >= 65")
  )
  mapping <- list(`1` = "0-14", `2` = "15-24", `3` = "25-64", `4` = "65+")
  recode_binned(dt, "age_group", "age", bin_rules, mapping)
  expect_equal(dt$age_group, c("0-14", "15-24", "25-64", "65+"))
  expect_true("age" %in% names(dt))
})

test_that("recode_quantile() creates new column and preserves source", {
  dt <- data.table::data.table(welfare = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  mapping <- list(`1` = "q1", `2` = "q2", `3` = "q3", `4` = "q4", `5` = "q5")
  recode_quantile(dt, "wquintile", "welfare", mapping)
  expect_true("wquintile" %in% names(dt))
  expect_true("welfare" %in% names(dt))
  expect_equal(length(unique(na.omit(dt$wquintile))), 5L)
})

test_that("apply_recode_spec() renames source for replace-type recode", {
  dt <- data.table::data.table(male = c(1L, 0L))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      gender = list(type = "factor", recode_type = "binary_map",
                    source_column = "male",
                    mapping = list(`1` = "male", `0` = "female"))
    )
  )
  local_mocked_bindings(
    sync_recode_spec = function(...) list(spec = spec, version_id = "v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("gender" %in% names(result))
  expect_false("male" %in% names(result))
})

test_that("apply_recode_spec() preserves source for derived recode", {
  dt <- data.table::data.table(age = c(5, 18, 40, 70))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      age_group = list(
        type = "factor", recode_type = "binned_from_continuous",
        source_column = "age",
        bin_rules = list(list(bin = 1L, condition = "age <= 14")),
        mapping    = list(`1` = "child")
      )
    )
  )
  local_mocked_bindings(
    sync_recode_spec = function(...) list(spec = spec, version_id = "v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("age" %in% names(result))
  expect_true("age_group" %in% names(result))
})

test_that("apply_recode_spec() attaches version_id attribute", {
  dt <- data.table::data.table(educy = c(0, 25, 50))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      educy = list(type = "numeric", recode_type = "range_clamp",
                   valid_range = list(0, 50))
    )
  )
  local_mocked_bindings(
    sync_recode_spec = function(...) list(spec = spec, version_id = "test_v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_equal(attr(result, "recode_spec_version_id"), "test_v1")
})
```

**5. Deprecate old functions**

Mark `recode_edu()`, `recode_gndr()`, `recode_age()`, and `add_area.pipmd()` with
`.Deprecated()` or remove them in the same PR as the `apply_recode_spec()` integration.

## Benefits

1. **Zero manual sync**: Pipeline auto-detects changes in package YAML and creates new stamp versions
2. **Single source of truth**: Edit `inst/extdata/recode_spec.yml` — everything else flows from it
3. **Full GitHub tracking**: All changes visible in git history, blame, and PR diffs
4. **Explicit label mapping**: `haven_labels` handler uses YAML mapping directly — no dependency on embedded haven attributes in survey data
5. **Fast loading**: Stamp stores R list (qs2) — instant load at pipeline runtime
6. **Release isolation**: Each release gets its own stamp version; inventory tracks which version was used
7. **Testability**: All handlers are atomic and easily tested in isolation
8. **Extensibility**: New recode types add one handler function + entries in `validate_recode_spec()`
9. **Traceability**: `version_id_recode_spec` in inventory links each survey to its recode spec version

## Risks & Mitigation

| Risk | Mitigation |
|------|------------|
| Spec schema errors crash pipeline | `validate_recode_spec()` validates on load; rejects unknown types and malformed entries |
| `bin_rules` conditions reference wrong column names | Condition strings are evaluated in data.table context; absent columns cause silent NA (logged) |
| `binary_map` with > 2 entries | `validate_recode_spec()` rejects with error |
| Performance regression from stamp I/O | qs2 format is fast; one sync check per pipeline run (not per survey) |
| Package YAML accidentally deleted | Stamp retains history; `export_recode_spec_yaml()` can restore |

## Dependencies

- `yaml` (add to `Imports`) for loading package YAML
- `digest` (add to `Imports`) for comparing spec versions
- `wbpip` (already in `Imports`) for `md_compute_quantiles()` in `recode_quantile()`
- `stamp` (already in `Imports`) for catalog queries
- `pipload` (already in `Imports`) for `pip_read()`, `pip_write()`
- No breaking changes to external API — `dlw_clean()` signature unchanged

## Rollout Plan

1. **Week 1**:
   - Implement core infrastructure: `load_package_recode_spec()`, `load_stamp_recode_spec()`, `validate_recode_spec()`, `sync_recode_spec()`
   - Implement all typed handlers: `recode_range()`, `recode_binary()`, `recode_haven()`, `recode_binned()`, `recode_quantile()`
2. **Week 2**:
   - Implement `apply_recode_spec()` with source-column replace/preserve logic
   - Modify `dlw_clean.pipmd()` — remove `add_area()`, `recode_edu()`, `recode_gndr()`, `recode_age()`; resolve Open Question on `subnatid`
   - Add utility functions: `export_recode_spec_yaml()`, `list_recode_spec_versions()`, `diff_recode_spec()`
3. **Week 3**:
   - Add `version_id_recode_spec` to `build_pip_inventory()`
   - Comprehensive test suite
   - Test round-trip: edit YAML → pipeline run → auto-sync → inventory column populated
4. **Week 4**:
   - Regression testing against current hardcoded behavior
   - [x] Deprecate `recode_edu()`, `recode_gndr()`, `recode_age()`, `add_area.pipmd()` — 2026-07-22: each now calls `.Deprecated("apply_recode_spec", package = "pipdata")` and roxygen docs note the replacement; existing tests updated to `suppressWarnings()` the old behavior checks and new tests assert the deprecation warning fires (`tests/testthat/test-pd_dlw_clean.R`)
   - Document workflow: "how to add/modify a recode rule"

## Success Criteria

- [ ] `apply_recode_spec()` produces identical output to the four replaced functions for all existing survey inputs
- [ ] All five recode types produce correct output with full test coverage
- [ ] Test coverage ≥ 90% for new functions
- [ ] `R CMD check` passes with no new warnings
- [ ] `version_id_recode_spec` appears in inventory for all surveys
- [ ] `git log inst/extdata/recode_spec.yml` shows meaningful change history
- [ ] Utility functions (`export_recode_spec_yaml`, `diff_recode_spec`) work correctly

## Resolved Design Decisions

1. **`subnatid` renaming**: Extracted to `shift_subnatid()`, called explicitly in
   `dlw_clean.pipmd()` before `apply_recode_spec()`. Structural column renames that are
   not variable-level recodes live in dedicated functions, not in the spec.

2. **`wquintile` weights**: `recode_quantile()` uses `wbpip::md_compute_quantiles()` with
   the `weight` column (formatted by `format_wgt()`) when `weight_col: weight` is specified
   in the YAML. `format_wgt()` is called before `apply_recode_spec()` to guarantee the
   column exists.

## References

- Current implementation: `R/pd_dlw_clean.R` — `recode_edu()`, `recode_gndr()`, `recode_age()`, `add_area.pipmd()` (lines 226–467)
- Recode specification: `inst/extdata/recode_spec.yml` (already created)
- Inventory integration: `R/build_pip_inventory.R`
- Stamp catalog query pattern: `stamp::st_catalog_query()` — see usage in `R/build_pip_inventory.R:144`
- Stamp storage: `pipload::pip_read()`, `pipload::pip_write()`
