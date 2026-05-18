# Tests for manifest_generation.R
# Plan: .cg-docs/plans/2026-03-17-arrow-data-preparation.md (Phase 0D, Step 7)
#
# Test coverage:
#   - discover_parquet_dimensions(): present dims, no dims, unreadable file
#   - build_manifest_entry(): correct structure and types
#   - generate_release_manifest(): valid run, missing files, bad inventory,
#     set_as_current pointer, summary return structure

# ---------------------------------------------------------------------------
# Helpers — build minimal fixture Parquet files using arrow
# ---------------------------------------------------------------------------

#' Build a tiny schema-conformant data.table for tests
#'
#' @param country_code  ISO3 code.
#' @param surveyid_year Survey year.
#' @param welfare_type  "INC" or "CON".
#' @param pip_id        pip_id string.
#' @param dims          Character vector of breakdown dims to include.
#' @param n_rows        Number of rows.
make_fixture_dt <- function(country_code  = "COL",
                            surveyid_year = 2010L,
                            welfare_type  = "INC",
                            pip_id        = "COL_2010_ECH_V01_M_V02_A_INC",
                            dims          = c("gender", "area"),
                            n_rows        = 10L) {
  dt <- data.table::data.table(
    country_code   = country_code,
    surveyid_year  = as.integer(surveyid_year),
    welfare_type   = welfare_type,
    pip_id         = pip_id,
    survey_acronym = "ECH",
    welfare        = seq(0.5, by = 0.1, length.out = n_rows),
    weight         = rep(1.0, n_rows)
  )

  if ("gender" %in% dims) {
    dt[, gender := factor(
      rep(c("male", "female"), length.out = n_rows),
      levels = c("male", "female")
    )]
  }
  if ("area" %in% dims) {
    dt[, area := factor(
      rep(c("urban", "rural"), length.out = n_rows),
      levels = c("urban", "rural")
    )]
  }
  if ("educat4" %in% dims) {
    dt[, educat4 := factor(
      rep(c("Primary (complete or incomplete)", "No education"), length.out = n_rows)
    )]
  }
  if ("educat5" %in% dims) {
    dt[, educat5 := factor(
      rep(c("Primary incomplete", "Secondary complete"), length.out = n_rows)
    )]
  }
  if ("age" %in% dims) {
    dt[, age := as.integer(seq(18L, by = 1L, length.out = n_rows))]
  }

  dt
}

#' Write a fixture data.table as a Parquet file under an Arrow-style partition
#'
#' @param arrow_root     Temp directory acting as the Arrow repository root.
#' @param dt             data.table produced by make_fixture_dt().
#' @param pip_id         pip_id string (used for filename).
#' @param country_code   ISO3 code.
#' @param surveyid_year  Survey year.
#' @param welfare_type   "INC" or "CON".
#' @param version        Combined version string, e.g. `"v01_v02"`.
#'
#' @return Absolute path of the written Parquet file.
write_fixture_parquet <- function(arrow_root, dt,
                                  pip_id        = "COL_2010_ECH_V01_M_V02_A_INC",
                                  country_code  = "COL",
                                  surveyid_year = 2010L,
                                  welfare_type  = "INC",
                                  version       = "v01_v02") {
  partition_dir <- file.path(
    arrow_root,
    paste0("country_code=",  country_code),
    paste0("surveyid_year=", surveyid_year),
    paste0("welfare_type=",  welfare_type),
    paste0("version=",       version)
  )
  dir.create(partition_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(partition_dir, paste0(pip_id, "-0.parquet"))
  arrow::write_parquet(dt, out_file, compression = "snappy")
  out_file
}

#' Build a tiny multi-welfare schema data.table for tests (new Arrow schema)
#'
#' Mirrors the output of prepare_for_arrow() for new-schema surveys:
#' no `welfare` column; instead `welfare_lcu` + `welfare_ppp_<year>_<vm>_<va>`.
#'
#' @param ppp_suffix PPP column suffix, e.g. "2017_01_02".
make_fixture_dt_multi <- function(country_code  = "COL",
                                  surveyid_year = 2010L,
                                  welfare_type  = "INC",
                                  pip_id        = "COL_2010_ECH_V01_M_V02_A_INC",
                                  dims          = c("gender", "area"),
                                  ppp_suffix    = "2017_01_02",
                                  n_rows        = 10L) {
  wppp_col <- paste0("welfare_ppp_", ppp_suffix)
  ppp_vals <- seq(0.5, by = 0.1, length.out = n_rows)
  dt <- data.table::data.table(
    country_code   = country_code,
    surveyid_year  = as.integer(surveyid_year),
    welfare_type   = welfare_type,
    pip_id         = pip_id,
    survey_acronym = "ECH",
    welfare_lcu    = seq(1.0, by = 0.1, length.out = n_rows),
    weight         = rep(1.0, n_rows)
  )
  data.table::set(dt, j = wppp_col, value = ppp_vals)

  if ("gender" %in% dims) {
    dt[, gender := factor(
      rep(c("male", "female"), length.out = n_rows),
      levels = c("male", "female")
    )]
  }
  if ("area" %in% dims) {
    dt[, area := factor(
      rep(c("urban", "rural"), length.out = n_rows),
      levels = c("urban", "rural")
    )]
  }
  dt
}

make_fixture_inventory <- function(surveys = list(
  list(
    survey_id     = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
    pip_id        = "COL_2010_ECH_V01_M_V02_A_INC",
    country_code  = "COL",
    surveyid_year = 2010L,
    welfare_type  = "INC",
    survey_acronym = "ECH",
    vermast       = "V01",
    veralt        = "V02",
    module        = "ALL"
  )
)) {
  data.table::rbindlist(surveys)
}

# ===========================================================================
# .derive_parquet_path()
# ===========================================================================

test_that(".derive_parquet_path includes version= segment in returned path", {
  path <- pipdata:::.derive_parquet_path(
    country_code  = "COL",
    surveyid_year = 2010L,
    welfare_type  = "INC",
    version       = "v01_v02",
    pip_id        = "COL_2010_ECH_V01_M_V02_A_INC"
  )

  expect_match(path, "country_code=COL")
  expect_match(path, "surveyid_year=2010")
  expect_match(path, "welfare_type=INC")
  expect_match(path, "version=v01_v02")
  expect_match(path, "COL_2010_ECH_V01_M_V02_A_INC-0.parquet")
  # Must be 4-level: country_code/surveyid_year/welfare_type/version/filename
  parts <- strsplit(path, "/")[[1L]]
  expect_length(parts, 5L)
})

test_that(".derive_parquet_path uses correct segment order", {
  path  <- pipdata:::.derive_parquet_path(
    country_code  = "BOL",
    surveyid_year = 2020L,
    welfare_type  = "INC",
    version       = "v01_v04",
    pip_id        = "BOL_2020_EH_V01_M_V04_A_INC"
  )
  parts <- strsplit(path, "/")[[1L]]

  expect_match(parts[[1L]], "^country_code=")
  expect_match(parts[[2L]], "^surveyid_year=")
  expect_match(parts[[3L]], "^welfare_type=")
  expect_match(parts[[4L]], "^version=")
  expect_match(parts[[5L]], "\\.parquet$")
})

# ===========================================================================
# discover_parquet_dimensions()
# ===========================================================================

test_that("discover_parquet_dimensions returns correct dims when educat4/5 present", {
  tmp <- withr::local_tempdir()
  dt  <- make_fixture_dt(dims = c("gender", "area", "educat4", "educat5", "age"))
  f   <- write_fixture_parquet(tmp, dt)

  dims <- discover_parquet_dimensions(f)
  expect_equal(sort(dims), sort(c("gender", "area", "educat4", "educat5", "age")))
})

test_that("discover_parquet_dimensions returns subset of dims correctly", {
  tmp <- withr::local_tempdir()
  dt  <- make_fixture_dt(dims = c("gender", "area"))
  f   <- write_fixture_parquet(tmp, dt)

  dims <- discover_parquet_dimensions(f)
  expect_equal(sort(dims), sort(c("gender", "area")))
})

test_that("discover_parquet_dimensions returns character(0) when no dims present", {
  tmp <- withr::local_tempdir()
  dt  <- make_fixture_dt(dims = character(0))
  f   <- write_fixture_parquet(tmp, dt)

  dims <- discover_parquet_dimensions(f)
  expect_equal(dims, character(0))
})

test_that("discover_parquet_dimensions warns and returns NA for unreadable file", {
  expect_warning(
    dims <- discover_parquet_dimensions(tempfile(fileext = ".parquet")),
    regexp = "Cannot read Parquet schema"
  )
  expect_true(is.na(dims))
  expect_length(dims, 1L)
})

test_that("discover_parquet_dimensions input validation requires single string", {
  expect_error(discover_parquet_dimensions(c("a.parquet", "b.parquet")))
  expect_error(discover_parquet_dimensions(42L))
})

# ===========================================================================
# build_manifest_entry()
# ===========================================================================

test_that("build_manifest_entry returns a correctly structured list with 12 fields", {
  entry <- build_manifest_entry(
    country_code   = "COL",
    surveyid_year  = 2010L,
    welfare_type   = "INC",
    survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
    survey_acronym = "ECH",
    version        = "v01_v02",
    module         = "ALL",
    pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
    dimensions     = c("gender", "area"),
    welfare_vars   = c("welfare_lcu", "welfare_ppp_2017_01_02"),
    ppp_sort       = 2017L
  )

  expect_type(entry, "list")
  # Exactly 12 fields
  expect_named(entry, c(
    "pip_id", "survey_id", "country_code", "year", "welfare_type",
    "version", "survey_acronym", "module", "reporting_level", "dimensions",
    "welfare_vars", "ppp_sort"
  ))
  expect_identical(entry$pip_id,          "COL_2010_ECH_V01_M_V02_A_INC")
  expect_identical(entry$survey_id,       "COL_2010_ECH_V01_M_V02_A_GMD_ALL")
  expect_identical(entry$country_code,    "COL")
  expect_identical(entry$year,             2010L)
  expect_identical(entry$welfare_type,    "INC")
  expect_identical(entry$version,         "v01_v02")
  expect_identical(entry$survey_acronym,  "ECH")
  expect_identical(entry$module,          "ALL")
  expect_identical(entry$reporting_level, "national")
  expect_identical(entry$dimensions,      c("gender", "area"))
  expect_identical(entry$welfare_vars,    c("welfare_lcu", "welfare_ppp_2017_01_02"))
  expect_identical(entry$ppp_sort,        2017L)
})

test_that("build_manifest_entry defaults welfare_vars to character(0) and ppp_sort to NA (legacy)", {
  entry <- build_manifest_entry(
    country_code   = "COL",
    surveyid_year  = 2010L,
    welfare_type   = "INC",
    survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
    survey_acronym = "ECH",
    version        = "v01_v02",
    module         = "ALL",
    pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
    dimensions     = character(0)
  )
  expect_identical(entry$welfare_vars, character(0))
  expect_identical(entry$ppp_sort,     NA_integer_)
})

test_that("build_manifest_entry has no file_path, vermast, or veralt fields", {
  entry <- build_manifest_entry(
    country_code   = "COL",
    surveyid_year  = 2010L,
    welfare_type   = "INC",
    survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
    survey_acronym = "ECH",
    version        = "v01_v02",
    module         = "ALL",
    pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
    dimensions     = character(0)
  )
  expect_false("file_path"          %in% names(entry))
  expect_false("vermast"            %in% names(entry))
  expect_false("veralt"             %in% names(entry))
  expect_false("available_dimensions" %in% names(entry))
})

test_that("build_manifest_entry coerces year to integer and accepts empty dimensions", {
  entry <- build_manifest_entry(
    country_code   = "BOL",
    surveyid_year  = "2012",  # character — should be coerced
    welfare_type   = "CON",
    survey_id      = "BOL_2012_EH_V01_M_V02_A_GMD_ALL",
    survey_acronym = "EH",
    version        = "v01_v02",
    module         = "ALL",
    pip_id         = "BOL_2012_EH_V01_M_V02_A_CON",
    dimensions     = character(0)
  )
  expect_identical(entry$year,       2012L)
  expect_identical(entry$version,    "v01_v02")
  expect_identical(entry$dimensions, character(0))
})

# ===========================================================================
# generate_release_manifest()
# ===========================================================================

test_that("generate_release_manifest writes valid JSON with new format and returns summary", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()

  # Write fixture Parquet with multi-welfare columns + 2 dims
  dt  <- make_fixture_dt_multi(dims = c("gender", "area"), ppp_suffix = "2017_01_02")
  write_fixture_parquet(tmp_arrow, dt, version = "v01_v02")

  inv <- make_fixture_inventory()
  out_path <- file.path(tmp_manifest, "manifest_20260206.json")

  result <- generate_release_manifest(
    release           = "20260206",
    arrow_root        = tmp_arrow,
    release_inventory = inv,
    output_path       = out_path
  )

  # File should exist
  expect_true(file.exists(out_path))

  # JSON should parse cleanly with new top-level keys
  manifest <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_identical(manifest$release, "20260206")
  expect_true(!is.null(manifest$generated_at))
  expect_equal(length(manifest$entries), 1L)
  # Old keys must NOT be present
  expect_null(manifest$release_id)
  expect_null(manifest$created_at)
  expect_null(manifest$surveys)
  expect_null(manifest$arrow_root)

  # Verify entry schema (12 fields)
  entry <- manifest$entries[[1L]]
  expect_named(entry, c(
    "pip_id", "survey_id", "country_code", "year", "welfare_type",
    "version", "survey_acronym", "module", "reporting_level", "dimensions",
    "welfare_vars", "ppp_sort"
  ), ignore.order = TRUE)
  expect_identical(entry$pip_id,        "COL_2010_ECH_V01_M_V02_A_INC")
  expect_identical(entry$country_code,  "COL")
  expect_identical(entry$welfare_type,  "INC")
  expect_identical(entry$version,       "v01_v02")
  # No file_path/vermast/veralt in entry
  expect_null(entry$file_path)
  expect_null(entry$vermast)
  expect_null(entry$veralt)
  expect_null(entry$available_dimensions)
  expect_true(all(c("gender", "area") %in% unlist(entry$dimensions)))
  # New fields
  expect_true(all(c("welfare_lcu", "welfare_ppp_2017_01_02") %in% unlist(entry$welfare_vars)))
  expect_identical(entry$ppp_sort, 2017L)

  # Summary data.table has 'dimensions' column (not 'available_dimensions')
  expect_s3_class(result, "data.table")
  expect_true("dimensions" %in% names(result))
  expect_false("available_dimensions" %in% names(result))
  expect_identical(result[pip_id == "COL_2010_ECH_V01_M_V02_A_INC", status], "included")
})

test_that("generate_release_manifest records 'missing' for absent Parquet files", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()
  # No Parquet file written — directory is empty

  inv      <- make_fixture_inventory()
  out_path <- file.path(tmp_manifest, "manifest_20260206.json")

  expect_warning(
    expect_error(
      generate_release_manifest(
        release           = "20260206",
        arrow_root        = tmp_arrow,
        release_inventory = inv,
        output_path       = out_path
      )
    )
  )
})

test_that("generate_release_manifest skips rows with NA pip_id", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()

  dt <- make_fixture_dt(dims = c("gender"))
  write_fixture_parquet(tmp_arrow, dt, version = "v01_v02")

  inv_with_na <- make_fixture_inventory(list(
    list(
      survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
      pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
      country_code   = "COL",
      surveyid_year  = 2010L,
      welfare_type   = "INC",
      survey_acronym = "ECH",
      vermast        = "V01",
      veralt         = "V02",
      module         = "ALL"
    ),
    # Row with NA pip_id — should be silently excluded
    list(
      survey_id      = "PHL_2021_FIES_V01_M_V01_A_GMD_GPWG",
      pip_id         = NA_character_,
      country_code   = "PHL",
      surveyid_year  = 2021L,
      welfare_type   = "CON",
      survey_acronym = "FIES",
      vermast        = "V01",
      veralt         = "V01",
      module         = "GPWG"
    )
  ))

  out_path <- file.path(tmp_manifest, "manifest_20260206.json")
  result   <- generate_release_manifest(
    release           = "20260206",
    arrow_root        = tmp_arrow,
    release_inventory = inv_with_na,
    output_path       = out_path
  )

  # Only the non-NA pip_id row should appear in summary
  expect_equal(nrow(result), 1L)
  expect_identical(result$pip_id, "COL_2010_ECH_V01_M_V02_A_INC")

  manifest <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_equal(length(manifest$entries), 1L)
  expect_identical(manifest$entries[[1L]]$version, "v01_v02")
})

test_that("generate_release_manifest with set_as_current writes pointer file", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()

  dt <- make_fixture_dt(dims = c("gender", "area"))
  write_fixture_parquet(tmp_arrow, dt, version = "v01_v02")

  inv      <- make_fixture_inventory()
  out_path <- file.path(tmp_manifest, "manifest_20260206.json")

  generate_release_manifest(
    release           = "20260206",
    arrow_root        = tmp_arrow,
    release_inventory = inv,
    output_path       = out_path,
    set_as_current    = TRUE
  )

  pointer_path <- file.path(tmp_manifest, "current_release.json")
  expect_true(file.exists(pointer_path))

  pointer <- jsonlite::fromJSON(pointer_path)
  expect_identical(pointer$current_release, "20260206")
  expect_true(!is.null(pointer$updated_at))
})

test_that("generate_release_manifest errors when arrow_root does not exist", {
  inv <- make_fixture_inventory()
  expect_error(
    generate_release_manifest(
      release           = "20260206",
      arrow_root        = "/nonexistent/arrow",
      release_inventory = inv,
      output_path       = tempfile(fileext = ".json")
    ),
    regexp = "does not exist"
  )
})

test_that("generate_release_manifest errors when output directory does not exist", {
  tmp_arrow <- withr::local_tempdir()
  inv       <- make_fixture_inventory()
  expect_error(
    generate_release_manifest(
      release           = "20260206",
      arrow_root        = tmp_arrow,
      release_inventory = inv,
      output_path       = "/nonexistent/dir/manifest.json"
    ),
    regexp = "does not exist"
  )
})

test_that("generate_release_manifest errors when inventory missing required columns", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()

  bad_inv  <- data.table::data.table(survey_id = "COL_2010", pip_id = "X")
  out_path <- file.path(tmp_manifest, "manifest.json")

  expect_error(
    generate_release_manifest(
      release           = "20260206",
      arrow_root        = tmp_arrow,
      release_inventory = bad_inv,
      output_path       = out_path
    ),
    regexp = "missing required column"
  )
})

test_that("generate_release_manifest handles multiple surveys correctly", {
  tmp_arrow    <- withr::local_tempdir()
  tmp_manifest <- withr::local_tempdir()

  surveys <- list(
    list(
      survey_id      = "COL_2010_ECH_V01_M_V02_A_GMD_ALL",
      pip_id         = "COL_2010_ECH_V01_M_V02_A_INC",
      country_code   = "COL",
      surveyid_year  = 2010L,
      welfare_type   = "INC",
      survey_acronym = "ECH",
      vermast        = "V01",
      veralt         = "V02",
      module         = "ALL"
    ),
    list(
      survey_id      = "BOL_2012_EH_V01_M_V02_A_GMD_ALL",
      pip_id         = "BOL_2012_EH_V01_M_V02_A_CON",
      country_code   = "BOL",
      surveyid_year  = 2012L,
      welfare_type   = "CON",
      survey_acronym = "EH",
      vermast        = "V01",
      veralt         = "V02",
      module         = "ALL"
    )
  )
  inv <- make_fixture_inventory(surveys)

  # Write both fixture files with version= partition
  dt_col <- make_fixture_dt(
    country_code = "COL", surveyid_year = 2010L, welfare_type = "INC",
    pip_id = "COL_2010_ECH_V01_M_V02_A_INC", dims = c("gender", "area")
  )
  dt_bol <- make_fixture_dt(
    country_code = "BOL", surveyid_year = 2012L, welfare_type = "CON",
    pip_id = "BOL_2012_EH_V01_M_V02_A_CON", dims = character(0),
    n_rows = 5L
  )
  write_fixture_parquet(tmp_arrow, dt_col,
    pip_id        = "COL_2010_ECH_V01_M_V02_A_INC",
    country_code  = "COL", surveyid_year = 2010L, welfare_type = "INC",
    version       = "v01_v02"
  )
  write_fixture_parquet(tmp_arrow, dt_bol,
    pip_id        = "BOL_2012_EH_V01_M_V02_A_CON",
    country_code  = "BOL", surveyid_year = 2012L, welfare_type = "CON",
    version       = "v01_v02"
  )

  out_path <- file.path(tmp_manifest, "manifest_20260206.json")
  result   <- generate_release_manifest(
    release           = "20260206",
    arrow_root        = tmp_arrow,
    release_inventory = inv,
    output_path       = out_path
  )

  expect_equal(nrow(result), 2L)
  expect_true(all(result$status == "included"))

  manifest <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_equal(length(manifest$entries), 2L)
  expect_identical(manifest$release, "20260206")

  # BOL survey has no dims
  bol_entry <- Filter(function(s) s$country_code == "BOL", manifest$entries)[[1L]]
  expect_equal(length(bol_entry$dimensions), 0L)
  expect_identical(bol_entry$version, "v01_v02")
  expect_null(bol_entry$file_path)

  # COL survey has gender + area
  col_entry <- Filter(function(s) s$country_code == "COL", manifest$entries)[[1L]]
  expect_true(all(c("gender", "area") %in% unlist(col_entry$dimensions)))
  expect_identical(col_entry$version, "v01_v02")
})
