# ── validate_recode_spec ──────────────────────────────────────────────────────

test_that("validate_recode_spec() rejects missing schema_version", {
  spec <- list(variables = list(
    x = list(type = "numeric", recode_type = "range_clamp", valid_range = c(0, 50))
  ))
  expect_error(
    pipdata:::validate_recode_spec(spec),
    class = "recode_spec_invalid"
  )
})

test_that("validate_recode_spec() rejects unknown recode_type", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      foo = list(type = "numeric", recode_type = "unsupported_type")
    )
  )
  expect_error(
    pipdata:::validate_recode_spec(spec),
    class = "recode_spec_invalid"
  )
})

test_that("validate_recode_spec() rejects binary_map with != 2 entries", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      x = list(
        type    = "factor",
        recode_type = "binary_map",
        mapping = list(`1` = "a", `2` = "b", `3` = "c")
      )
    )
  )
  expect_error(
    pipdata:::validate_recode_spec(spec),
    class = "recode_spec_invalid"
  )
})

test_that("validate_recode_spec() rejects range_clamp with missing valid_range", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      x = list(type = "numeric", recode_type = "range_clamp")
    )
  )
  expect_error(
    pipdata:::validate_recode_spec(spec),
    class = "recode_spec_invalid"
  )
})

test_that("validate_recode_spec() returns TRUE for valid spec", {
  spec <- list(
    schema_version = "1.0",
    variables = list(
      age   = list(type = "numeric", recode_type = "range_clamp", valid_range = c(0, 110)),
      urban = list(
        type        = "factor",
        recode_type = "binary_map",
        mapping     = list(`1` = "urban", `0` = "rural")
      )
    )
  )
  expect_true(pipdata:::validate_recode_spec(spec))
})

# ── recode_range ──────────────────────────────────────────────────────────────

test_that("recode_range() clamps values and converts to double", {
  dt <- data.table::data.table(educy = c(-1, 0, 25, 50, 60))
  pipdata:::recode_range(dt, "educy", list(0, 50))
  expect_equal(dt$educy, c(NA_real_, 0, 25, 50, NA_real_))
  expect_type(dt$educy, "double")
})

test_that("recode_range() is a no-op when column is absent", {
  dt <- data.table::data.table(welfare = 100)
  pipdata:::recode_range(dt, "educy", list(0, 50))
  expect_false("educy" %in% names(dt))
})

test_that("recode_range() preserves exact boundary values", {
  dt <- data.table::data.table(x = c(0, 110))
  pipdata:::recode_range(dt, "x", list(0, 110))
  expect_equal(dt$x, c(0, 110))
})

# ── recode_binary ─────────────────────────────────────────────────────────────

test_that("recode_binary() maps exactly 2 values; unmapped -> NA", {
  dt <- data.table::data.table(male = c(1L, 0L, NA_integer_, 9L))
  pipdata:::recode_binary(dt, "male", list(`1` = "male", `0` = "female"))
  expect_equal(dt$male, c("male", "female", NA_character_, NA_character_))
})

test_that("recode_binary() is a no-op when column is absent", {
  dt <- data.table::data.table(welfare = 1)
  pipdata:::recode_binary(dt, "male", list(`1` = "male", `0` = "female"))
  expect_false("male" %in% names(dt))
})

# ── recode_haven ──────────────────────────────────────────────────────────────

test_that("recode_haven() uses YAML mapping, not haven labels", {
  dt <- data.table::data.table(lstatus = c(1L, 2L, 3L, 9L))
  mapping <- list(`1` = "Employed", `2` = "Unemployed", `3` = "Not-in-labor force")
  pipdata:::recode_haven(dt, "lstatus", mapping)
  expect_equal(dt$lstatus,
    c("Employed", "Unemployed", "Not-in-labor force", NA_character_))
})

test_that("recode_haven() is a no-op when column is absent", {
  dt <- data.table::data.table(welfare = 1)
  pipdata:::recode_haven(dt, "lstatus", list(`1` = "Employed"))
  expect_false("lstatus" %in% names(dt))
})

# ── recode_binned ─────────────────────────────────────────────────────────────

test_that("recode_binned() creates new column and preserves source", {
  dt <- data.table::data.table(age = c(5, 18, 40, 70))
  bin_rules <- list(
    list(bin = 1L, condition = "age >= 0 & age <= 14"),
    list(bin = 2L, condition = "age >= 15 & age <= 24"),
    list(bin = 3L, condition = "age >= 25 & age <= 64"),
    list(bin = 4L, condition = "age >= 65")
  )
  mapping <- list(`1` = "0-14", `2` = "15-24", `3` = "25-64", `4` = "65+")
  pipdata:::recode_binned(dt, "age_group", "age", bin_rules, mapping)
  expect_equal(dt$age_group, c("0-14", "15-24", "25-64", "65+"))
  expect_true("age" %in% names(dt))
})

test_that("recode_binned() is a no-op when source column is absent", {
  dt <- data.table::data.table(welfare = 1)
  pipdata:::recode_binned(dt, "age_group", "age",
    list(list(bin = 1L, condition = "age >= 0")),
    list(`1` = "child")
  )
  expect_false("age_group" %in% names(dt))
})

# ── recode_quantile ───────────────────────────────────────────────────────────

test_that("recode_quantile() creates new column and preserves source", {
  dt <- data.table::data.table(welfare = as.double(1:10))
  mapping <- list(`1` = "q1", `2` = "q2", `3` = "q3", `4` = "q4", `5` = "q5")
  pipdata:::recode_quantile(dt, "wquintile", "welfare", mapping)
  expect_true("wquintile" %in% names(dt))
  expect_true("welfare"   %in% names(dt))
  expect_equal(length(unique(na.omit(dt$wquintile))), 5L)
})

test_that("recode_quantile() is a no-op when source column is absent", {
  dt <- data.table::data.table(x = 1)
  pipdata:::recode_quantile(dt, "wquintile", "welfare",
    list(`1` = "q1", `2` = "q2"))
  expect_false("wquintile" %in% names(dt))
})

# ── shift_subnatid ────────────────────────────────────────────────────────────

test_that("shift_subnatid() renames subnatid -> subnatid1 when no numbered cols exist", {
  dt <- data.table::data.table(subnatid = c("a", "b"), x = 1:2)
  pipdata:::shift_subnatid(dt)
  expect_true("subnatid1" %in% names(dt))
  expect_false("subnatid"  %in% names(dt))
})

test_that("shift_subnatid() shifts existing subnatidN columns up by one", {
  dt <- data.table::data.table(subnatid = "a", subnatid1 = "b", subnatid2 = "c")
  pipdata:::shift_subnatid(dt)
  expect_true(all(c("subnatid1", "subnatid2", "subnatid3") %in% names(dt)))
  expect_false("subnatid" %in% names(dt))
})

test_that("shift_subnatid() is a no-op when no plain subnatid column exists", {
  dt <- data.table::data.table(subnatid1 = "a", x = 1)
  nms_before <- names(dt)
  pipdata:::shift_subnatid(dt)
  expect_equal(names(dt), nms_before)
})

# ── sync_recode_spec ──────────────────────────────────────────────────────────

test_that("sync_recode_spec() saves new version when stamp is empty", {
  pkg_spec <- list(schema_version = "1.0", variables = list())
  local_mocked_bindings(
    load_package_recode_spec = function(...) pkg_spec,
    load_stamp_recode_spec   = function(...) NULL
  )
  local_mocked_bindings(
    pip_write = function(...) list(version_id = "new_v1"),
    .package  = "pipload"
  )
  result <- pipdata:::sync_recode_spec(verbose = FALSE)
  expect_equal(result$version_id, "new_v1")
})

test_that("sync_recode_spec() reuses stamp version when unchanged", {
  same_spec <- list(schema_version = "1.0", variables = list())
  local_mocked_bindings(
    load_package_recode_spec = function(...) same_spec,
    load_stamp_recode_spec   = function(...) same_spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path       = "recode_spec.qs2",
      version_id = "existing_v1"
    ),
    .package = "stamp"
  )
  result <- pipdata:::sync_recode_spec(verbose = FALSE)
  expect_equal(result$version_id, "existing_v1")
})

# ── apply_recode_spec ─────────────────────────────────────────────────────────

test_that("apply_recode_spec() renames source for replace-type recode", {
  dt <- data.table::data.table(male = c(1L, 0L))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      gender = list(
        type          = "factor",
        recode_type   = "binary_map",
        source_column = "male",
        mapping       = list(`1` = "male", `0` = "female")
      )
    )
  )
  local_mocked_bindings(
    load_stamp_recode_spec = function(...) spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path = "recode_spec.qs2", version_id = "v1"
    ),
    .package = "stamp"
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("gender"  %in% names(result))
  expect_false("male"   %in% names(result))
  expect_true(is.factor(result$gender))
  expect_equal(as.character(result$gender), c("male", "female"))
  expect_equal(levels(result$gender), c("male", "female"))
})

test_that("apply_recode_spec() preserves source for derive-type recode", {
  dt <- data.table::data.table(age = c(5, 18, 40, 70))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      age_group = list(
        type          = "factor",
        recode_type   = "binned_from_continuous",
        source_column = "age",
        bin_rules     = list(list(bin = 1L, condition = "age <= 14")),
        mapping       = list(`1` = "child")
      )
    )
  )
  local_mocked_bindings(
    load_stamp_recode_spec = function(...) spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path = "recode_spec.qs2", version_id = "v1"
    ),
    .package = "stamp"
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("age"       %in% names(result))
  expect_true("age_group" %in% names(result))
  expect_true(is.factor(result$age_group))
  expect_equal(levels(result$age_group), c("child"))
})

test_that("apply_recode_spec() skips variables absent from dt", {
  dt <- data.table::data.table(welfare = 1.0)
  spec <- list(
    schema_version = "1.0",
    variables = list(
      educy = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 50))
    )
  )
  local_mocked_bindings(
    load_stamp_recode_spec = function(...) spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path = "recode_spec.qs2", version_id = "v1"
    ),
    .package = "stamp"
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_false("educy" %in% names(result))
})

test_that("apply_recode_spec() attaches recode_spec_version_id attribute", {
  dt <- data.table::data.table(educy = c(0, 25, 50))
  spec <- list(
    schema_version = "1.0",
    variables = list(
      educy = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 50))
    )
  )
  local_mocked_bindings(
    load_stamp_recode_spec = function(...) spec
  )
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path = "recode_spec.qs2", version_id = "test_v1"
    ),
    .package = "stamp"
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_equal(attr(result, "recode_spec_version_id"), "test_v1")
})

# ── Equivalence: apply_recode_spec() vs. legacy recode_*()/add_area.pipmd() ───
# Uses the real package YAML (inst/extdata/recode_spec.yml) via the
# recode_spec = list(spec, version_id) fast path, so no stamp/pipload I/O
# mocking is needed for the *new* path.

test_that("apply_recode_spec() matches legacy recode_edu()/recode_age() numeric clamping", {
  pkg_spec <- pipdata:::load_package_recode_spec()

  raw <- data.table::data.table(
    educy = c(-1, 0, 25, 50, 60),
    age   = c(-5, 0, 40, 110, 200)
  )

  legacy <- data.table::copy(raw)
  legacy <- suppressWarnings(pipdata:::recode_edu(legacy))
  legacy <- suppressWarnings(pipdata:::recode_age(legacy))

  new <- data.table::copy(raw)
  new <- apply_recode_spec(
    new,
    verbose     = FALSE,
    recode_spec = list(spec = pkg_spec, version_id = "test_v1")
  )

  expect_equal(new$educy, legacy$educy)
  expect_equal(new$age, legacy$age)
})

test_that("apply_recode_spec() matches legacy recode_gndr() mapping for non-missing values", {
  pkg_spec <- pipdata:::load_package_recode_spec()

  # NA excluded here: add_area.pipmd()/recode_gndr() and apply_recode_spec()
  # diverge on NA handling for the *area* variable specifically (see the
  # dedicated "known NA divergence" test below); male/gender NA handling is
  # identical (both default to NA), but we keep this test focused on the
  # mapped values.
  raw <- data.table::data.table(male = c(1L, 0L, 1L, 0L))

  legacy <- data.table::copy(raw)
  legacy <- suppressWarnings(pipdata:::recode_gndr(legacy))

  new <- data.table::copy(raw)
  new <- apply_recode_spec(
    new,
    verbose     = FALSE,
    recode_spec = list(spec = pkg_spec, version_id = "test_v1")
  )

  # apply_recode_spec() renames male -> gender (replace-type recode) and
  # returns a factor (per `type: factor` in the spec); recode_gndr() keeps
  # `male` and adds `gender` as character. Compare on the mapped values only.
  expect_false("male" %in% names(new))
  expect_equal(as.character(new$gender), legacy$gender)
})

test_that("apply_recode_spec() matches legacy add_area.pipmd() urban -> area mapping for non-missing values", {
  pkg_spec <- pipdata:::load_package_recode_spec()

  raw <- data.table::data.table(urban = c(1L, 0L, 1L, 0L))

  legacy <- data.table::copy(raw)
  class(legacy) <- c("pipmd", class(legacy))
  legacy <- suppressWarnings(pipdata:::add_area.pipmd(legacy))

  new <- data.table::copy(raw)
  new <- apply_recode_spec(
    new,
    verbose     = FALSE,
    recode_spec = list(spec = pkg_spec, version_id = "test_v1")
  )

  expect_false("urban" %in% names(new))
  expect_equal(as.character(new$area), legacy$area)
})

test_that("KNOWN DIVERGENCE: add_area.pipmd() maps NA urban to '' while apply_recode_spec() maps it to NA", {
  # This documents an intentional/inherited behavior difference discovered
  # while verifying output equivalence for the yaml-recode-dictionary plan.
  # add_area.pipmd() special-cases `is.na(urban)` to the empty string "",
  # whereas apply_recode_spec()'s recode_binary() has no such special case
  # and falls through to its `default = NA_character_`. Locked in here so a
  # future change to either path is a deliberate decision, not a silent
  # regression.
  pkg_spec <- pipdata:::load_package_recode_spec()

  raw <- data.table::data.table(urban = NA_integer_)

  legacy <- data.table::copy(raw)
  class(legacy) <- c("pipmd", class(legacy))
  legacy <- suppressWarnings(pipdata:::add_area.pipmd(legacy))

  new <- data.table::copy(raw)
  new <- apply_recode_spec(
    new,
    verbose     = FALSE,
    recode_spec = list(spec = pkg_spec, version_id = "test_v1")
  )

  expect_identical(legacy$area, "")
  expect_true(is.na(new$area))
})

# ── export_recode_spec_yaml ────────────────────────────────────────────────────

test_that("export_recode_spec_yaml() prints YAML to console when path is NULL", {
  spec <- list(schema_version = "1.0", variables = list(
    age = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 110))
  ))
  local_mocked_bindings(
    pip_read = function(...) spec,
    .package = "pipload"
  )

  expect_output(
    result <- export_recode_spec_yaml(),
    "schema_version"
  )
  expect_identical(result, spec)
})

test_that("export_recode_spec_yaml() writes YAML to disk when path is supplied", {
  spec <- list(schema_version = "1.0", variables = list(
    age = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 110))
  ))
  local_mocked_bindings(
    pip_read = function(...) spec,
    .package = "pipload"
  )

  out_path <- withr::local_tempfile(fileext = ".yml")
  result <- export_recode_spec_yaml(path = out_path)

  expect_true(file.exists(out_path))
  expect_equal(yaml::read_yaml(out_path)$schema_version, "1.0")
  expect_identical(result, spec)
})

test_that("export_recode_spec_yaml() passes version through to pip_read()", {
  seen_version <- NULL
  local_mocked_bindings(
    pip_read = function(id, format, alias, version = NULL, verbose = NULL) {
      seen_version <<- version
      list(schema_version = "1.0", variables = list())
    },
    .package = "pipload"
  )

  invisible(export_recode_spec_yaml(version = "v42"))
  expect_equal(seen_version, "v42")
})

# ── list_recode_spec_versions ─────────────────────────────────────────────────

test_that("list_recode_spec_versions() filters the stamp catalog to recode_spec rows", {
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path       = c("recode_spec.qs2", "pfw.qs2", "recode_spec.qs2"),
      version_id = c("v1", "v_other", "v2")
    ),
    .package = "stamp"
  )

  result <- list_recode_spec_versions()

  expect_equal(nrow(result), 2L)
  expect_true(all(grepl("recode_spec", result$path)))
})

test_that("list_recode_spec_versions() returns zero rows when nothing matches", {
  local_mocked_bindings(
    st_catalog_query = function(...) data.table::data.table(
      path       = "pfw.qs2",
      version_id = "v1"
    ),
    .package = "stamp"
  )

  result <- list_recode_spec_versions()

  expect_equal(nrow(result), 0L)
})

# ── diff_recode_spec ───────────────────────────────────────────────────────────

test_that("diff_recode_spec() reports identical = TRUE for matching specs", {
  same_spec <- list(schema_version = "1.0", variables = list(
    age = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 110))
  ))
  local_mocked_bindings(
    pip_read = function(...) same_spec,
    .package = "pipload"
  )
  local_mocked_bindings(
    load_package_recode_spec = function(...) same_spec
  )

  result <- diff_recode_spec(version1 = "v1")

  expect_true(result$identical)
  expect_identical(result$spec1, same_spec)
  expect_identical(result$spec2, same_spec)
})

test_that("diff_recode_spec() reports identical = FALSE for differing specs", {
  spec1 <- list(schema_version = "1.0", variables = list(
    age = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 110))
  ))
  spec2 <- list(schema_version = "1.0", variables = list(
    age = list(type = "numeric", recode_type = "range_clamp", valid_range = list(0, 100))
  ))
  local_mocked_bindings(
    pip_read = function(...) spec1,
    .package = "pipload"
  )
  local_mocked_bindings(
    load_package_recode_spec = function(...) spec2
  )

  result <- diff_recode_spec(version1 = "v1")

  expect_false(result$identical)
})

test_that("diff_recode_spec() compares two stamp versions when version2 is supplied", {
  spec1 <- list(schema_version = "1.0", variables = list())
  spec2 <- list(schema_version = "1.0", variables = list())
  seen_versions <- character(0L)
  local_mocked_bindings(
    pip_read = function(id, format, alias, version = NULL, verbose = NULL) {
      seen_versions <<- c(seen_versions, version)
      if (version == "v1") spec1 else spec2
    },
    .package = "pipload"
  )

  result <- diff_recode_spec(version1 = "v1", version2 = "v2")

  expect_equal(seen_versions, c("v1", "v2"))
  expect_true(result$identical)
})
