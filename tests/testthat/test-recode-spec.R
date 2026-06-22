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
    sync_recode_spec = function(...) list(spec = spec, version_id = "v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("gender"  %in% names(result))
  expect_false("male"   %in% names(result))
  expect_equal(result$gender, c("male", "female"))
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
    sync_recode_spec = function(...) list(spec = spec, version_id = "v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_true("age"       %in% names(result))
  expect_true("age_group" %in% names(result))
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
    sync_recode_spec = function(...) list(spec = spec, version_id = "v1")
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
    sync_recode_spec = function(...) list(spec = spec, version_id = "test_v1")
  )
  result <- apply_recode_spec(dt, verbose = FALSE)
  expect_equal(attr(result, "recode_spec_version_id"), "test_v1")
})
