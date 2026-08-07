# Tests for get_aux_hashes() — run-level aux content-hash resolver
#
# Covers:
#   happy path — all six default measures resolve, including PFW
#   subset — only requested measures resolve
#   missing artifact aborts
#   ambiguous (multiple matching) artifact aborts
#   empty catalog aborts
#   catalog query failure aborts
#   missing content_hash aborts

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Returns a catalog data.table matching the schema from st_catalog_query(),
# with one row per measure artifact under the aux alias.
make_aux_catalog <- function(measures = c("cpi", "ppp", "pfw", "pop", "gdp", "pce")) {
  if (length(measures) == 0L) {
    return(data.table::data.table(
      path = character(),
      version_id = character(),
      content_hash = character(),
      code_hash = character(),
      size_bytes = numeric(),
      created_at = character()
    ))
  }
  data.table::data.table(
    path = paste0("/aux/20260401_test/", measures, ".qs2"),
    version_id = paste0("vid_", measures),
    content_hash = paste0("hash_", measures),
    code_hash = paste0("code_", measures),
    size_bytes = rep(1000, length(measures)),
    created_at = rep("2026-04-06T00:00:00", length(measures))
  )
}

# ---------------------------------------------------------------------------
# Happy path: all six default measures resolve, including PFW
# ---------------------------------------------------------------------------

test_that("get_aux_hashes resolves all six default measures including PFW", {
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_aux_catalog(),
    .package = "stamp"
  )

  result <- get_aux_hashes(c("pfw", "cpi", "ppp", "pop", "gdp", "pce"))

  expect_named(result, c("pfw", "cpi", "ppp", "pop", "gdp", "pce"))
  expect_equal(result[["pfw"]], "hash_pfw")
  expect_equal(result[["cpi"]], "hash_cpi")
  expect_equal(result[["ppp"]], "hash_ppp")
  expect_equal(result[["pop"]], "hash_pop")
  expect_equal(result[["gdp"]], "hash_gdp")
  expect_equal(result[["pce"]], "hash_pce")
})

# ---------------------------------------------------------------------------
# Subset: only requested measures resolve
# ---------------------------------------------------------------------------

test_that("get_aux_hashes resolves only the requested measures", {
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_aux_catalog(),
    .package = "stamp"
  )

  result <- get_aux_hashes(c("cpi", "ppp"))

  expect_named(result, c("cpi", "ppp"))
  expect_equal(result[["cpi"]], "hash_cpi")
  expect_equal(result[["ppp"]], "hash_ppp")
})

# ---------------------------------------------------------------------------
# Missing artifact aborts
# ---------------------------------------------------------------------------

test_that("get_aux_hashes aborts when a requested artifact is missing", {
  # Catalog only has cpi and ppp; request pfw too.
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_aux_catalog(c("cpi", "ppp")),
    .package = "stamp"
  )

  expect_error(
    get_aux_hashes(c("cpi", "pfw")),
    class = "get_aux_hashes_missing_artifact"
  )
})

# ---------------------------------------------------------------------------
# Ambiguous artifact aborts
# ---------------------------------------------------------------------------

test_that("get_aux_hashes aborts when multiple catalog rows match a measure", {
  cat <- make_aux_catalog(c("cpi", "ppp"))
  # Add a duplicate cpi artifact at a different path.
  cat <- rbind(
    cat,
    data.table::data.table(
      path = "/aux/20260401_test/other/cpi.qs2",
      version_id = "vid_cpi_dup",
      content_hash = "hash_cpi_dup",
      code_hash = "code_cpi_dup",
      size_bytes = 2000,
      created_at = "2026-04-07T00:00:00"
    )
  )

  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) cat,
    .package = "stamp"
  )

  expect_error(
    get_aux_hashes("cpi"),
    class = "get_aux_hashes_ambiguous_artifact"
  )
})

# ---------------------------------------------------------------------------
# Empty catalog aborts
# ---------------------------------------------------------------------------

test_that("get_aux_hashes aborts when the aux catalog is empty", {
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_aux_catalog(character(0)),
    .package = "stamp"
  )

  expect_error(
    get_aux_hashes("cpi"),
    class = "get_aux_hashes_empty_catalog"
  )
})

# ---------------------------------------------------------------------------
# Catalog query failure aborts
# ---------------------------------------------------------------------------

test_that("get_aux_hashes aborts when the catalog query fails", {
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) stop("catalog corrupt"),
    .package = "stamp"
  )

  expect_error(
    get_aux_hashes("cpi"),
    class = "get_aux_hashes_catalog_failure"
  )
})

# ---------------------------------------------------------------------------
# Missing content_hash aborts
# ---------------------------------------------------------------------------

test_that("get_aux_hashes aborts when a measure has no content_hash", {
  cat <- make_aux_catalog(c("cpi", "ppp"))
  # Set the ppp row's content_hash to NA (row 2).
  cat[2L, content_hash := NA_character_]

  testthat::local_mocked_bindings(
    st_catalog_query = function(alias = NULL) cat,
    .package = "stamp"
  )

  expect_error(
    get_aux_hashes("ppp"),
    class = "get_aux_hashes_missing_hash"
  )
})
