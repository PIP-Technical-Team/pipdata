# Tests for pd_process_data.R: force_surveys mutual-exclusivity guard and
# stamp versioning preservation.
#
# Covers:
#   - force = TRUE + force_surveys aborts with class "piperr" (C3)
#   - force_surveys never calls stamp::st_opts() (R3 / C1)
#   - force = TRUE alone still switches to timestamp versioning (R10 regression)

# ---------------------------------------------------------------------------
# force = TRUE is mutually exclusive with force_surveys
# ---------------------------------------------------------------------------

test_that("pd_process_data aborts with piperr when force and force_surveys are both set", {
  # The guard must fire BEFORE the stamp-versioning side effect, so st_opts
  # must never be touched even with force = TRUE.
  st_opts_called <- 0L
  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      st_opts_called <<- st_opts_called + 1L
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )

  expect_error(
    pd_process_data(
      inv = data.table::data.table(survey_id = character(0)),
      force = TRUE,
      force_surveys = "COL_2020_GEIH",
      verbose = FALSE
    ),
    class = "piperr"
  )
  expect_equal(
    st_opts_called,
    0L,
    info = "the mutual-exclusivity guard must fire before the stamp versioning switch"
  )
})

# ---------------------------------------------------------------------------
# P1.1 regression: force_surveys appended after verbose preserves positional
# `verbose` compatibility (4th argument must still bind to verbose).
# ---------------------------------------------------------------------------

test_that("pd_process_data positional verbose call still binds to verbose", {
  # P1.1 regression: force_surveys must be appended AFTER verbose so existing
  # positional callers (inv, aux_measures, force, verbose) keep binding the 4th
  # slot to verbose. Assert the parameter order in the definition.
  fml <- names(formals(pd_process_data))
  expect_true("verbose" %in% fml)
  expect_true("force_surveys" %in% fml)
  expect_true(
    match("force_surveys", fml) > match("verbose", fml),
    info = "force_surveys must be appended after verbose to preserve positional compatibility"
  )
})

# ---------------------------------------------------------------------------
# force_surveys never calls stamp::st_opts()
# ---------------------------------------------------------------------------

test_that("pd_process_data never calls st_opts when only force_surveys is set", {
  st_opts_called <- 0L

  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      st_opts_called <<- st_opts_called + 1L
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )
  # Abort on inventory load so the test never touches the real pipeline.
  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) stop("stub inventory load"),
    .package = "pipload"
  )

  # force = FALSE + force_surveys: the mutual-exclusivity guard passes and the
  # run proceeds to inventory load, which we stub to stop. st_opts must never
  # have been called along the force_surveys path.
  expect_error(
    pd_process_data(
      inv = NULL,
      force = FALSE,
      force_surveys = "COL_2020_GEIH",
      verbose = FALSE
    ),
    "stub inventory load"
  )

  expect_equal(st_opts_called, 0L)
})

# ---------------------------------------------------------------------------
# force = TRUE alone still switches stamp versioning to "timestamp" (R10)
# ---------------------------------------------------------------------------

test_that("pd_process_data force waits for authoritative preflight", {
  versioning_requests <- character(0)

  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      if (!isTRUE(.get)) {
        versioning_requests <<- c(versioning_requests, versioning)
      }
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) stop("stub inventory load"),
    .package = "pipload"
  )

  # Inventory loading and bootstrap validation are read-only preflight work.
  expect_error(
    pd_process_data(
      inv = NULL,
      force = TRUE,
      force_surveys = NULL,
      verbose = FALSE
    ),
    "stub inventory load"
  )

  expect_false(
    "timestamp" %in% versioning_requests,
    info = "force must not mutate versioning before authoritative preflight"
  )
})

test_that("pd_process_data returns authoritative no-op master unchanged", {
  inv <- data.table::data.table(survey_id = "s")
  master <- data.table::data.table(survey_id = "s", pip_id = "p")
  prepared <- FALSE
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(...) {
      prepared <<- TRUE
      list(plan = list(actions = pd_empty_actions()), lease = list())
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )
  out <- pd_process_data(inv, verbose = FALSE)
  expect_true(prepared)
  expect_identical(out, master)
})
