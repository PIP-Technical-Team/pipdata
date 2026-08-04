# Tests for valid_aux_load.R and valid_dlw_load() → valid_aux_load() verbose chain
#
# Covers:
#   valid_aux_load()   — verbose propagation to pipaux::compare_aux_releases
#                        and pipaux::compare_aux_vintages
#   valid_dlw_load()   — verbose propagated down to valid_aux_load()

# ---------------------------------------------------------------------------
# valid_aux_load() verbose propagation
# ---------------------------------------------------------------------------

test_that("valid_aux_load(verbose=FALSE, compare='release') passes verbose=FALSE to compare_aux_releases", {
  rel_verbose <- logical(0)

  testthat::local_mocked_bindings(
    compare_aux_releases = function(measure, owner, verbose = TRUE, ...) {
      rel_verbose <<- c(rel_verbose, verbose)
      list() # empty: no changes, function returns NULL
    },
    .package = "pipaux"
  )

  result <- valid_aux_load(
    measure = "cpi",
    compare = "release",
    verbose = FALSE
  )

  expect_null(result)
  expect_true(
    length(rel_verbose) > 0 && all(!rel_verbose),
    info = "compare_aux_releases should receive verbose=FALSE"
  )
})

test_that("valid_aux_load(verbose=FALSE, compare='vintage') passes verbose=FALSE to compare_aux_vintages", {
  vint_verbose <- logical(0)

  testthat::local_mocked_bindings(
    compare_aux_vintages = function(measure, verbose = TRUE, ...) {
      vint_verbose <<- c(vint_verbose, verbose)
      list()
    },
    .package = "pipaux"
  )

  result <- valid_aux_load(
    measure = "cpi",
    compare = "vintage",
    verbose = FALSE
  )

  expect_null(result)
  expect_true(
    length(vint_verbose) > 0 && all(!vint_verbose),
    info = "compare_aux_vintages should receive verbose=FALSE"
  )
})

test_that("valid_aux_load(verbose=FALSE, compare='all') passes verbose=FALSE to both pipaux functions", {
  rel_verbose <- logical(0)
  vint_verbose <- logical(0)

  testthat::local_mocked_bindings(
    compare_aux_releases = function(measure, owner, verbose = TRUE, ...) {
      rel_verbose <<- c(rel_verbose, verbose)
      list()
    },
    compare_aux_vintages = function(measure, verbose = TRUE, ...) {
      vint_verbose <<- c(vint_verbose, verbose)
      list()
    },
    .package = "pipaux"
  )

  result <- valid_aux_load(measure = "cpi", compare = "all", verbose = FALSE)

  expect_null(result)
  expect_true(
    length(rel_verbose) > 0 && all(!rel_verbose),
    info = "compare_aux_releases should receive verbose=FALSE"
  )
  expect_true(
    length(vint_verbose) > 0 && all(!vint_verbose),
    info = "compare_aux_vintages should receive verbose=FALSE"
  )
})

# ---------------------------------------------------------------------------
# valid_dlw_load() → valid_aux_load() verbose chain
# ---------------------------------------------------------------------------

test_that("valid_dlw_load(verbose=FALSE) propagates verbose=FALSE to valid_aux_load()", {
  aux_verbose <- logical(0)

  # Minimal inventory — only columns needed by the module-filter and setorder steps.
  fake_inv <- data.table::data.table(
    survey_id = "ABC_2015_TST_INC_D1",
    country_id = "ABC",
    year = 2015L,
    module = "ALL"
  )

  # Mock all three internal functions together so we isolate the
  # verbose-propagation path and avoid any external I/O.
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE, ...) {
      aux_verbose <<- c(aux_verbose, verbose)
      NULL # no aux changes → inv_aux = NULL
    },
    # last_ver_inv requires many inventory columns; return empty table so the
    # function hits the early-return branch (no surveys to process → NULL).
    last_ver_inv = function(inv, ...) data.table::data.table(),
    inv_to_process = function(inv, ...) NULL,
    .package = "pipdata"
  )

  # With no aux changes and no surveys to process (inv_to_process mocked to
  # NULL, last_ver_inv mocked to empty), valid_dlw_load() now aborts
  # (class "piperr") instead of silently returning NULL -- verbose is still
  # propagated to valid_aux_load() before the abort is raised.
  suppressMessages(
    expect_error(
      valid_dlw_load(inv = fake_inv, verbose = FALSE),
      class = "piperr"
    )
  )

  expect_true(
    length(aux_verbose) > 0 && all(!aux_verbose),
    info = "valid_aux_load should receive verbose=FALSE from valid_dlw_load"
  )
})
