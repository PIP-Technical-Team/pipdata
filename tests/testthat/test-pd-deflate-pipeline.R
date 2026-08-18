# Tests for R/pd_deflate_pipeline.R
#
# Covers:
#   deflate_one()            — happy path, NA-return guard, save-failure,
#                              piperr/error paths, named-list save
#   pd_deflate_pipeline()    — empty inventory, missing deflated column,
#                              all-already-deflated, single success, partial
#                              failure, force flag, caller-supplied inventory,
#                              aux-hash snapshots, master save pk

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

make_inv <- function(
  pip_ids = c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"),
  deflated = rep(NA, length(pip_ids)),
  with_deflated = TRUE,
  with_deflation_cols = TRUE
) {
  dt <- data.table::data.table(
    survey_id = paste0("S", seq_along(pip_ids)),
    pip_id = pip_ids
  )
  if (with_deflated) {
    dt[, deflated := deflated]
  }
  if (with_deflation_cols) {
    dt[, `:=`(
      content_hash_deflated = rep(NA_character_, length(pip_ids)),
      aux_cpi_hash_at_deflation = rep(NA_character_, length(pip_ids)),
      aux_ppp_hash_at_deflation = rep(NA_character_, length(pip_ids)),
      aux_pop_hash_at_deflation = rep(NA_character_, length(pip_ids))
    )]
  }
  dt
}

def_dt <- function() {
  data.table::data.table(welfare_lcu = c(5, 10), weight = c(100, 200))
}

# Minimal stamp catalog row as returned by stamp::st_catalog_query()
make_catalog <- function(pip_ids, hashes = paste0("hash_", seq_along(pip_ids))) {
  data.table::data.table(
    version_id = paste0("v", seq_along(pip_ids)),
    content_hash = hashes,
    code_hash = rep(NA_character_, length(pip_ids)),
    path = sprintf(
      "pip_repository/pip_deflated/%s.qs2",
      pip_ids
    ),
    size_bytes = rep(100, length(pip_ids)),
    created_at = rep(Sys.time(), length(pip_ids))
  )
}

# ---------------------------------------------------------------------------
# deflate_one() — worker
# ---------------------------------------------------------------------------

test_that("deflate_one returns success when deflation and save succeed", {
  inv_row <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")

  # Assert the save receives a named list whose names equal pip_id
  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(data, alias, verbose = TRUE) {
      expect_identical(alias, "pip_deflated")
      expect_identical(names(data), "ABC_2015_TST_INC_D1")
      stats::setNames(list(list(pip_id = "ABC_2015_TST_INC_D1", success = TRUE)), "ABC_2015_TST_INC_D1")
    },
    .package = "pipdata"
  )

  res <- pipdata:::deflate_one(inv_row, verbose = TRUE)

  expect_identical(res, list(pip_id = "ABC_2015_TST_INC_D1", success = TRUE))
})

test_that("deflate_one treats NA return as a failure and does not save", {
  inv_row <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")
  save_called <- FALSE
  logged <- list()

  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose = FALSE) NA,
    save_pip_data = function(...) {
      save_called <<- TRUE
      NULL
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    log_add = function(event, message, name, logmeta) {
      logged <<- c(logged, list(list(event = event, logmeta = logmeta)))
      invisible(NULL)
    },
    .package = "pipfun"
  )

  res <- suppressMessages(pipdata:::deflate_one(inv_row, verbose = TRUE))

  expect_null(res)
  expect_false(save_called)
  expect_length(logged, 1L)
  expect_identical(logged[[1L]]$logmeta$error, "deflation_na")
  expect_identical(logged[[1L]]$logmeta$survey, "ABC_2015_TST_INC_D1")
})

test_that("deflate_one treats a failed save as a failure", {
  inv_row <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")
  logged <- list()

  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(...) NULL, # save failed
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    log_add = function(event, message, name, logmeta) {
      logged <<- c(logged, list(list(event = event, logmeta = logmeta)))
      invisible(NULL)
    },
    .package = "pipfun"
  )

  res <- suppressMessages(pipdata:::deflate_one(inv_row, verbose = TRUE))

  expect_null(res)
  expect_length(logged, 1L)
  expect_identical(logged[[1L]]$logmeta$error, "deflate_save_error")
})

test_that("deflate_one logs a piperr condition and returns NULL", {
  inv_row <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")
  logged <- list()
  cnd <- structure(
    list(message = "boom", call = NULL),
    class = c("test_piperr", "piperr", "error", "condition")
  )

  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose = FALSE) stop(cnd),
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    log_add = function(event, message, name, logmeta) {
      logged <<- c(logged, list(list(event = event, logmeta = logmeta)))
      invisible(NULL)
    },
    .package = "pipfun"
  )

  res <- suppressMessages(pipdata:::deflate_one(inv_row, verbose = TRUE))

  expect_null(res)
  expect_length(logged, 1L)
  expect_identical(logged[[1L]]$logmeta$error, "test_piperr")
  expect_identical(logged[[1L]]$logmeta$survey, "ABC_2015_TST_INC_D1")
})

test_that("deflate_one logs a generic error condition and returns NULL", {
  inv_row <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")
  logged <- list()

  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose = FALSE) stop("generic boom"),
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    log_add = function(event, message, name, logmeta) {
      logged <<- c(logged, list(list(event = event, logmeta = logmeta)))
      invisible(NULL)
    },
    .package = "pipfun"
  )

  res <- suppressMessages(pipdata:::deflate_one(inv_row, verbose = TRUE))

  expect_null(res)
  expect_length(logged, 1L)
  expect_identical(logged[[1L]]$logmeta$error, "unknown_error")
})

# ---------------------------------------------------------------------------
# pd_deflate_pipeline()
# ---------------------------------------------------------------------------

test_that("pd_deflate_pipeline returns an empty inventory unchanged", {
  inv <- make_inv(pip_ids = character(0))

  res <- pd_deflate_pipeline(inv = inv, verbose = TRUE)

  expect_true(data.table::is.data.table(res))
  expect_equal(nrow(res), 0L)
})

test_that("pd_deflate_pipeline normalizes a missing deflated column and deflates", {
  inv <- make_inv(
    pip_ids = "ABC_2015_TST_INC_D1",
    with_deflated = FALSE,
    with_deflation_cols = FALSE
  )
  fake_cat <- make_catalog("ABC_2015_TST_INC_D1", hashes = "h1")

  testthat::local_mocked_bindings(
    get_aux_hashes = function(aux_measures, verbose = TRUE) {
      c(cpi = "ch", ppp = "ph", pop = "oh")
    },
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(data, alias, verbose = TRUE) {
      stats::setNames(
        list(list(pip_id = names(data), success = TRUE)),
        names(data)
      )
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias) fake_cat,
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, pk, verbose = TRUE) NULL,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(...) invisible(NULL),
    .package = "pipfun"
  )

  res <- suppressMessages(pd_deflate_pipeline(inv = inv, verbose = TRUE))

  expect_true("deflated" %in% names(res))
  expect_true(isTRUE(res$deflated[[1L]]))
  expect_identical(res$content_hash_deflated[[1L]], "h1")
  expect_identical(res$aux_cpi_hash_at_deflation[[1L]], "ch")
  expect_identical(res$aux_ppp_hash_at_deflation[[1L]], "ph")
  expect_identical(res$aux_pop_hash_at_deflation[[1L]], "oh")
})

test_that("pd_deflate_pipeline returns early when everything is already deflated", {
  inv <- make_inv(
    pip_ids = c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"),
    deflated = c(TRUE, TRUE)
  )
  before <- copy(inv)

  testthat::local_mocked_bindings(
    get_aux_hashes = function(...) stop("should not be called"),
    .package = "pipdata"
  )

  res <- pd_deflate_pipeline(inv = inv, verbose = TRUE)

  expect_false(is.null(res))
  expect_true(all(res$deflated))
  expect_identical(names(res), names(before))
})

test_that("pd_deflate_pipeline deflates a single survey and updates the inventory", {
  inv <- make_inv("ABC_2015_TST_INC_D1")
  fake_cat <- make_catalog("ABC_2015_TST_INC_D1", hashes = "h1")
  written <- list()

  testthat::local_mocked_bindings(
    get_aux_hashes = function(aux_measures, verbose = TRUE) {
      c(cpi = "cpi_h", ppp = "ppp_h", pop = "pop_h")
    },
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(data, alias, verbose = TRUE) {
      stats::setNames(
        list(list(pip_id = names(data), success = TRUE)),
        names(data)
      )
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias) fake_cat,
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, pk, verbose = TRUE) {
      written <<- list(x = x, id = id, alias = alias, pk = pk)
      invisible(NULL)
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(...) invisible(NULL),
    .package = "pipfun"
  )

  res <- suppressMessages(pd_deflate_pipeline(inv = inv, verbose = TRUE))

  expect_true(isTRUE(res$deflated[[1L]]))
  expect_identical(res$content_hash_deflated[[1L]], "h1")
  expect_identical(res$aux_cpi_hash_at_deflation[[1L]], "cpi_h")
  expect_identical(res$aux_ppp_hash_at_deflation[[1L]], "ppp_h")
  expect_identical(res$aux_pop_hash_at_deflation[[1L]], "pop_h")

  # V7: master write uses pk = c("survey_id", "pip_id")
  expect_identical(written$id, "pip_master_inventory")
  expect_identical(written$alias, "pip_master")
  expect_identical(written$pk, c("survey_id", "pip_id"))
})

test_that("pd_deflate_pipeline handles partial failure and logs n_failed = 1", {
  inv <- make_inv(c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1", "BOL_2015_TST_INC_D1"))
  fake_cat <- make_catalog(
    c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"),
    hashes = c("h1", "h2")
  )
  summary_log <- list()

  testthat::local_mocked_bindings(
    get_aux_hashes = function(aux_measures, verbose = TRUE) {
      c(cpi = "c", ppp = "p", pop = "o")
    },
    pd_deflation = function(pip_id, verbose = FALSE) {
      if (identical(pip_id, "BOL_2015_TST_INC_D1")) {
        return(NA) # failure on BOL
      }
      def_dt()
    },
    save_pip_data = function(data, alias, verbose = TRUE) {
      stats::setNames(
        list(list(pip_id = names(data), success = TRUE)),
        names(data)
      )
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias) fake_cat,
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, pk, verbose = TRUE) invisible(NULL),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(event, message, name, args = NULL, logmeta) {
      summary_log <<- logmeta
      invisible(NULL)
    },
    .package = "pipfun"
  )

  res <- suppressMessages(pd_deflate_pipeline(inv = inv, verbose = TRUE))

  expect_identical(res$deflated, c(TRUE, TRUE, NA))
  expect_identical(res$content_hash_deflated[[1L]], "h1")
  expect_identical(res$content_hash_deflated[[2L]], "h2")
  expect_true(is.na(res$content_hash_deflated[[3L]]))

  expect_identical(summary_log$info, "deflate_summary_inf")
  expect_identical(summary_log$n_total, 3L)
  expect_identical(summary_log$n_success, 2L)
  expect_identical(summary_log$n_failed, 1L)
  expect_setequal(summary_log$surveys_success, c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"))
  expect_identical(summary_log$surveys_failed, "BOL_2015_TST_INC_D1")
})

test_that("pd_deflate_pipeline with force = TRUE re-deflates already-deflated surveys", {
  inv <- make_inv(
    pip_ids = c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"),
    deflated = c(TRUE, TRUE)
  )
  fake_cat <- make_catalog(
    c("ABC_2015_TST_INC_D1", "NGA_2015_TST_INC_D1"),
    hashes = c("h1", "h2")
  )
  save_count <- 0L

  testthat::local_mocked_bindings(
    get_aux_hashes = function(aux_measures, verbose = TRUE) {
      c(cpi = "c", ppp = "p", pop = "o")
    },
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(data, alias, verbose = TRUE) {
      save_count <<- save_count + 1L
      stats::setNames(
        list(list(pip_id = names(data), success = TRUE)),
        names(data)
      )
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias) fake_cat,
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, pk, verbose = TRUE) invisible(NULL),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(...) invisible(NULL),
    .package = "pipfun"
  )

  res <- suppressMessages(pd_deflate_pipeline(inv = inv, force = TRUE, verbose = TRUE))

  expect_identical(save_count, 2L)
  expect_true(all(res$deflated))
  expect_identical(res$content_hash_deflated, c("h1", "h2"))
})

test_that("pd_deflate_pipeline uses the caller-supplied inventory without reloading", {
  inv <- make_inv("ABC_2015_TST_INC_D1")
  load_called <- FALSE

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      load_called <<- TRUE
      inv
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    get_aux_hashes = function(aux_measures, verbose = TRUE) {
      c(cpi = "c", ppp = "p", pop = "o")
    },
    pd_deflation = function(pip_id, verbose = FALSE) def_dt(),
    save_pip_data = function(data, alias, verbose = TRUE) {
      stats::setNames(
        list(list(pip_id = names(data), success = TRUE)),
        names(data)
      )
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias) make_catalog("ABC_2015_TST_INC_D1", "h1"),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, pk, verbose = TRUE) invisible(NULL),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(...) invisible(NULL),
    .package = "pipfun"
  )

  res <- suppressMessages(pd_deflate_pipeline(inv = inv, verbose = TRUE))

  expect_false(load_called)
  expect_true(isTRUE(res$deflated[[1L]]))
})
