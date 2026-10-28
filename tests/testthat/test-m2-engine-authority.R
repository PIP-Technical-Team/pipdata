# Fixtures stay under the runner's explicitly selected E store. No auto helpers.
m2_fixture <- function() {
  base <- file.path(Sys.getenv("PIP_ROOT_DIR"), "..", "phase2")
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  stopifnot(grepl("^E:/", base, ignore.case = TRUE))
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  root <- tempfile("authority-", tmpdir = base)
  stopifnot(dir.create(root))
  root <- normalizePath(root, winslash = "/")
  alias <- paste0("authority-", Sys.getpid(), "-", basename(root))
  stamp::st_init(root, alias = alias, verbose = FALSE)
  stamp::st_opts(versioning = "content", retain_versions = Inf,
    meta_format = "both", warn_missing_pk_on_load = FALSE,
    require_pk_on_load = FALSE)
  list(root = root, alias = alias, context = list(
    release = "m2-fixture", identity = "TEST", root = root))
}

m2_node <- function(path = "a.rds", value = 1L, parents = list()) {
  list(path = path, parents = parents,
    calculation = function(path, parents) list(x = value), helpers = list(),
    constants = list(value = value), semantic_schema = NULL,
    semantic = list(), format = tools::file_ext(path))
}

m2_run <- function(fixture, nodes, ...) {
  pd_run_pipeline(recipes = list(context = fixture$context, nodes = nodes),
                  verbose = FALSE, ...)
}

m2_read_node <- function(fixture) {
  input <- m2_run(fixture, list(m2_node("input.rds")))
  node <- m2_node(parents = list(list(path = input$path[[1L]],
                                     version_id = input$version_id[[1L]])))
  node$calculation <- function(path, parents) list(x = pipload::pip_read(
    parents[[1L]]$path, format = "rds", version = parents[[1L]]$version_id))
  node
}

m2_mock_read <- function(fn) {
  formals(fn) <- formals(pipload::pip_read)
  fn
}

test_that("M2 empty route and incompatible controls never invoke legacy work", {
  f <- m2_fixture()
  testthat::local_mocked_bindings(
    pd_prepare_execution_locked = function(...) stop("legacy selector"),
    pd_snapshot_facts = function(...) stop("legacy facts"),
    pd_dependency_plan = function(...) stop("legacy plan"),
    .package = "pipdata")
  empty <- m2_run(f, list())
  expect_s3_class(empty, "data.table")
  expect_identical(nrow(empty), 0L)
  expect_identical(names(empty), c("release_id", "path", "preview_decision",
    "status", "reason", "version_id", "request_id", "receipt_id", "outcome_id",
    "failure_class", "governing_failure_id", "message", "attempted"))
  before <- list.files(f$root, all.files = TRUE, recursive = TRUE)
  expect_error(m2_run(f, list(m2_node()), force = TRUE), "incremental")
  expect_error(m2_run(f, list(m2_node()), inv = data.frame()), "legacy")
  expect_identical(list.files(f$root, all.files = TRUE, recursive = TRUE), before)
})

test_that("M2 copies declared lexical proof and rejects undeclared or dynamic code", {
  f <- m2_fixture()
  node <- m2_node()
  helper <- function(x) x + offset
  node$calculation <- function(path, parents) list(x = helper(value))
  node$helpers <- list(helper = helper)
  node$constants$offset <- 2L
  first <- m2_run(f, list(node))
  expect_identical(first$status, "built")
  expect_identical(pipload::pip_read(first$path, "rds", first$version_id,
    f$alias, verbose = FALSE), 3L)
  node$constants$offset <- 3L
  expect_identical(m2_run(f, list(node))$status, "built")
  for (primitive in list(sum, max)) {
    node$helpers$helper <- primitive
    expect_error(m2_run(f, list(node)), "ordinary|primitive")
  }
  node <- m2_node()
  node$calculation <- function(path, parents) list(x = undeclared)
  expect_error(m2_run(f, list(node)), "undeclared")
  node$calculation <- function(path, parents) list(x = get("value"))
  expect_error(m2_run(f, list(node)), "Unsupported|dynamic")
  node$calculation <- function(path, parents) { value <<- 8L; list(x = value) }
  expect_error(m2_run(f, list(node)), "Unsupported|assignment")
  node <- m2_node(value = NA_integer_)
  expect_error(m2_run(f, list(node)), "missing|immutable")
})

test_that("M2 real first-build chain and retained-output cutoff belong to stamp", {
  f <- m2_fixture()
  a <- m2_node()
  b <- m2_node("b.rds", 9L, list(list(path = "a.rds", version_id = NULL)))
  b$calculation <- function(path, parents) {
    ignored <- pipload::pip_read(parents[[1L]]$path, format = "rds",
      version = parents[[1L]]$version_id, verbose = FALSE)
    list(x = value)
  }
  c <- m2_node("c.rds", 10L, list(list(path = "b.rds", version_id = NULL)))
  testthat::local_mocked_bindings(
    pd_dependency_plan = function(...) stop("shadow authority"),
    pd_snapshot_facts = function(...) stop("shadow facts"),
    pd_manifest_read = function(...) stop("shadow manifest"),
    .package = "pipdata")
  first <- m2_run(f, list(a, b, c))
  expect_identical(first$status, rep("built", 3L))
  expect_true(all(first$attempted))
  expect_equal(length(unique(first$path)), 3L)
  old <- pipload::pip_read(first$path[[2L]], "rds", first$version_id[[2L]],
    f$alias, FALSE, metadata_only = TRUE)
  repeated <- m2_run(f, list(a, b, c))
  expect_identical(repeated$status, rep("skipped", 3L))
  expect_false(any(repeated$attempted))
  expect_identical(repeated$receipt_id, first$receipt_id)
  a$constants$value <- 2L
  second <- m2_run(f, list(a, b, c))
  expect_identical(second$status, c("built", "unchanged", "skipped"))
  expect_identical(second$attempted, c(TRUE, TRUE, FALSE))
  expect_identical(second$version_id[[2L]], first$version_id[[2L]])
  expect_false(identical(second$receipt_id[[2L]], first$receipt_id[[2L]]))
  expect_identical(pipload::pip_read(first$path[[2L]], "rds", first$version_id[[2L]],
    f$alias, FALSE, metadata_only = TRUE), old)
})

test_that("M2 deterministic failure feedback preserves governing ID across blocks", {
  f <- m2_fixture()
  a <- m2_node()
  a$calculation <- function(path, parents) {
    rlang::abort("fixture failure", class = "stamp_deterministic_failure")
  }
  b <- m2_node("b.rds", parents = list(list(path = "a.rds", version_id = NULL)))
  first <- m2_run(f, list(a, b))
  expect_identical(first$status, c("failed", "blocked"))
  expect_identical(first$attempted, c(TRUE, FALSE))
  expect_identical(first$failure_class[[1L]], "deterministic")
  governing <- first$governing_failure_id[[1L]]
  expect_identical(governing, first$outcome_id[[1L]])
  for (i in 1:2) {
    blocked <- m2_run(f, list(a, b))
    expect_identical(blocked$status, c("blocked", "blocked"))
    expect_false(any(blocked$attempted))
    expect_identical(blocked$governing_failure_id[[1L]], governing)
  }
  a$calculation <- function(path, parents) list(x = value)
  expect_identical(m2_run(f, list(a, b))$status, c("blocked", "blocked"))
  a$review <- list(outcome_id = governing)
  expect_identical(m2_run(f, list(a, b))$status, c("built", "built"))
})

test_that("M2 explicit formats preserve NULL values and reject unsupported declarations", {
  for (format in c("rds", "qs2", "fst")) {
    f <- m2_fixture()
    node <- m2_node(paste0("a.", format), data.frame(id = 1:2))
    built <- m2_run(f, list(node))
    expect_identical(built$status, "built")
    expect_equal(as.data.frame(pipload::pip_read(built$path, format,
      built$version_id, f$alias, FALSE)), data.frame(id = 1:2))
    node$calculation <- function(path, parents) list(x = NULL)
    if (format == "fst") {
      expect_error(m2_run(f, list(node)), class = "pipdata_engine_integrity_error")
    } else {
      null <- m2_run(f, list(node))
      expect_identical(null$status, "built")
      expect_null(pipload::pip_read(null$path, format, null$version_id, f$alias, FALSE))
    }
  }
  f <- m2_fixture()
  node <- m2_node()
  node$format <- "RDS"
  expect_error(m2_run(f, list(node)), "format")
  node <- m2_node()
  node$parents <- list(list(path = "parent.rds", version_id = "latest"))
  expect_error(m2_run(f, list(node)), "exact")
})

test_that("M2 audit edits do not invalidate and diamond selects every supplied root", {
  f <- m2_fixture()
  a <- m2_node()
  z <- m2_node("z.rds")
  b <- m2_node("b.rds", parents = list(list(path = "a.rds", version_id = NULL),
                                      list(path = "z.rds", version_id = NULL)))
  c <- m2_node("c.rds", parents = list(list(path = "a.rds", version_id = NULL),
                                      list(path = "b.rds", version_id = NULL)))
  nodes <- list(c, b, z, a)
  first <- m2_run(f, nodes)
  expect_identical(nrow(first), 4L)
  expect_setequal(basename(first$path), c("a.rds", "b.rds", "c.rds", "z.rds"))
  expect_true(all(first$status == "built"))
  nodes[[4L]]$audit <- list(source = "description-only")
  second <- m2_run(f, nodes)
  expect_true(all(second$status == "skipped"))
  expect_identical(first$receipt_id, second$receipt_id)
  a$parents <- list(list(path = "c.rds", version_id = NULL))
  expect_error(m2_run(f, list(a, b, c, z)), "[Cc]ycl")
})

test_that("M2 typed semantic values change versions without losing NA distinctions", {
  f <- m2_fixture()
  node <- m2_node()
  node$semantic_schema <- list(unit = list(type = "integer", shape = "scalar",
    required = TRUE, nullable = TRUE, allow_na = TRUE))
  node$semantic <- list(unit = NA_integer_)
  first <- m2_run(f, list(node))
  expect_identical(first$status, "built")
  node$semantic$unit <- 1L
  second <- m2_run(f, list(node))
  expect_identical(second$status, "built")
  expect_false(identical(second$version_id, first$version_id))
  expect_identical(pipload::pip_read(first$path, "rds", first$version_id,
    f$alias, FALSE), 1L)
})

test_that("M2 unknown errors block and external failures retry only on next call", {
  for (kind in c("unknown", "external")) {
    f <- m2_fixture()
    node <- m2_node()
    node$calculation <- if (kind == "external") {
      function(path, parents) rlang::abort("transient", class = "stamp_external_failure")
    } else function(path, parents) stop("unknown actual failure")
    independent <- m2_node("z.rds")
    first <- m2_run(f, list(node, independent))
    expect_identical(first$status, c("failed", "built"))
    expect_identical(first$failure_class[[1L]], kind)
    expect_true(all(first$attempted))
    second <- m2_run(f, list(node, independent))
    expect_identical(second$status[[1L]], if (kind == "external") "failed" else "blocked")
    expect_identical(second$attempted[[1L]], kind == "external")
  }
})

test_that("M2 lease scope excludes proof and legacy no-recipes route is preserved", {
  f <- m2_fixture()
  recipes <- list(context = f$context, nodes = list(m2_node()))
  first <- pd_engine_prepare(recipes)
  recipes$nodes[[1L]]$constants$value <- 2L
  second <- pd_engine_prepare(recipes)
  expect_identical(first$context$scope_id, second$context$scope_id)
  expect_false(identical(first$context$proof_digest, second$context$proof_digest))
  lease <- pd_lease_acquire(first$context,
    root = file.path(f$root, ".pipdata-engine-state"))
  withr::defer(pd_lease_release(lease))
  expect_error(pd_run_pipeline(recipes = recipes), class = "pipdata_manifest_lease_held")
  testthat::local_mocked_bindings(
    pd_validate_pipeline_arguments = function(...) stop("entered legacy route"),
    .package = "pipdata")
  expect_error(pd_run_pipeline(recipes = NULL), "entered legacy route")
})

test_that("M2 mutation followed by classified failure still latches postguard", {
  f <- m2_fixture()
  old <- options()
  withr::defer(options(old))
  withr::defer(options(pipdata.engine_fault = NULL))
  node <- m2_read_node(f)
  testthat::local_mocked_bindings(pip_read = m2_mock_read(function(...) {
    options(pipdata.engine_fault = TRUE)
    rlang::abort("original calculation", class = "stamp_deterministic_failure")
  }), .package = "pipload")
  error <- tryCatch(m2_run(f, list(node, m2_node("z.rds"))), error = identity)
  expect_s3_class(error, "pipdata_engine_integrity_error")
  expect_identical(error$feedback$attempted, c(TRUE, FALSE))
  expect_true(any(vapply(error$causes, function(cnd)
    inherits(cnd, "stamp_deterministic_failure"), logical(1L))))
  expect_false(any(error$feedback$status %in% c("built", "unchanged")))
})

test_that("M2 lost lease retains feedback and secondary release failure", {
  f <- m2_fixture()
  node <- m2_read_node(f)
  context <- pd_engine_prepare(list(context = f$context, nodes = list(node)))$context
  owner <- file.path(f$root, ".pipdata-engine-state", "dependency-manifest",
                     context$scope_id, "writer.lease", "owner.rds")
  original <- NULL
  testthat::local_mocked_bindings(pip_read = m2_mock_read(function(...) {
    original <<- readRDS(owner)
    changed <- original
    changed$token <- "another-owner"
    saveRDS(changed, owner)
    1L
  }), .package = "pipload")
  withr::defer({
    if (!is.null(original)) {
      saveRDS(original, owner)
      pd_lease_release(original)
    }
  })
  error <- tryCatch(m2_run(f, list(node, m2_node("z.rds"))), error = identity)
  expect_s3_class(error, "pipdata_engine_integrity_error")
  expect_identical(error$feedback$attempted, c(TRUE, FALSE))
  expect_true(sum(vapply(error$causes, function(cnd)
    inherits(cnd, "pipdata_manifest_lease_lost"), logical(1L))) >= 2L)
  expect_identical(readRDS(owner)$token, "another-owner")
})

test_that("M2 cancellation remains cancellation after cleanup fails", {
  f <- m2_fixture()
  node <- m2_read_node(f)
  cancelled <- structure(list(message = "cancel fixture", call = NULL,
    marker = "original-cancel"), class = c("interrupt", "condition"))
  cleanup_error <- simpleError("release fixture")
  testthat::local_mocked_bindings(pip_read = m2_mock_read(function(...) {
    signalCondition(cancelled)
  }), .package = "pipload")
  release <- pd_lease_release
  testthat::local_mocked_bindings(pd_lease_release = function(lease) {
    release(lease)
    stop(cleanup_error)
  }, .package = "pipdata")
  error <- tryCatch(m2_run(f, list(node)), interrupt = identity)
  expect_s3_class(error, "interrupt")
  expect_identical(error$marker, "original-cancel")
  expect_true(error$feedback$attempted[[1L]])
  expect_true(any(vapply(error$secondary, identical, logical(1L), cleanup_error)))
  expect_false(length(list.files(f$root, pattern = "owner.rds", recursive = TRUE,
                                 all.files = TRUE)) > 0L)
})

test_that("M2 deterministic review resumes in a fresh process using public feedback", {
  f <- m2_fixture()
  node <- m2_node()
  node$calculation <- function(path, parents) {
    rlang::abort("restart fixture", class = "stamp_deterministic_failure")
  }
  failed <- m2_run(f, list(node))
  for (i in 1:2) expect_identical(m2_run(f, list(node))$status, "blocked")
  environment(node$calculation) <- baseenv()
  input <- file.path(f$root, "restart-input.rds")
  output <- file.path(f$root, "restart-output.rds")
  script <- file.path(f$root, "restart.R")
  source <- getNamespaceInfo(asNamespace("pipdata"), "path")
  saveRDS(list(root = f$root, governing = failed$governing_failure_id[[1L]],
               recipes = list(context = f$context, nodes = list(node)),
               source = source, libraries = .libPaths(), output = output), input)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "input <- readRDS(args[[1L]])",
    "options(pipfun.log.auto = FALSE, pipfun.main_dir = Sys.getenv('PIP_ROOT_DIR'),",
    "  pipdata.dependency_manifest_path = file.path(Sys.getenv('PIP_ROOT_DIR'), 'manifest'))",
    ".libPaths(input$libraries, include.site = FALSE)",
    "pkgload::load_all(input$source, helpers = FALSE, quiet = TRUE)",
    "stamp::st_init(input$root, alias = 'restart', verbose = FALSE)",
    "stamp::st_opts(versioning = 'content', retain_versions = Inf, meta_format = 'both')",
    "options(warn = 2)",
    "blocked <- pipdata::pd_run_pipeline(recipes = input$recipes, verbose = FALSE)",
    "stopifnot(identical(blocked$status, 'blocked'), !blocked$attempted,",
    " identical(blocked$governing_failure_id, input$governing))",
    "input$recipes$nodes[[1L]]$calculation <- function(path, parents) list(x = value)",
    "input$recipes$nodes[[1L]]$review <- list(outcome_id = blocked$governing_failure_id[[1L]])",
    "result <- pipdata::pd_run_pipeline(recipes = input$recipes, verbose = FALSE)",
    "saveRDS(result, input$output)"), script)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"),
    c("--vanilla", shQuote(script), shQuote(input)),
    stdout = file.path(f$root, "restart.stdout.log"),
    stderr = file.path(f$root, "restart.stderr.log"), timeout = 120)
  expect_identical(status, 0L)
  result <- readRDS(output)
  expect_identical(result$status, "built")
  expect_true(result$attempted)
  info <- stamp::st_execution_info(result$path, result$receipt_id, f$alias)
  expect_identical(info$state, "success")
})

test_that("M2 missing parent, returned NULL and omitted x have distinct facts", {
  f <- m2_fixture()
  child <- m2_node("child.rds", parents = list(list(path = "absent.rds", version_id = "exact-missing")))
  missing <- m2_run(f, list(child))
  expect_identical(missing$status, "blocked")
  expect_false(missing$attempted)
  expect_true(is.na(missing$version_id))
  omitted <- m2_node()
  omitted$calculation <- function(path, parents) list(metadata = list())
  failed <- m2_run(f, list(omitted))
  expect_identical(failed$status, "failed")
  expect_true(failed$attempted)
  expect_identical(failed$failure_class, "unknown")
})

test_that("M2 exact receipt removal never falls back to an earlier success", {
  f <- m2_fixture()
  node <- m2_node()
  earlier <- m2_run(f, list(node))
  node$constants$unused <- 1L
  first <- m2_run(f, list(node))
  expect_identical(first$status, "built")
  expect_false(identical(first$receipt_id, earlier$receipt_id))
  receipt <- file.path(first$path, "executions", first$receipt_id, "receipt.json")
  expect_true(file.exists(receipt))
  expect_true(startsWith(tolower(normalizePath(receipt, winslash = "/")),
                         paste0(tolower(f$root), "/")))
  # Only this captured public-created receipt is changed; keep evidence backup.
  backup <- file.path(f$root, "captured-receipt.json")
  expect_true(file.rename(receipt, backup))
  error <- tryCatch(m2_run(f, list(node)), error = identity)
  expect_s3_class(error, "pipdata_engine_integrity_error")
  expect_false(any(error$feedback$status %in% c("built", "unchanged", "skipped")))
})

test_that("M2 corrupted exact metadata and cyclic outcome links fail closed", {
  for (kind in c("metadata", "history")) {
    f <- m2_fixture()
    node <- m2_node()
    if (kind == "history") {
      node$calculation <- function(path, parents) stop("history fixture")
    }
    first <- m2_run(f, list(node))
    target <- if (kind == "metadata") {
      file.path(first$path, "versions", first$version_id, "sidecar.json")
    } else file.path(first$path, "executions", first$outcome_id, "receipt.json")
    expect_true(file.exists(target))
    expect_true(startsWith(tolower(normalizePath(target, winslash = "/")),
                           paste0(tolower(f$root), "/")))
    writeLines("{invalid captured proof", target)
    expect_error(m2_run(f, list(node)), class = "pipdata_engine_integrity_error")
  }
})

test_that("M2 exact outcome traversal rejects cycles and cross-artifact links", {
  # Real public records are used. Corruption is limited to captured receipt paths.
  for (kind in c("cycle", "missing", "cross")) {
    f <- m2_fixture()
    node <- m2_node()
    node$calculation <- function(path, parents) stop("failure fixture")
    first <- m2_run(f, list(node))
    blocked <- m2_run(f, list(node))
    receipt <- file.path(blocked$path, "executions", blocked$outcome_id, "receipt.json")
    expect_true(file.exists(receipt))
    expect_true(startsWith(tolower(normalizePath(receipt, winslash = "/")),
                           paste0(tolower(f$root), "/")))
    record <- jsonlite::read_json(receipt, simplifyVector = FALSE)
    if (kind == "cycle") record$previous_id <- record$id
    if (kind == "missing") record$previous_id <- "exact-absent-id"
    if (kind == "cross") record$artifact$path <- "different.rds"
    jsonlite::write_json(record, receipt, auto_unbox = TRUE, null = "null")
    # Keep the integrity marker valid so these cases reach envelope/chain checks,
    # rather than merely repeating the separate checksum-corruption test.
    writeLines(secretbase::siphash13(file = receipt),
               file.path(dirname(receipt), "COMMITTED"))
    error <- tryCatch(m2_run(f, list(node)), error = identity)
    expect_s3_class(error, "pipdata_engine_integrity_error")
    expect_true(any(grepl(if (kind == "cross") "envelope" else "chain",
                         error$feedback$message)))
  }
})

test_that("M2 helper-only changes and omitted subscripts have truthful proof", {
  f <- m2_fixture()
  node <- m2_node()
  node$calculation <- function(path, parents) list(x = helper(value))
  node$helpers <- list(helper = function(x) x + 1L)
  first <- m2_run(f, list(node))
  proof1 <- pd_engine_proof(node$calculation, node$helpers, node$constants)
  node$helpers$helper <- function(x) x + 2L
  proof2 <- pd_engine_proof(node$calculation, node$helpers, node$constants)
  expect_false(identical(proof1$components[["helper:helper"]],
                         proof2$components[["helper:helper"]]))
  second <- m2_run(f, list(node))
  expect_identical(second$status, "built")
  expect_identical(pipload::pip_read(second$path, "rds", second$version_id, f$alias, FALSE), 3L)
  node <- m2_node("matrix.rds", matrix(1:4, ncol = 2L))
  node$calculation <- function(path, parents) list(x = value[, 1L])
  expect_identical(m2_run(f, list(node))$status, "built")
})

test_that("M2 rejects ambient conversion, hidden S3 dispatch and undeclared reads", {
  for (calculation in list(
    function(path, parents) list(x = sort(value)),
    function(path, parents) list(x = as.character(value)),
    function(path, parents) list(x = mean(structure(1L, class = "hidden"))))) {
    expect_error(pd_engine_proof(calculation, list(), list(value = 1L)), "Unsupported")
  }
  for (kind in c("missing", "NULL", "latest", "numeric", "undeclared")) {
    f <- m2_fixture()
    node <- m2_read_node(f)
    node$calculation <- switch(kind,
      missing = function(path, parents) list(x = pipload::pip_read(parents[[1L]]$path, format = "rds")),
      NULL = function(path, parents) list(x = pipload::pip_read(parents[[1L]]$path, format = "rds", version = NULL)),
      latest = function(path, parents) list(x = pipload::pip_read(parents[[1L]]$path, format = "rds", version = "latest")),
      numeric = function(path, parents) list(x = pipload::pip_read(parents[[1L]]$path, format = "rds", version = 0L)),
      undeclared = function(path, parents) list(x = pipload::pip_read(path, format = "rds", version = parents[[1L]]$version_id)))
    result <- m2_run(f, list(node))
    expect_identical(result$status, "failed")
    expect_match(result$message, if (kind == "undeclared")
      "not an effective declared parent" else "explicit exact declared parent pin")
  }
})

test_that("M2 explicit cancellation prevents independent attempts and preserves condition", {
  f <- m2_fixture()
  node <- m2_node()
  node$calculation <- function(path, parents) rlang::abort("explicit cancel",
    class = "pipdata_pipeline_cancellation", marker = "original-explicit")
  error <- tryCatch(m2_run(f, list(node, m2_node("z.rds"))), error = identity)
  expect_s3_class(error, "pipdata_pipeline_cancellation")
  expect_identical(error$marker, "original-explicit")
  expect_false(error$feedback$attempted[[2L]])
})

test_that("M2 save failure after calculation is integrity feedback, never success", {
  f <- m2_fixture()
  # fst cannot serialize NULL: a real producer publication failure, no API mocks.
  node <- m2_node("failure.fst")
  node$calculation <- function(path, parents) list(x = NULL)
  error <- tryCatch(m2_run(f, list(node)), error = identity)
  expect_s3_class(error, "pipdata_engine_integrity_error")
  expect_identical(error$feedback$reason, "publication_failed")
  expect_true(error$feedback$attempted)
  expect_true(is.na(error$feedback$version_id))
})

test_that("M2 retained success cannot replace a later governing failure", {
  f <- m2_fixture()
  first <- m2_run(f, list(m2_node()))
  node <- m2_node()
  node$calculation <- function(path, parents) stop("changed-proof failure")
  failed <- m2_run(f, list(node))
  for (i in 1:2) {
    blocked <- m2_run(f, list(node))
    expect_identical(blocked$status, "blocked")
    expect_identical(blocked$governing_failure_id, failed$outcome_id)
    expect_true(is.na(blocked$version_id))
  }
  expect_error(pd_engine_outcome(blocked$path, blocked$outcome_id, f$alias,
    tolower(f$root), max_records = 1L), "bounded lookup limit")
  expect_identical(pipload::pip_read(first$path, "rds", first$version_id, f$alias, FALSE), 1L)
})

test_that("M2 same-pin semantic metadata retains types, shapes and nullability", {
  values <- list(TRUE, 1L, 1, "one", NA_integer_, NULL, integer(), c(1L, 2L))
  types <- c("logical", "integer", "double", "character", rep("integer", 4L))
  for (i in seq_along(values)) {
    f <- m2_fixture()
    node <- m2_node()
    node$semantic_schema <- list(unit = list(type = types[[i]],
      shape = if (i >= 7L) "vector" else "scalar", required = FALSE,
      nullable = TRUE, allow_na = TRUE))
    node$semantic <- list(unit = values[[i]])
    built <- m2_run(f, list(node))
    expect_identical(built$status, "built")
    metadata <- pipload::pip_read(built$path, "rds", built$version_id, f$alias,
                                 FALSE, metadata_only = TRUE)
    expect_identical(metadata$unit, values[[i]])
    expect_true("unit" %in% names(metadata))
    if (i == 1L) {
      node$semantic <- list()
      absent <- m2_run(f, list(node))
      expect_false(identical(absent$version_id, built$version_id))
      expect_false("unit" %in% names(pipload::pip_read(absent$path, "rds",
        absent$version_id, f$alias, FALSE, metadata_only = TRUE)))
      node$semantic <- list(unit = "wrong-type")
      expect_identical(m2_run(f, list(node))$status, "blocked")
    }
  }
  f <- m2_fixture()
  node <- m2_node()
  node$semantic_schema <- list(unit = list(type = "integer", shape = "scalar",
    required = TRUE, nullable = FALSE, allow_na = FALSE))
  node$semantic <- list(unit = 1L)
  node$calculation <- function(path, parents) list(x = value, metadata = list(unit = 2L))
  expect_identical(m2_run(f, list(node))$status, "failed")
})

test_that("M2 ignores contradictory legacy manifests while real stamp governs", {
  f <- m2_fixture()
  legacy_root <- file.path(f$root, "legacy-only")
  withr::local_options(list(pipdata.dependency_manifest_path = legacy_root))
  first <- m2_run(f, list(m2_node()))
  context <- pd_dependency_context(release = f$context$release, identity = "TEST",
    main_dir = f$root, namespace = "legacy-only")
  stale <- pd_empty_manifest(context)
  current <- pd_empty_manifest(context)
  current$records <- data.table::data.table(stage = "clean", entity_id = "a.rds",
    output_version_id = first$version_id, output_hash = "legacy-current-claim",
    input_hash = "legacy-input-claim", code_hash = "legacy-code-claim",
    output_receipts = list(list(list(alias = f$alias, artifact = "a.rds",
      path = first$path, version_id = first$version_id, content_hash = "legacy-current-claim"))))
  current$inputs <- data.table::data.table(stage = "clean", entity_id = "a.rds",
    name = "canonical", version_id = "legacy-input-claim", content_hash = "legacy-input-claim")
  publish <- function(payload, root) {
    lease <- pd_lease_acquire(context, root)
    on.exit(pd_lease_release(lease))
    pd_manifest_publish(payload, context, lease, root)
  }
  publish(stale, legacy_root)
  current_root <- file.path(f$root, "legacy-current")
  publish(current, current_root)
  testthat::local_mocked_bindings(pd_manifest_read = function(...) stop("legacy authority"),
    pd_dependency_plan = function(...) stop("legacy selector"), .package = "pipdata")
  expect_identical(m2_run(f, list(m2_node()))$status, "skipped")
  options(pipdata.dependency_manifest_path = current_root)
  node <- m2_node(value = 2L)
  expect_identical(m2_run(f, list(node))$status, "built")
  node$calculation <- function(path, parents) stop("current legacy cannot override failure")
  expect_identical(m2_run(f, list(node))$status, "failed")
  expect_identical(m2_run(f, list(node))$status, "blocked")
})

test_that("M2 ambient conversions and implicit operator methods cannot supply hidden inputs", {
  f <- m2_fixture()
  node <- m2_node(value = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"))
  node$calculation <- function(path, parents) list(x = as.character(value))
  for (timezone in c("UTC", "America/New_York")) {
    withr::local_envvar(c(TZ = timezone))
    expect_error(m2_run(f, list(node)), "Unsupported")
  }
  original <- getS3method("+", "Date")
  withr::defer(registerS3method("+", "Date", original, envir = baseenv()))
  for (value in c(10L, 20L)) {
    registerS3method("+", "Date", function(e1, e2) value, envir = baseenv())
    node <- m2_node(value = as.Date("2026-01-01"))
    node$calculation <- function(path, parents) list(x = value + 1L)
    result <- m2_run(f, list(node))
    expect_true(result$status %in% c("failed", "blocked"))
    expect_false(result$status %in% c("built", "skipped", "unchanged"))
  }
})

test_that("M2 rejects a pre-existing escaping lease junction before writing", {
  f <- m2_fixture()
  outside <- m2_fixture()$root
  link <- file.path(f$root, ".pipdata-engine-state")
  status <- system2("cmd.exe", c("/c", "mklink", "/J", shQuote(link), shQuote(outside)),
    stdout = file.path(f$root, "junction.stdout"), stderr = file.path(f$root, "junction.stderr"))
  expect_identical(status, 0L)
  withr::defer(system2("cmd.exe", c("/c", "rmdir", shQuote(link))))
  before <- list.files(outside, all.files = TRUE, recursive = TRUE)
  expect_error(m2_run(f, list(m2_node())), "escapes")
  expect_identical(list.files(outside, all.files = TRUE, recursive = TRUE), before)
})

test_that("M2 character ordering cannot introduce ambient collation into proof", {
  for (op in c("<", ">", "<=", ">=", "min", "max", "range")) {
    boundary <- pd_engine_base_boundary(op)
    expect_error(boundary("Z", "a"), "Ambient character collation")
    expect_error(boundary(1L, "a"), "Ambient character collation")
  }
  expect_error(pd_engine_base_boundary("range")(list("Z", "a")),
               "Ambient character collation")
})

test_that("M2 nested registered methods cannot change results outside proof", {
  f <- m2_fixture()
  methods <- get(".__S3MethodsTable__.", asNamespace("base"))
  existed <- exists("length.Date", methods, inherits = FALSE)
  previous <- if (existed) get("length.Date", methods, inherits = FALSE) else NULL
  withr::defer({
    if (existed) registerS3method("length", "Date", previous, envir = baseenv())
    else rm(list = "length.Date", envir = methods)
  })
  node <- m2_node(value = as.Date("2026-01-01"))
  node$calculation <- function(path, parents) list(x = lengths(list(value)))
  for (answer in c(10L, 20L)) {
    calls <- 0L
    registerS3method("length", "Date", function(x) {
      calls <<- calls + 1L
      answer
    }, envir = baseenv())
    expect_identical(lengths(list(as.Date("2026-01-01"))), answer)
    calls <- 0L
    result <- m2_run(f, list(node))
    expect_true(result$status %in% c("failed", "blocked"))
    expect_true(is.na(result$version_id))
    expect_identical(calls, 0L)
  }
  expect_identical(pd_engine_base_boundary("lengths")(list(1:3)), 3L)
})

test_that("M2 incompatible pipload is rejected before any write-side work", {
  f <- m2_fixture()
  before <- list.files(f$root, recursive = TRUE, all.files = TRUE)
  lease_calls <- 0L
  acquire <- pd_lease_acquire
  testthat::local_mocked_bindings(pd_lease_acquire = function(...) {
    lease_calls <<- lease_calls + 1L
    acquire(...)
  }, .package = "pipdata")
  testthat::local_mocked_bindings(pip_read = function(id, format = NULL,
    version = NULL, alias = NULL, verbose = TRUE, pk = NULL) {
      stop("Incompatible adapter must not be invoked")
    }, .package = "pipload")
  expect_error(m2_run(f, list(m2_node())), "pipload::pip_read.*metadata_only",
               class = "pipdata_engine_dependency_error")
  expect_identical(lease_calls, 0L)
  expect_false(dir.exists(file.path(f$root, "a.rds")))
  expect_identical(list.files(f$root, recursive = TRUE, all.files = TRUE), before)
})

test_that("M2 incompatible stamp requests are rejected before lease or planning", {
  f <- m2_fixture()
  before <- list.files(f$root, recursive = TRUE, all.files = TRUE)
  lease_calls <- 0L
  planner_calls <- 0L
  acquire <- pd_lease_acquire
  testthat::local_mocked_bindings(pd_lease_acquire = function(...) {
    lease_calls <<- lease_calls + 1L
    acquire(...)
  }, .package = "pipdata")
  testthat::local_mocked_bindings(st_plan_rebuild = function(targets,
    include_targets = FALSE) {
      planner_calls <<- planner_calls + 1L
      stop("Incompatible planner must not be invoked")
    }, .package = "stamp")
  expect_error(m2_run(f, list(m2_node())), "stamp::st_plan_rebuild.*requests",
               class = "pipdata_engine_dependency_error")
  expect_identical(lease_calls, 0L)
  expect_identical(planner_calls, 0L)
  expect_identical(list.files(f$root, recursive = TRUE, all.files = TRUE), before)
})

test_that("M2 unavailable codetools fails actionably before lease without bypass", {
  f <- m2_fixture()
  before <- list.files(f$root, recursive = TRUE, all.files = TRUE)
  lease_calls <- 0L
  acquire <- pd_lease_acquire
  available <- base::requireNamespace
  testthat::local_mocked_bindings(pd_lease_acquire = function(...) {
    lease_calls <<- lease_calls + 1L
    acquire(...)
  }, .package = "pipdata")
  # Controlled availability seam; the real package/library stays untouched.
  testthat::local_mocked_bindings(requireNamespace = function(package, ...,
    quietly = FALSE) {
      if (identical(package, "codetools")) return(FALSE)
      available(package, ..., quietly = quietly)
    }, .package = "base")
  expect_error(m2_run(f, list(m2_node())), "codetools.*approved library",
               class = "pipdata_engine_dependency_error")
  expect_identical(lease_calls, 0L)
  expect_identical(list.files(f$root, recursive = TRUE, all.files = TRUE), before)
})
