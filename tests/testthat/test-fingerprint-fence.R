fingerprint_fence_fixture <- function(env = parent.frame()) {
  root <- withr::local_tempdir(pattern = "ff-", .local_envir = env)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- list(pipfun = getFromNamespace(".pipenv", "pipfun"),
    aliases = getFromNamespace(".stamp_aliases", "stamp"),
    stamp = getFromNamespace(".stamp_state", "stamp"))
  previous <- lapply(state, as.list)
  withr::defer({
    for (key in names(state)) {
      rm(list = ls(state[[key]], all.names = TRUE), envir = state[[key]])
      list2env(previous[[key]], envir = state[[key]])
    }
  }, envir = env)
  rm(list = ls(state$pipfun, all.names = TRUE), envir = state$pipfun)
  rm(list = ls(state$aliases, all.names = TRUE), envir = state$aliases)
  withr::local_options(list(pipfun.main_dir = root, pipfun.log.auto = FALSE,
    pipfun.verbose = FALSE, pipload.verbose = FALSE,
    pipdata.dependency_manifest_path = file.path(root, "manifest")), .local_envir = env)
  withr::local_envvar(c(PIP_ROOT_DIR = root), .local_envir = env)
  testthat::local_mocked_bindings(
    get_file_from_gh = function(...) data.table::data.table(
      release = "20261103", identity = "TEST"),
    download_from_gh = function(path, file) {
      stopifnot(grepl("/ppp_vintage.csv$", path))
      data.table::fwrite(data.table::data.table(ppp_year = 2021L,
        ppp_rv = "01", ppp_av = "02"), file)
    },
    get_file_info_from_gh = function(...) list(sha = "synthetic-ppp-vintage-v1"),
    .package = "pipfun", .env = env
  )
  pipfun::setup_working_release("20261103", "TEST", ppp = 2021L,
    root_dir = root, verbose = FALSE)
  snapshot <- list(
    context = pd_dependency_context(), fingerprints = pd_code_fingerprints(),
    aux = list(catalog = stamp::st_catalog_query(alias = "aux")), catalogs = list()
  )
  lease <- pd_lease_acquire(snapshot$context)
  withr::defer(pd_lease_release(lease), envir = env)
  manifest <- pd_manifest_read(snapshot$context, allow_absent = TRUE)
  list(root = root, snapshot = snapshot, execution = list(
    context = snapshot$context, snapshot = snapshot, lease = lease,
    manifest_identity = if (inherits(manifest, "pipdata_manifest_absent")) NULL else
      attr(manifest, "manifest_identity")
  ))
}

expect_stale_fingerprint_fences <- function(fixture, snapshot = fixture$snapshot) {
  expect_error(pd_revalidate_snapshot(snapshot), class = "pipdata_dependency_snapshot_stale")
  execution <- fixture$execution
  execution$snapshot <- snapshot
  expect_error(pd_assert_execution_refresh_fence(execution),
    class = "pipdata_dependency_snapshot_stale")
}

test_that("execution fence accepts only a secondary-index representation change", {
  fixture <- fingerprint_fence_fixture()
  snapshot <- fixture$snapshot
  data.table::setindexv(snapshot$fingerprints$components, "stage")
  before <- data.table::copy(snapshot)
  expect_identical(data.table::indices(snapshot$fingerprints$components), "stage")
  expect_no_error(pd_revalidate_snapshot(snapshot))
  execution <- fixture$execution
  execution$snapshot <- snapshot
  expect_no_error(pd_assert_execution_refresh_fence(execution))
  expect_identical(snapshot, before)
  expect_identical(data.table::indices(snapshot$fingerprints$components), "stage")
})

test_that("execution fence rejects an actual configured context change", {
  fixture <- fingerprint_fence_fixture()
  withr::local_options(list(pipdata.dependency_scope = "changed_scope"))
  expect_false(identical(pd_dependency_context(), fixture$snapshot$context))
  expect_stale_fingerprint_fences(fixture)
})

test_that("execution fence rejects component identity membership and schema changes", {
  fixture <- fingerprint_fence_fixture()
  changed <- data.table::copy(fixture$snapshot)
  changed$fingerprints$components$component[[1L]] <- "different_component"
  expect_stale_fingerprint_fences(fixture, changed)
  changed <- data.table::copy(fixture$snapshot)
  changed$fingerprints$components <- changed$fingerprints$components[-1L]
  expect_stale_fingerprint_fences(fixture, changed)
  changed <- data.table::copy(fixture$snapshot)
  changed$fingerprints$components <- as.data.frame(changed$fingerprints$components)
  expect_stale_fingerprint_fences(fixture, changed)
})

test_that("execution fence rejects component summary and audit hash changes", {
  fixture <- fingerprint_fence_fixture()
  for (table in c("components", "summary")) {
    changed <- data.table::copy(fixture$snapshot)
    changed$fingerprints[[table]]$hash[[1L]] <- "different_hash"
    expect_stale_fingerprint_fences(fixture, changed)
  }
  changed <- data.table::copy(fixture$snapshot)
  changed$fingerprints$audit$pipdata <- "different_version"
  expect_stale_fingerprint_fences(fixture, changed)
})
