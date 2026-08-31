test_that("canonical fingerprints ignore source references and preserve formals", {
  f <- function(x = 1) x + 1
  a <- pd_hash_object(pd_canonical_function(f))
  attr(body(f), "srcref") <- structure(integer(), class = "srcref")
  expect_identical(pd_hash_object(pd_canonical_function(f)), a)
  formals(f)$x <- 2
  expect_false(identical(pd_hash_object(pd_canonical_function(f)), a))
})

test_that("component ordering is deterministic", {
  env <- new.env(parent = emptyenv())
  env$a <- function() 1
  env$b <- function() 2
  x <- pd_code_fingerprints(list(clean = c("b", "a")), env)
  y <- pd_code_fingerprints(list(clean = c("a", "b")), env)
  expect_identical(x$summary$hash, y$summary$hash)
})

test_that("curated fingerprint closure passes codetools audit", {
  expect_identical(pd_fingerprint_audit(), character())
  fingerprints <- pd_code_fingerprints()
  expect_true(all(!is.na(fingerprints$components$hash)))
  expect_true("recode_spec.yml" %in% fingerprints$components$component)
  expect_true("wbpip::md_clean_data" %in% fingerprints$components$component)
  expect_true("wbpip::gd_clean_data" %in% fingerprints$components$component)
  expect_true("wbpip::md_compute_quantiles" %in% fingerprints$components$component)
  expect_true("wbpip::deflate_welfare_mean" %in% fingerprints$components$component)
  expect_true(".DOMAIN_COLS" %in% fingerprints$components$component)
  expect_true(all(c(
    "dlw_clean.pipmd", "dlw_clean.pipgd", "wbpip_clean.pipmd",
    "wbpip_clean.pipgd", "deflation.pipmd", "deflation.pipgd"
  ) %in% fingerprints$components$component))
})

fingerprint_hashes <- function() {
  stats::setNames(pd_code_fingerprints()$summary$hash,
                  pd_code_fingerprints()$summary$stage)
}

test_that("constant mutation invalidates only its owning stage", {
  before <- fingerprint_hashes()
  testthat::local_mocked_bindings(
    .DOMAIN_COLS = c(.DOMAIN_COLS, "mutation"), .package = "pipdata"
  )
  after <- fingerprint_hashes()
  expect_false(identical(after[["clean"]], before[["clean"]]))
  expect_identical(after[c("metadata", "deflate")],
                   before[c("metadata", "deflate")])
})

test_that("active S3 method mutation invalidates only clean", {
  before <- fingerprint_hashes()
  testthat::local_mocked_bindings(
    wbpip_clean.pipmd = function(df, ...) df[0], .package = "pipdata"
  )
  after <- fingerprint_hashes()
  expect_false(identical(after[["clean"]], before[["clean"]]))
  expect_identical(after[c("metadata", "deflate")],
                   before[c("metadata", "deflate")])
})

test_that("quantile implementation mutation invalidates only clean", {
  before <- fingerprint_hashes()
  testthat::local_mocked_bindings(
    md_compute_quantiles = function(...) "mutation", .package = "wbpip"
  )
  after <- fingerprint_hashes()
  expect_false(identical(after[["clean"]], before[["clean"]]))
  expect_identical(after[c("metadata", "deflate")],
                   before[c("metadata", "deflate")])
})

test_that("external deflation mutation invalidates only deflate", {
  before <- fingerprint_hashes()
  testthat::local_mocked_bindings(
    deflate_welfare_mean = function(...) "mutation", .package = "wbpip"
  )
  after <- fingerprint_hashes()
  expect_false(identical(after[["deflate"]], before[["deflate"]]))
  expect_identical(after[c("clean", "metadata")],
                   before[c("clean", "metadata")])
})

test_that("recode component change has exclusive recode reason ownership", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  inputs <- pd_build_input_rows(
    "clean", "survey",
    data.table::data.table(
      name = c("dlw", "pfw"), version_id = c("d1", "p1"),
      content_hash = c("dh1", "ph1")
    )
  )
  manifest$inputs <- inputs
  manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "survey", output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = inputs[
      name == "canonical", content_hash
    ], code_hash = "old-summary", output_receipts = list(list())
  )
  manifest$fingerprints <- data.table::data.table(
    stage = "clean", component = c("recode_spec.yml", "clean_fn"),
    hash = c("recode-old", "fn-same")
  )
  snapshot <- list(
    current = data.table::data.table(
      stage = "clean", entity_id = "survey", survey_id = "survey",
      pip_id = NA_character_, output_version_id = "out-v1",
      output_hash = "out-h1", input_hash = inputs[
        name == "canonical", content_hash
      ], legacy_input_hash = "legacy", code_hash = "new-summary",
      input_rows = list(inputs)
    ),
    fingerprints = list(
      summary = data.table::data.table(stage = "clean", hash = "new-summary"),
      components = data.table::data.table(
        stage = "clean", component = c("recode_spec.yml", "clean_fn"),
        hash = c("recode-new", "fn-same")
      )
    )
  )

  facts <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(facts$reason, "recode_spec_changed")
  expect_identical(facts$input, "recode_spec.yml")
})

test_that("non-recode component and legacy summary own stage-code reasons", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  inputs <- pd_build_input_rows(
    "clean", "survey",
    data.table::data.table(
      name = c("dlw", "pfw"), version_id = c("d1", "p1"),
      content_hash = c("dh1", "ph1")
    )
  )
  manifest$inputs <- inputs
  manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "survey", output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = inputs[
      name == "canonical", content_hash
    ], code_hash = "old-summary", output_receipts = list(list())
  )
  current <- data.table::data.table(
    stage = "clean", entity_id = "survey", survey_id = "survey",
    pip_id = NA_character_, output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = inputs[
      name == "canonical", content_hash
    ], legacy_input_hash = "legacy", code_hash = "new-summary",
    input_rows = list(inputs)
  )
  snapshot <- list(
    current = current,
    fingerprints = list(
      summary = data.table::data.table(stage = "clean", hash = "new-summary"),
      components = data.table::data.table(
        stage = "clean", component = "clean_fn", hash = "fn-new"
      )
    )
  )
  manifest$fingerprints <- data.table::data.table(
    stage = "clean", component = "clean_fn", hash = "fn-old"
  )

  component_fact <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(component_fact$reason, "clean_code_changed")
  expect_identical(component_fact$input, "clean_fn")

  manifest$fingerprints <- manifest$fingerprints[0]
  legacy_fact <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(legacy_fact$reason, "clean_code_changed")
  expect_identical(legacy_fact$input, "code")
})
