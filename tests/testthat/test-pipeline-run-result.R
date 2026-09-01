pipeline_result_identity <- function(generation) {
  list(
    filename = paste0("manifest-v1-", generation, ".rds"),
    uuid = paste0("u", generation), checksum = paste0("c", generation),
    generation = generation
  )
}

pipeline_result_stage <- function(stage, status = "cached",
                                  reason = "current") {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  entity <- if (stage == "clean") "S1" else "P1"
  units <- data.table::data.table(
    stage = stage, entity_id = entity,
    survey_id = "S1", pip_id = if (stage == "clean") NA_character_ else "P1",
    status = status, action = if (status == "cached") "none" else "refresh",
    reason_codes = list(reason), input_hash = NA_character_,
    output_hash = NA_character_, started_at = as.POSIXct(NA, tz = "UTC"),
    completed_at = as.POSIXct(NA, tz = "UTC")
  )
  context <- list(
    run_id = "run", release = "20260831", identity = "TEST",
    dependency = list(
      scope_id = "scope", context_hash = "context",
      plan_hash = paste0(stage, "-plan")
    )
  )
  identity <- pipeline_result_identity(1)
  provenance <- list(
    release = "20260831", identity = "TEST", scope_id = "scope",
    context_hash = "context", plan_hash = paste0(stage, "-plan"),
    manifest_before = identity, manifest_after = identity,
    checkpoint_generations = numeric(), final_evidence_manifest = identity,
    stage_reason_codes = reason
  )
  new_pipdata_stage_result(
    context, stage, FALSE, units, pd_empty_artifact_references(),
    log_ref = list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = NA_character_, log_checkpoint = NULL
    ),
    provenance = provenance, started_at = now, completed_at = now
  )
}

test_that("pipeline aggregate has the exact frozen schema and derived counts", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  clean <- pipeline_result_stage("clean")
  result <- new_pipdata_pipeline_result(
    run_id = "run",
    stage_results = list(clean = clean, metadata = NULL, deflate = NULL),
    warnings = list(), errors = list(),
    plan_hashes = c(
      initial = "initial-plan", clean = "clean-plan",
      metadata = NA_character_, deflate = NA_character_
    ),
    manifest_before = pipeline_result_identity(1),
    manifest_after = pipeline_result_identity(1),
    log_ref = list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = "pipeline_run_summary_inf",
      log_checkpoint = NULL
    ),
    started_at = now, completed_at = now
  )

  expect_s3_class(result, "pipdata_pipeline_result")
  expect_named(result, c(
    "schema_version", "run_id", "status", "terminal", "stage_results",
    "counts", "warnings", "errors", "plan_hashes", "manifest_before",
    "manifest_after", "log_ref", "started_at", "completed_at"
  ))
  expect_identical(result$schema_version, 1L)
  expect_identical(result$status, "cached")
  expect_named(result$stage_results, c("clean", "metadata", "deflate"))
  expect_named(result$counts, c(
    "selected", "attempted", "succeeded", "failed", "skipped", "cached",
    "blocked", "warnings", "errors"
  ))
  expect_true(all(vapply(result$counts, is.integer, logical(1L))))
  expect_identical(result$counts$selected, 1L)
  expect_identical(result$counts$cached, 1L)
  expect_no_error(validate_pipdata_pipeline_result(result))
})

test_that("pipeline aggregate counts blocked units as skipped", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  metadata <- pipeline_result_stage(
    "metadata", status = "skipped", reason = "upstream_failed"
  )
  result <- new_pipdata_pipeline_result(
    "run", list(clean = NULL, metadata = metadata, deflate = NULL),
    list(), list(),
    c(initial = "initial", clean = NA_character_,
      metadata = "metadata-plan", deflate = NA_character_),
    pipeline_result_identity(1), pipeline_result_identity(1),
    list(name = "pipdata_log", run_id = "run",
         summary_discriminator = "pipeline_run_summary_inf",
         log_checkpoint = NULL),
    now, now
  )

  expect_identical(result$status, "skipped")
  expect_identical(result$counts$skipped, 1L)
  expect_identical(result$counts$blocked, 1L)
})

test_that("pipeline aggregate rejects unavailable-wave hashes", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  expect_error(
    new_pipdata_pipeline_result(
      "run", list(clean = NULL, metadata = NULL, deflate = NULL),
      list(), list(),
      c(initial = "initial", clean = "fabricated",
        metadata = NA_character_, deflate = NA_character_),
      NULL, NULL,
      list(name = "pipdata_log", run_id = "run",
           summary_discriminator = "pipeline_run_summary_inf",
           log_checkpoint = NULL),
      now, now
    ),
    class = "pipdata_pipeline_result_invalid"
  )
})

test_that("pipeline aggregate cannot discard a valid starting manifest", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  expect_error(
    new_pipdata_pipeline_result(
      "run", list(clean = NULL, metadata = NULL, deflate = NULL),
      list(), list(),
      c(initial = "initial", clean = NA_character_,
        metadata = NA_character_, deflate = NA_character_),
      pipeline_result_identity(1), NULL,
      list(name = "pipdata_log", run_id = "run",
           summary_discriminator = "pipeline_run_summary_inf",
           log_checkpoint = NULL),
      now, now
    ),
    class = "pipdata_pipeline_result_invalid"
  )
})

test_that("pipeline portable projection is deterministic and pointer free", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  clean <- pipeline_result_stage("clean")
  make_result <- function() new_pipdata_pipeline_result(
    "run", list(clean = clean, metadata = NULL, deflate = NULL),
    list(), list(),
    c(initial = "initial", clean = "clean-plan",
      metadata = NA_character_, deflate = NA_character_),
    pipeline_result_identity(1), pipeline_result_identity(1),
    list(name = "pipdata_log", run_id = "run",
         summary_discriminator = "pipeline_run_summary_inf",
         log_checkpoint = NULL),
    now, now
  )
  first <- pd_pipeline_result_portable(make_result())
  second <- pd_pipeline_result_portable(make_result())
  prohibited <- function(x) {
    is.environment(x) || inherits(x, "externalptr") ||
      data.table::is.data.table(x) ||
      (is.list(x) && any(vapply(x, prohibited, logical(1L))))
  }

  expect_false(prohibited(first))
  expect_identical(serialize(first, NULL, version = 3L),
                   serialize(second, NULL, version = 3L))
  expect_no_error(validate_pipdata_pipeline_result(first, portable = TRUE))
})

test_that("pipeline print method is registered", {
  expect_true("print.pipdata_pipeline_result" %in% getNamespaceExports("pipdata") ||
                !is.null(getS3method(
                  "print", "pipdata_pipeline_result", optional = TRUE
                )))
})
