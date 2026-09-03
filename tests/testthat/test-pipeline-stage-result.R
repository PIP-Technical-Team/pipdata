test_that("stage tables enforce frozen schemas", {
  units <- pd_empty_stage_units()
  artifacts <- pd_empty_artifact_references()
  expect_named(units, c(
    "stage", "entity_id", "survey_id", "pip_id", "status", "action",
    "reason_codes", "input_hash", "output_hash", "started_at", "completed_at"
  ))
  expect_named(artifacts, c(
    "entity_id", "alias", "artifact", "path", "version_id", "content_hash",
    "role", "manifest_generation"
  ))
  expect_no_error(validate_stage_units(units))
  expect_no_error(validate_artifact_references(artifacts))
})

test_that("condition records are stable and bounded", {
  condition <- rlang::error_cnd("specific_failure", message = "failed")
  record <- new_stage_condition_record(
    condition, severity = "error", stage = "deflate", operation = "transform",
    recoverable = TRUE, pip_id = "AAA_2020_TEST_INC_D1",
    details = list(attempt = 1L)
  )
  expect_identical(names(record), c(
    "schema_version", "condition_id", "severity", "code", "classes", "message",
    "stage", "entity_id", "survey_id", "pip_id", "operation", "recoverable",
    "timestamp", "parent_code", "parent_message", "details"
  ))
  expect_identical(record$code, "specific_failure")
  expect_no_error(validate_stage_condition_record(record))
  expect_error(
    new_stage_condition_record(
      severity = "error", code = "bad", message = "bad", stage = "deflate",
      operation = "test", recoverable = TRUE,
      details = list(condition = condition)
    ),
    class = "pipdata_stage_result_invalid"
  )
})

test_that("status precedence and portable tables are deterministic", {
  now <- as.POSIXct("2026-08-26 12:00:00", tz = "UTC")
  units <- pd_empty_stage_units()
  units <- rbind(units, data.table::data.table(
    stage = "deflate", entity_id = "b", survey_id = NA_character_,
    pip_id = "b", status = "success", action = "create",
    reason_codes = list("new_entity"), input_hash = "in",
    output_hash = "out", started_at = now, completed_at = now
  ))
  expect_identical(pd_stage_status(pd_stage_counts(units, list(), list()), FALSE),
                   "success")
  first <- serialize(pd_portable_table(units), NULL, version = 3L)
  second <- serialize(pd_portable_table(units[rev(seq_len(nrow(units)))]), NULL,
                      version = 3L)
  expect_identical(first, second)
})

test_that("complete portable stage results are recursively deterministic", {
  now <- as.POSIXct("2026-08-26 12:00:00", tz = "UTC")
  units <- data.table::data.table(
    stage = c("deflate", "deflate"), entity_id = c("b", "a"),
    survey_id = c("sb", "sa"), pip_id = c("b", "a"),
    status = c("success", "success"), action = c("create", "create"),
    reason_codes = list(c("new_entity"), c("new_entity")),
    input_hash = c("ib", "ia"), output_hash = c("ob", "oa"),
    started_at = c(now, now), completed_at = c(now, now)
  )
  artifacts <- data.table::data.table(
    entity_id = c("b", "a"), alias = "pip_deflated",
    artifact = c("b", "a"), path = c("pb", "pa"),
    version_id = c("vb", "va"), content_hash = c("ob", "oa"),
    role = "primary", manifest_generation = 2
  )
  identity <- list(filename = "m", uuid = "u", checksum = "c", generation = 2)
  context <- list(
    run_id = "run", release = "20260826", identity = "TEST",
    dependency = list(scope_id = "scope", context_hash = "context",
                      plan_hash = "plan")
  )
  provenance <- list(
    release = "20260826", identity = "TEST", scope_id = "scope",
    context_hash = "context", plan_hash = "plan", manifest_before = NULL,
    manifest_after = identity, checkpoint_generations = 2,
    final_evidence_manifest = identity,
    stage_reason_codes = "new_entity"
  )
  make_result <- function(unit_rows, artifact_rows) new_pipdata_stage_result(
    context, "deflate", FALSE, unit_rows, artifact_rows,
    log_ref = list(name = "pipdata_log", run_id = "run",
                   summary_discriminator = "deflate_summary_inf",
                   log_checkpoint = NULL),
    provenance = provenance, started_at = now, completed_at = now
  )
  first <- pd_stage_result_portable(make_result(units, artifacts))
  second <- pd_stage_result_portable(make_result(units[2:1], artifacts[2:1]))
  expect_identical(serialize(first, NULL, version = 3L),
                   serialize(second, NULL, version = 3L))
})

test_that("stage result validation rejects malformed nested records", {
  condition <- new_stage_condition_record(
    severity = "error", code = "bad", message = "bad", stage = "deflate",
    operation = "test", recoverable = FALSE
  )
  condition$recoverable <- NA
  expect_error(validate_stage_condition_record(condition),
               class = "pipdata_stage_result_invalid")
})

test_that("recoverable failures use a controlled unit reason", {
  now <- as.POSIXct("2026-09-01 00:00:00", tz = "UTC")
  action <- data.table::data.table(
    stage = "deflate", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    action = "refresh", input_hash = "input"
  )
  units <- pd_stage_unit_row(
    action, "deflate", "failed", "entity_failed", now, now
  )
  condition <- new_stage_condition_record(
    severity = "error", code = "deflation_na", message = "failed",
    stage = "deflate", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    operation = "transform", recoverable = TRUE, timestamp = now
  )
  context <- list(
    run_id = "run", release = "20260901", identity = "TEST",
    dependency = list(
      scope_id = "scope", context_hash = "context", plan_hash = "plan"
    )
  )
  provenance <- list(
    release = "20260901", identity = "TEST", scope_id = "scope",
    context_hash = "context", plan_hash = "plan", manifest_before = NULL,
    manifest_after = NULL, checkpoint_generations = numeric(),
    final_evidence_manifest = NULL, stage_reason_codes = "entity_failed"
  )

  result <- new_pipdata_stage_result(
    context, "deflate", FALSE, units, pd_empty_artifact_references(),
    errors = list(condition),
    log_ref = list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = "deflate_summary_inf", log_checkpoint = NULL
    ),
    provenance = provenance, started_at = now, completed_at = now
  )

  expect_identical(result$units$reason_codes, list("entity_failed"))
  expect_identical(result$errors[[1L]]$code, "deflation_na")
})

test_that("new stage results use v2 final evidence provenance", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  final_identity <- list(
    filename = "manifest-v1-9.rds", uuid = "u9", checksum = "c9",
    generation = 9
  )
  units <- data.table::data.table(
    stage = "clean", entity_id = "survey", survey_id = "survey",
    pip_id = NA_character_, status = "success", action = "rebuild",
    reason_codes = list("dlw_changed"), input_hash = "input",
    output_hash = "clean-set", started_at = now, completed_at = now
  )
  artifacts <- data.table::data.table(
    entity_id = "survey", alias = "pip", artifact = c("P1", "P2"),
    path = c("p1.qs2", "p2.qs2"), version_id = c("v1", "v2"),
    content_hash = c("h1", "h2"), role = "primary",
    manifest_generation = 9
  )
  context <- list(
    run_id = "run", release = "20260831", identity = "TEST",
    dependency = list(
      scope_id = "scope", context_hash = "context", plan_hash = "clean-plan"
    )
  )
  provenance <- list(
    release = "20260831", identity = "TEST", scope_id = "scope",
    context_hash = "context", plan_hash = "clean-plan",
    manifest_before = NULL,
    manifest_after = list(
      filename = "manifest-v1-2.rds", uuid = "u2", checksum = "c2",
      generation = 2
    ),
    checkpoint_generations = 2,
    final_evidence_manifest = final_identity,
    stage_reason_codes = "dlw_changed"
  )

  result <- new_pipdata_stage_result(
    context, "clean", FALSE, units, artifacts,
    log_ref = list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = "process_summary_inf", log_checkpoint = NULL
    ),
    provenance = provenance, started_at = now, completed_at = now
  )

  expect_identical(result$schema_version, 2L)
  expect_identical(
    names(result$provenance),
    c(
      "release", "identity", "scope_id", "context_hash", "plan_hash",
      "manifest_before", "manifest_after", "checkpoint_generations",
      "final_evidence_manifest", "stage_reason_codes"
    )
  )
  expect_identical(result$artifacts$manifest_generation, c(9, 9))
  expect_no_error(validate_pipdata_stage_result(result, context))

  wrong_wave <- result
  wrong_wave$provenance$checkpoint_generations <- c(2, 3)
  expect_error(
    validate_pipdata_stage_result(wrong_wave, context),
    class = "pipdata_stage_result_invalid"
  )

  missing_final <- result
  missing_final$provenance$final_evidence_manifest <- NULL
  expect_error(
    validate_pipdata_stage_result(missing_final, context),
    class = "pipdata_stage_result_invalid"
  )
})

test_that("stage result validator retains schema v1 compatibility", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  identity <- list(filename = "m", uuid = "u", checksum = "c", generation = 2)
  context <- list(
    run_id = "run", release = "20260831", identity = "TEST",
    dependency = list(scope_id = "scope", context_hash = "context",
                      plan_hash = "plan")
  )
  units <- data.table::data.table(
    stage = "deflate", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    status = "success", action = "refresh",
    reason_codes = list("aux_cpi_changed"), input_hash = "input",
    output_hash = "output", started_at = now, completed_at = now
  )
  artifacts <- data.table::data.table(
    entity_id = "P1", alias = "pip_deflated", artifact = "P1",
    path = "p.qs2", version_id = "v1", content_hash = "output",
    role = "primary", manifest_generation = 2
  )
  provenance <- list(
    release = "20260831", identity = "TEST", scope_id = "scope",
    context_hash = "context", plan_hash = "plan", manifest_before = NULL,
    manifest_after = identity, checkpoint_generations = 2,
    final_evidence_manifest = identity,
    stage_reason_codes = "aux_cpi_changed"
  )
  result <- new_pipdata_stage_result(
    context, "deflate", FALSE, units, artifacts,
    log_ref = list(name = "pipdata_log", run_id = "run",
                   summary_discriminator = "deflate_summary_inf",
                   log_checkpoint = NULL),
    provenance = provenance, started_at = now, completed_at = now
  )
  legacy <- unclass(result)
  legacy$schema_version <- 1L
  legacy$provenance$final_evidence_manifest <- NULL

  expect_no_error(validate_pipdata_stage_result(legacy, context))
})

test_that("V21 public multi-wave results bind exact final receipts", {
  fixture <- c4_pipeline_fixture()
  run <- c4_pipeline_run(
    fixture, force = TRUE, checkpoint_size = 1L,
    checkpoint_seconds = Inf
  )
  final_manifest <- pd_manifest_read(fixture$context, fixture$root)
  final_identity <- attr(final_manifest, "manifest_identity")
  clean_after <- run$result$stage_results$clean$provenance$manifest_after

  expect_gt(final_identity$generation - clean_after$generation, 3)
  expect_identical(run$result$manifest_after, final_identity)

  receipt_fields <- c(
    "alias", "artifact", "path", "version_id", "content_hash"
  )
  all_artifacts <- data.table::data.table()
  for (stage in .PD_STAGES) {
    stage_result <- run$result$stage_results[[stage]]
    expect_no_error(validate_pipdata_stage_result(stage_result))
    expect_true(all(
      stage_result$artifacts$manifest_generation == final_identity$generation
    ))
    for (i in seq_len(nrow(stage_result$artifacts))) {
      artifact <- stage_result$artifacts[i]
      retained <- pd_committed_output_receipt(
        final_manifest, stage, artifact$entity_id[[1L]],
        artifact$artifact[[1L]]
      )
      expect_identical(
        unname(unlist(retained[receipt_fields])),
        unname(unlist(as.list(artifact[, ..receipt_fields])))
      )
    }
    tagged <- data.table::copy(stage_result$artifacts)
    tagged[, stage := stage]
    all_artifacts <- data.table::rbindlist(
      list(all_artifacts, tagged), fill = TRUE
    )
  }

  finalized <- list(execution = list(
    manifest = final_manifest, manifest_identity = final_identity
  ))
  sample <- all_artifacts[1L]
  for (field in receipt_fields) {
    mutated <- as.list(sample[, ..receipt_fields])
    mutated[[field]] <- paste0(mutated[[field]], "-mutated")
    mutated$success <- TRUE
    expect_error(
      new_artifact_reference(
        mutated, finalized, sample$stage[[1L]], sample$entity_id[[1L]]
      ),
      class = "pipdata_stage_result_invalid",
      info = field
    )
  }
})
