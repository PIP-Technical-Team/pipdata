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
