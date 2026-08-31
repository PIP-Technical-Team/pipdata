pd_reconcile_inventory <- function(master, stage, results, survey_id = NULL,
                                   expected_pip_ids = NULL) {
  out <- data.table::as.data.table(data.table::copy(master))
  results <- data.table::as.data.table(data.table::copy(results))
  fail <- function(reason) structure(
    list(success = FALSE, candidate = out,
         tombstones = data.table::data.table(), reason = reason),
    class = "pipdata_reconciliation"
  )
  if (!all(c("pip_id", "version_id", "content_hash", "success") %in% names(results)) ||
      !nrow(results) || any(!results$success) || anyNA(results[, .(pip_id, version_id, content_hash)]) ||
      anyDuplicated(results$pip_id)) return(fail("unverified_result"))
  if (stage == "clean") {
    expected <- tryCatch(
      pd_validate_expected_pip_ids(expected_pip_ids),
      error = function(e) character()
    )
    if (!length(expected) ||
        !identical(sort(results$pip_id), expected)) {
      return(fail("incomplete_output_set"))
    }
    selected_survey_id <- survey_id
    old <- out[get("survey_id") == selected_survey_id]
    out <- out[get("survey_id") != selected_survey_id]
    downstream <- c(
      "version_id_metadata", "content_hash_metadata", "path_metadata",
      "size_bytes_metadata", "created_at_metadata", "version_id_deflated",
      "content_hash_deflated", "path_deflated", "size_bytes_deflated",
      "created_at_deflated", "deflated", "aux_cpi_hash_at_deflation",
      "aux_ppp_hash_at_deflation", "aux_pop_hash_at_deflation",
      "latest_release_version_id"
    )
    receipt_cols <- c("alias", "artifact", "path", "version_id",
                      "content_hash", "skipped", "success", "error",
                      "input_hash", "code_hash", "stage")
    invariant_cols <- setdiff(names(results), c(receipt_cols, downstream))
    add <- results[, invariant_cols, with = FALSE]
    add[, `:=`(survey_id = selected_survey_id,
               version_id_data = results$version_id,
               content_hash_data = results$content_hash)]
    retained <- old[add, on = "pip_id"]
    for (column in setdiff(names(add), "pip_id")) {
      retained[, (column) := add[[column]]]
    }
    add <- retained
    for (column in intersect(downstream, names(add))) {
      value <- if (column == "deflated") FALSE else NA
      data.table::set(add, j = column, value = value)
    }
    tombstones <- old[!pip_id %in% results$pip_id,
                      .(survey_id, pip_id, reason = "output_removed")]
    out <- data.table::rbindlist(list(out, add), fill = TRUE)
  } else if (stage == "metadata") {
    if (any(!results$pip_id %in% out$pip_id)) return(fail("unknown_pip_id"))
    out[results, on = "pip_id", `:=`(
      version_id_metadata = i.version_id,
      content_hash_metadata = i.content_hash,
      version_id_deflated = NA_character_,
      content_hash_deflated = NA_character_,
      deflated = FALSE)]
  } else if (stage == "deflate") {
    if (any(!results$pip_id %in% out$pip_id)) return(fail("unknown_pip_id"))
    out[results, on = "pip_id", `:=`(
      version_id_deflated = i.version_id,
      content_hash_deflated = i.content_hash,
      deflated = TRUE)]
  } else {
    rlang::abort("Unknown reconciliation stage.", class = "pipdata_dependency_stage_error")
  }
  data.table::setorder(out, survey_id, pip_id)
  if (!exists("tombstones", inherits = FALSE)) {
    tombstones <- data.table::data.table()
  }
  structure(list(success = TRUE, candidate = out, tombstones = tombstones,
                 reason = NA_character_), class = "pipdata_reconciliation")
}
