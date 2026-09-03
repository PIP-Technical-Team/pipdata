pd_manifest_dir <- function(context,
                            root = getOption("pipdata.dependency_manifest_path")) {
  fs::path(root, "dependency-manifest", context$scope_id)
}

pd_manifest_files <- function(context, root = getOption("pipdata.dependency_manifest_path")) {
  dir <- pd_manifest_dir(context, root)
  if (!fs::dir_exists(dir)) return(character())
  files <- fs::dir_ls(dir, regexp = "manifest-v1-[0-9]{20}-[^/]+[.]rds$")
  files[order(pd_manifest_generation(files), decreasing = TRUE)]
}

pd_manifest_generation <- function(path) {
  as.numeric(sub("^manifest-v1-([0-9]{20})-.*$", "\\1", fs::path_file(path)))
}

pd_manifest_identity <- function(envelope, path) {
  list(filename = fs::path_file(path), uuid = envelope$uuid,
       checksum = envelope$checksum, generation = envelope$generation)
}

pd_manifest_read <- function(context, root = getOption("pipdata.dependency_manifest_path"),
                             allow_absent = TRUE) {
  files <- pd_manifest_files(context, root)
  if (!length(files)) {
    if (allow_absent) return(structure(list(), class = "pipdata_manifest_absent"))
    rlang::abort("Dependency manifest is absent.", class = "pipdata_manifest_absent")
  }
  for (path in files) {
    envelope <- tryCatch(readRDS(path), error = function(e) NULL)
    valid <- !is.null(envelope) && is.list(envelope) &&
      identical(envelope$checksum,
                pd_hash_object(envelope$payload, algo = "sha256"))
    if (valid) {
      valid <- tryCatch({
        pd_validate_manifest(envelope$payload, context)
        TRUE
      }, error = function(e) FALSE)
    }
    if (valid) {
      attr(envelope$payload, "manifest_identity") <- pd_manifest_identity(envelope, path)
      return(envelope$payload)
    }
  }
  rlang::abort("No valid dependency manifest generation remains.",
               class = "pipdata_manifest_corrupt")
}

pd_random_id <- function() {
  paste0(sprintf("%08x", sample.int(.Machine$integer.max, 4L)), collapse = "")
}

pd_lease_acquire <- function(context, root = getOption("pipdata.dependency_manifest_path"),
                             run_id = pd_random_id()) {
  dir <- pd_manifest_dir(context, root)
  fs::dir_create(dir, recurse = TRUE)
  lease_dir <- fs::path(dir, "writer.lease")
  if (!dir.create(lease_dir, showWarnings = FALSE)) {
    rlang::abort("Dependency manifest writer lease is held.",
                 class = "pipdata_manifest_lease_held")
  }
  lease <- list(host = Sys.info()[["nodename"]], pid = Sys.getpid(), run_id = run_id,
                heartbeat = format(Sys.time(), tz = "UTC", usetz = TRUE),
                token = pd_random_id(), path = lease_dir)
  saveRDS(lease, fs::path(lease_dir, "owner.rds"), version = 3L)
  lease
}

pd_lease_assert <- function(lease) {
  owner_path <- fs::path(lease$path, "owner.rds")
  owner <- if (fs::file_exists(owner_path)) readRDS(owner_path) else NULL
  if (is.null(owner) || !identical(owner$token, lease$token)) {
    rlang::abort("Dependency manifest writer lease was lost.",
                 class = "pipdata_manifest_lease_lost")
  }
  invisible(lease)
}

pd_lease_heartbeat <- function(lease) {
  pd_lease_assert(lease)
  lease$heartbeat <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  tmp <- fs::path(lease$path, paste0("owner-", lease$token, ".tmp"))
  saveRDS(lease, tmp, version = 3L)
  pd_lease_assert(lease)
  if (!file.rename(tmp, fs::path(lease$path, "owner.rds"))) {
    rlang::abort("Lease heartbeat publication failed.",
                 class = "pipdata_manifest_lease_lost")
  }
  lease
}

pd_lease_recover_offline <- function(context,
                                     root = getOption("pipdata.dependency_manifest_path"),
                                     confirm_offline = FALSE) {
  if (!isTRUE(confirm_offline)) {
    rlang::abort("Lease recovery requires explicit offline confirmation.",
                 class = "pipdata_manifest_recovery_confirmation")
  }
  lease_dir <- fs::path(pd_manifest_dir(context, root), "writer.lease")
  if (!fs::dir_exists(lease_dir)) return(invisible(NULL))
  owner_path <- fs::path(lease_dir, "owner.rds")
  owner <- if (fs::file_exists(owner_path)) readRDS(owner_path) else NULL
  if (is.null(owner) || !identical(owner$host, Sys.info()[["nodename"]])) {
    rlang::abort("Owner death cannot be demonstrated on this host.",
                 class = "pipdata_manifest_recovery_owner_unknown")
  }
  alive <- pd_pid_alive(owner$pid)
  if (alive) {
    rlang::abort("The same-host lease owner process is still alive.",
                 class = "pipdata_manifest_recovery_owner_alive")
  }
  quarantine <- fs::path(pd_manifest_dir(context, root),
                         paste0("writer.lease.quarantine-", pd_random_id()))
  if (!file.rename(lease_dir, quarantine)) {
    rlang::abort("The prior lease could not be quarantined atomically.",
                 class = "pipdata_manifest_recovery_error")
  }
  invisible(quarantine)
}

pd_pid_alive <- function(pid) {
  if (!is.numeric(pid) || length(pid) != 1L || is.na(pid) || pid <= 0) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    output <- tryCatch(
      suppressWarnings(system2(
        "tasklist", c("/FI", shQuote(sprintf("PID eq %d", as.integer(pid))),
                      "/NH"), stdout = TRUE, stderr = FALSE
      )),
      error = function(e) character()
    )
    return(any(grepl(sprintf("[[:space:]]%d[[:space:]]", as.integer(pid)),
                     output)))
  }
  tryCatch({
    tools::pskill(pid, signal = 0L)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

pd_lease_release <- function(lease) {
  if (fs::dir_exists(lease$path)) {
    pd_lease_assert(lease)
    fs::dir_delete(lease$path)
  }
  invisible(NULL)
}

pd_manifest_publish <- function(payload, context, lease,
                                root = getOption("pipdata.dependency_manifest_path"),
                                parent = NULL) {
  pd_lease_assert(lease)
  pd_validate_manifest(payload, context)
  current <- pd_manifest_read(context, root, allow_absent = TRUE)
  current_identity <- if (inherits(current, "pipdata_manifest_absent")) NULL else
    attr(current, "manifest_identity")
  if (!identical(current_identity, parent)) {
    rlang::abort("Manifest parent changed before publication.",
                 class = "pipdata_manifest_parent_changed")
  }
  files <- pd_manifest_files(context, root)
  generation <- if (length(files)) max(pd_manifest_generation(files)) + 1 else 1
  uuid <- pd_random_id()
  envelope <- list(schema_version = .PD_DEPENDENCY_SCHEMA, generation = generation,
                   uuid = uuid, parent = parent, payload = payload,
                   checksum = pd_hash_object(payload, algo = "sha256"))
  dir <- pd_manifest_dir(context, root)
  tmp <- fs::path(dir, paste0(".manifest-", uuid, ".tmp"))
  final <- fs::path(dir, sprintf("manifest-v1-%020.0f-%s.rds", generation, uuid))
  saveRDS(envelope, tmp, version = 3L)
  verify <- readRDS(tmp)
  if (!identical(verify$checksum, pd_hash_object(verify$payload, algo = "sha256"))) {
    rlang::abort("Manifest temporary write failed verification.",
                 class = "pipdata_manifest_write_error")
  }
  pd_lease_assert(lease)
  if (!file.rename(tmp, final)) {
    rlang::abort("Manifest generation publication failed.",
                 class = "pipdata_manifest_write_error")
  }
  ownership <- tryCatch({
    pd_lease_assert(lease)
    TRUE
  }, error = function(e) FALSE)
  if (!ownership) {
    quarantine <- fs::path(dir, paste0(".quarantine-", fs::path_file(final)))
    file.rename(final, quarantine)
    rlang::abort("Lease ownership changed during manifest publication.",
                 class = "pipdata_manifest_lease_lost")
  }
  published <- pd_manifest_read(context, root, allow_absent = FALSE)
  pd_manifest_cleanup(context, root, keep = 3L)
  published
}

pd_manifest_cleanup <- function(context,
                                root = getOption("pipdata.dependency_manifest_path"),
                                keep = 3L) {
  files <- pd_manifest_files(context, root)
  valid <- character()
  for (path in files) {
    envelope <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(envelope) && identical(
      envelope$checksum, pd_hash_object(envelope$payload, algo = "sha256")
    )) valid <- c(valid, path)
  }
  remove <- setdiff(valid, utils::head(valid, max(3L, keep)))
  for (path in remove) try(fs::file_delete(path), silent = TRUE)
  invisible(remove)
}

pd_checkpoint <- function(master, stage, results, context, lease, manifest,
                          release_writer, master_writer, manifest_root = NULL,
                          survey_id = NULL, expected_pip_ids = NULL) {
  execution <- list(context = context, lease = lease, manifest = manifest,
                    manifest_identity = attr(manifest, "manifest_identity"))
  finalized <- pd_finalize_checkpoint(
    execution, master, stage, results, release_writer, master_writer,
    manifest_root, survey_id, expected_pip_ids
  )
  finalized$candidate
}

pd_checkpoint_named_inputs <- function(execution, stage, results, entities) {
  current <- execution$snapshot$current %||% data.table::data.table()
  if (!nrow(current) || !"input_rows" %in% names(current)) {
    return(NULL)
  }
  selected_stage <- stage
  output <- list()
  for (entity_id in sort(unique(entities))) {
    selected_entity <- entity_id
    accepted <- current[
      which(
        current$stage == selected_stage &
          current$entity_id == selected_entity
      )
    ]
    if (nrow(accepted) != 1L || !length(accepted$input_rows[[1L]])) {
      rlang::abort(
        "Accepted named input components are missing or ambiguous.",
        class = "pipdata_checkpoint_provenance_error"
      )
    }
    components <- data.table::as.data.table(
      data.table::copy(accepted$input_rows[[1L]])
    )[name != "canonical", .(name, version_id, content_hash)]
    result_rows <- if (identical(stage, "clean")) {
      results
    } else {
      results[which(results$pip_id == selected_entity)]
    }
    if (identical(stage, "metadata")) {
      required <- c("data_version_id", "data_hash")
      if (nrow(result_rows) != 1L ||
          !all(required %in% names(result_rows)) ||
          anyNA(result_rows[, ..required]) ||
          any(!nzchar(unlist(result_rows[, ..required])))) {
        rlang::abort(
          "Final metadata inputs lack an exact clean receipt.",
          class = "pipdata_checkpoint_provenance_error"
        )
      }
      components[name == "clean_data", `:=`(
        version_id = result_rows$data_version_id[[1L]],
        content_hash = result_rows$data_hash[[1L]]
      )]
    } else if (identical(stage, "deflate")) {
      upstream <- list(
        clean_data = c("data_version_id", "data_hash"),
        metadata = c("metadata_version_id", "metadata_hash")
      )
      if (nrow(result_rows) != 1L) {
        rlang::abort(
          "Final deflate inputs are ambiguous.",
          class = "pipdata_checkpoint_provenance_error"
        )
      }
      for (name in names(upstream)) {
        selected_name <- name
        fields <- upstream[[name]]
        if (!fields[[1L]] %in% names(result_rows) ||
            is.na(result_rows[[fields[[1L]]]][[1L]]) ||
            !nzchar(result_rows[[fields[[1L]]]][[1L]])) {
          rlang::abort(
            "Final deflate inputs lack exact upstream versions.",
            class = "pipdata_checkpoint_provenance_error"
          )
        }
        accepted_version <- components[
          which(components$name == selected_name), version_id
        ][[1L]]
        final_version <- result_rows[[fields[[1L]]]][[1L]]
        if (fields[[2L]] %in% names(result_rows) &&
            !is.na(result_rows[[fields[[2L]]]][[1L]]) &&
            nzchar(result_rows[[fields[[2L]]]][[1L]])) {
          components[which(components$name == selected_name), `:=`(
            version_id = final_version,
            content_hash = result_rows[[fields[[2L]]]][[1L]]
          )]
        } else if (!identical(accepted_version, final_version)) {
          rlang::abort(
            "Committed upstream version differs from its accepted hash.",
            class = "pipdata_checkpoint_provenance_error"
          )
        }
      }
    }
    output[[length(output) + 1L]] <- pd_build_input_rows(
      stage, selected_entity, components
    )
  }
  return(data.table::rbindlist(output, use.names = TRUE))
}

pd_committed_output_receipt <- function(manifest, stage, entity_id, artifact) {
  selected_stage <- stage
  selected_entity <- entity_id
  record <- manifest$records[
    manifest$records$stage == selected_stage &
      manifest$records$entity_id == selected_entity
  ]
  if (nrow(record) != 1L) {
    return(NULL)
  }
  receipts <- record$output_receipts[[1L]]
  while (is.list(receipts) && length(receipts) == 1L &&
         is.list(receipts[[1L]]) && is.null(names(receipts[[1L]]))) {
    receipts <- receipts[[1L]]
  }
  if (is.list(receipts) && !is.null(names(receipts))) {
    receipts <- list(receipts)
  }
  matches <- Filter(function(receipt) {
    is.list(receipt) && identical(receipt$artifact, artifact)
  }, receipts)
  if (length(matches) != 1L) {
    return(NULL)
  }
  return(matches[[1L]])
}

pd_assert_checkpoint_provenance <- function(execution, master, stage, results,
                                            named_inputs) {
  if (is.null(named_inputs)) {
    return(invisible(NULL))
  }
  pd_validate_manifest(execution$manifest, execution$context)
  summary <- data.table::as.data.table(
    execution$snapshot$fingerprints$summary %||% data.table::data.table()
  )
  selected_stage <- stage
  expected_code <- if (all(c("stage", "hash") %in% names(summary))) {
    summary[summary$stage == selected_stage][["hash"]]
  } else {
    character()
  }
  if (length(expected_code) != 1L || is.na(expected_code) ||
      !nzchar(expected_code) ||
      any(results$code_hash != expected_code)) {
    rlang::abort(
      "Checkpoint code provenance differs from the accepted fingerprint.",
      class = "pipdata_checkpoint_provenance_error"
    )
  }
  if (identical(stage, "clean")) {
    return(invisible(NULL))
  }
  master <- data.table::as.data.table(data.table::copy(master))
  upstream <- if (identical(stage, "metadata")) {
    list(clean_data = c("clean", "data_version_id", "data_hash"))
  } else {
    list(
      clean_data = c("clean", "data_version_id", "data_hash"),
      metadata = c("metadata", "metadata_version_id", "metadata_hash")
    )
  }
  for (i in seq_len(nrow(results))) {
    pip_id <- results$pip_id[[i]]
    selected_pip_id <- pip_id
    owner <- master[master$pip_id == selected_pip_id, survey_id]
    if (length(owner) != 1L || is.na(owner) || !nzchar(owner)) {
      rlang::abort(
        "Checkpoint upstream ownership is missing or ambiguous.",
        class = "pipdata_checkpoint_provenance_error"
      )
    }
    for (name in names(upstream)) {
      fields <- upstream[[name]]
      entity_id <- if (identical(fields[[1L]], "clean")) owner else pip_id
      receipt <- pd_committed_output_receipt(
        execution$manifest, fields[[1L]], entity_id, pip_id
      )
      version <- results[[fields[[2L]]]][[i]] %||% NA_character_
      hash <- results[[fields[[3L]]]][[i]] %||% NA_character_
      if (is.null(receipt) || is.na(version) || !nzchar(version) ||
          is.na(hash) || !nzchar(hash) ||
          !identical(version, receipt$version_id) ||
          !identical(hash, receipt$content_hash)) {
        rlang::abort(
          paste("Checkpoint", name, "does not match its committed receipt."),
          class = "pipdata_checkpoint_provenance_error"
        )
      }
    }
  }
  return(invisible(NULL))
}

pd_release_inventory_candidate <- function(candidate) {
  release_candidate <- data.table::copy(data.table::as.data.table(candidate))
  if ("latest_release_version_id" %in% names(release_candidate)) {
    release_candidate[, latest_release_version_id := NA_character_]
  }
  release_candidate
}

pd_inventory_replay_current <- function(execution, master, candidate) {
  if (!identical(
    pd_canonical_snapshot_table(candidate),
    pd_canonical_snapshot_table(master)
  )) {
    return(FALSE)
  }
  master <- data.table::as.data.table(master)
  if (!"latest_release_version_id" %in% names(master)) {
    return(FALSE)
  }
  versions <- unique(master$latest_release_version_id)
  versions <- versions[!is.na(versions) & nzchar(versions)]
  if (length(versions) != 1L) {
    return(FALSE)
  }
  catalog <- data.table::as.data.table(
    execution$snapshot$catalogs$pip_inv %||% data.table::data.table()
  )
  all(c("version_id", "content_hash", "path") %in% names(catalog)) &&
    nrow(catalog[
      version_id == versions[[1L]] & !is.na(content_hash) &
        nzchar(content_hash) & !is.na(path) & nzchar(path)
    ]) == 1L
}

pd_finalize_checkpoint <- function(execution, master, stage, results,
                                   release_writer, master_writer,
                                   manifest_root = NULL, survey_id = NULL,
                                   expected_pip_ids = NULL) {
  results <- data.table::as.data.table(data.table::copy(results))
  advanced_receipts <- results
  assert_fence <- function() {
    if (!is.null(execution$snapshot)) {
      fence <- pd_assert_execution_fence
      if ("advanced_receipts" %in% names(formals(fence))) {
        fence(execution, advanced_receipts)
      } else {
        fence(execution)
      }
    } else {
      pd_lease_assert(execution$lease)
    }
  }
  assert_fence()
  if (!nrow(results) || any(!results$success)) {
    rlang::abort("Checkpoint contains an unverified stage result.",
                 class = "pipdata_checkpoint_unverified")
  }
  receipt_columns <- c("alias", "path", "version_id", "content_hash", "success")
  if (all(receipt_columns %in% names(results))) {
    for (i in seq_len(nrow(results))) {
      pd_revalidate_receipt(as.list(results[i]))
    }
  }
  if (!all(c("input_hash", "code_hash") %in% names(results)) ||
      anyNA(results[, .(input_hash, code_hash)])) {
    rlang::abort("Checkpoint results lack complete input/code provenance.",
                 class = "pipdata_checkpoint_provenance_error")
  }
  entity <- if (identical(stage, "clean")) {
    rep(survey_id, nrow(results))
  } else {
    results$pip_id
  }
  named_inputs <- pd_checkpoint_named_inputs(
    execution, stage, results, unique(entity)
  )
  pd_assert_checkpoint_provenance(
    execution, master, stage, results, named_inputs
  )
  receipt_set <- if (identical(stage, "clean")) {
    pd_clean_receipt_set(results, expected_pip_ids)
  } else {
    NULL
  }
  reconciliation <- pd_reconcile_inventory(
    master, stage, results, survey_id, expected_pip_ids
  )
  if (!isTRUE(reconciliation$success)) {
    rlang::abort("Stage reconciliation rejected the checkpoint.",
                 class = "pipdata_checkpoint_reconciliation_error",
                 reason = reconciliation$reason)
  }
  candidate <- reconciliation$candidate
  inventory_changed <- !pd_inventory_replay_current(
    execution, master, candidate
  )
  release_receipt <- NULL
  master_receipt <- NULL
  if (inventory_changed) {
    assert_fence()
    release_receipt <- release_writer(
      pd_release_inventory_candidate(candidate), execution$lease
    )
    if (!isTRUE(release_receipt$success)) {
      rlang::abort("Release inventory verification failed.",
                   class = "pipdata_checkpoint_release_error")
    }
    if (all(c("alias", "path", "content_hash") %in% names(release_receipt))) {
      pd_revalidate_receipt(release_receipt)
    }
    advanced_receipts <- data.table::rbindlist(list(
      advanced_receipts,
      data.table::as.data.table(release_receipt)
    ), fill = TRUE)
    candidate[, latest_release_version_id := release_receipt$version_id]
    assert_fence()
    master_receipt <- master_writer(candidate, execution$lease)
    if (!isTRUE(master_receipt$success)) {
      rlang::abort("Master inventory verification failed.",
                   class = "pipdata_checkpoint_master_error")
    }
    if (all(c("alias", "path", "content_hash") %in% names(master_receipt))) {
      pd_revalidate_receipt(master_receipt)
    }
    advanced_receipts <- data.table::rbindlist(list(
      advanced_receipts,
      data.table::as.data.table(master_receipt)
    ), fill = TRUE)
  }
  if (all(receipt_columns %in% names(results))) {
    for (i in seq_len(nrow(results))) {
      pd_revalidate_receipt(as.list(results[i]))
    }
  }
  assert_fence()
  has_exact_receipt <- all(c("alias", "path") %in% names(results))
  records <- results[, .(
    stage = ..stage, entity_id = entity,
    output_version_id = version_id, output_hash = content_hash,
    input_hash, code_hash, output_receipts = lapply(seq_len(.N), function(i) {
      if (!has_exact_receipt) return(list())
      artifact_id <- if ("artifact" %in% names(results)) artifact[i] else pip_id[i]
      list(alias = alias[i], artifact = artifact_id, path = path[i],
           version_id = version_id[i], content_hash = content_hash[i])
    })
  )]
  if (identical(stage, "clean")) {
    records <- records[, .(
      output_version_id = receipt_set$output_version_id,
      output_hash = receipt_set$output_hash,
      input_hash = .SD$input_hash[1L], code_hash = .SD$code_hash[1L],
      output_receipts = list(lapply(seq_len(nrow(receipt_set$receipts)), function(i) {
        as.list(receipt_set$receipts[i, .(
          alias, artifact, path, version_id, content_hash
        )])
      }))
    ), by = .(stage, entity_id)]
  }
  if (!is.null(named_inputs)) {
    canonical <- named_inputs[name == "canonical",
                              .(stage, entity_id, content_hash)]
    records[canonical, on = c("stage", "entity_id"),
            input_hash := i.content_hash]
  }
  manifest <- execution$manifest
  manifest$records <- manifest$records[!
    paste(stage, entity_id) %in% paste(records$stage, records$entity_id)]
  manifest$records <- data.table::rbindlist(list(manifest$records, records), fill = TRUE)
  affected <- unique(records[, .(stage, entity_id)])
  input_version <- function(i) {
    if (identical(stage, "metadata") && "data_version_id" %in% names(results)) {
      return(results$data_version_id[i])
    }
    if (identical(stage, "deflate") &&
        all(c("data_version_id", "metadata_version_id") %in% names(results))) {
      return(pd_hash_object(list(
        results$data_version_id[i], results$metadata_version_id[i]
      )))
    }
    results$input_hash[i]
  }
  new_inputs <- if (!is.null(named_inputs)) {
    named_inputs
  } else {
    fallback <- results[, .(
      stage = ..stage, entity_id = entity, name = "canonical",
      version_id = vapply(seq_len(.N), input_version, character(1)),
      content_hash = input_hash
    )]
    unique(fallback, by = c("stage", "entity_id"))
  }
  manifest$inputs <- manifest$inputs[!
    paste(stage, entity_id) %in% paste(affected$stage, affected$entity_id)]
  manifest$inputs <- data.table::rbindlist(list(manifest$inputs, new_inputs), fill = TRUE)
  if (!is.null(execution$snapshot$fingerprints$components)) {
    checkpoint_stage <- as.character(stage)[[1L]]
    current_fingerprints <- data.table::copy(
      execution$snapshot$fingerprints$components
    )
    committed_fingerprints <- current_fingerprints[
      which(current_fingerprints[["stage"]] == checkpoint_stage)
    ]
    manifest$fingerprints <- manifest$fingerprints[
      which(manifest$fingerprints[["stage"]] != checkpoint_stage)
    ]
    manifest$fingerprints <- data.table::rbindlist(
      list(manifest$fingerprints, committed_fingerprints), fill = TRUE
    )
    data.table::setorder(manifest$fingerprints, stage, component)
  }
  manifest$tombstones <- data.table::rbindlist(
    list(manifest$tombstones, reconciliation$tombstones), fill = TRUE
  )
  parent <- execution$manifest_identity
  root <- manifest_root %||% getOption("pipdata.dependency_manifest_path")
  assert_fence()
  published <- pd_manifest_publish(
    manifest, execution$context, execution$lease, root, parent
  )
  execution$manifest <- published
  execution$manifest_identity <- attr(published, "manifest_identity")
  execution <- pd_advance_execution_state(
    execution, candidate, advanced_receipts
  )
  list(candidate = candidate, execution = execution,
       release_receipt = release_receipt, master_receipt = master_receipt)
}
