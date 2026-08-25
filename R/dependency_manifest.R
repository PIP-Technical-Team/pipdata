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

pd_finalize_checkpoint <- function(execution, master, stage, results,
                                   release_writer, master_writer,
                                   manifest_root = NULL, survey_id = NULL,
                                   expected_pip_ids = NULL) {
  assert_fence <- function() {
    if (!is.null(execution$snapshot)) {
      pd_assert_execution_fence(execution)
    } else {
      pd_lease_assert(execution$lease)
    }
  }
  assert_fence()
  results <- data.table::as.data.table(data.table::copy(results))
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
  reconciliation <- pd_reconcile_inventory(
    master, stage, results, survey_id, expected_pip_ids
  )
  if (!isTRUE(reconciliation$success)) {
    rlang::abort("Stage reconciliation rejected the checkpoint.",
                 class = "pipdata_checkpoint_reconciliation_error",
                 reason = reconciliation$reason)
  }
  candidate <- reconciliation$candidate
  assert_fence()
  release_receipt <- release_writer(candidate, execution$lease)
  if (!isTRUE(release_receipt$success)) {
    rlang::abort("Release inventory verification failed.",
                 class = "pipdata_checkpoint_release_error")
  }
  if (all(c("alias", "path", "content_hash") %in% names(release_receipt))) {
    pd_revalidate_receipt(release_receipt)
  }
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
  if (all(receipt_columns %in% names(results))) {
    for (i in seq_len(nrow(results))) {
      pd_revalidate_receipt(as.list(results[i]))
    }
  }
  assert_fence()
  if (!all(c("input_hash", "code_hash") %in% names(results)) ||
      anyNA(results[, .(input_hash, code_hash)])) {
    rlang::abort("Checkpoint results lack complete input/code provenance.",
                 class = "pipdata_checkpoint_provenance_error")
  }
  entity <- if (identical(stage, "clean")) {
    rep(survey_id, nrow(results))
  } else results$pip_id
  has_exact_receipt <- all(c("alias", "path") %in% names(results))
  records <- results[, .(
    stage = ..stage, entity_id = entity,
    output_version_id = version_id, output_hash = content_hash,
    input_hash, code_hash, output_receipts = lapply(seq_len(.N), function(i) {
      if (!has_exact_receipt) return(list())
      list(alias = alias[i], artifact = pip_id[i], path = path[i],
           version_id = version_id[i], content_hash = content_hash[i])
    })
  )]
  if (identical(stage, "clean")) {
    records <- records[, .(
      output_version_id = pd_hash_object(sort(.SD$output_version_id)),
      output_hash = pd_hash_object(sort(.SD$output_hash)),
      input_hash = .SD$input_hash[1L], code_hash = .SD$code_hash[1L],
      output_receipts = list(unlist(.SD$output_receipts, recursive = FALSE))
    ), by = .(stage, entity_id)]
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
  new_inputs <- results[, .(
    stage = ..stage, entity_id = entity, name = "canonical",
    version_id = vapply(seq_len(.N), input_version, character(1)),
    content_hash = input_hash
  )]
  new_inputs <- unique(new_inputs, by = c("stage", "entity_id"))
  manifest$inputs <- manifest$inputs[!
    paste(stage, entity_id) %in% paste(affected$stage, affected$entity_id)]
  manifest$inputs <- data.table::rbindlist(list(manifest$inputs, new_inputs), fill = TRUE)
  if (!is.null(execution$snapshot$fingerprints$components)) {
    manifest$fingerprints <- data.table::copy(execution$snapshot$fingerprints$components)
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
  list(candidate = candidate, execution = execution,
       release_receipt = release_receipt, master_receipt = master_receipt)
}
