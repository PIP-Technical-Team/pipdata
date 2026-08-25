pd_metadata_refresh <- function(metadata, aux_projection, pip_id) {
  metadata <- pd_normalize_metadata_keys(metadata)
  aux_projection <- pd_normalize_metadata_keys(aux_projection)
  allowed <- c("cpi", "ppp", "pop", "gdp", "pce")
  aux <- aux_projection[intersect(names(aux_projection), allowed)]
  result <- metadata
  for (name in names(aux)) result[[name]] <- aux[[name]]
  attr(result, "pip_id") <- pip_id
  result
}

pd_execute_metadata <- function(action, snapshot, execution,
                                clean_result = NULL, verbose = FALSE) {
  pip_id <- action$pip_id[[1L]]
  if (!is.null(clean_result)) {
    if (!isTRUE(clean_result$success) ||
        !pip_id %in% clean_result$receipts$pip_id) {
      return(list(stage = "metadata", pip_id = pip_id, success = FALSE,
                  error = "upstream_clean_failed"))
    }
    metadata <- clean_result$metadata[[pip_id]]
  } else if (isTRUE((action$reconstruct_base_metadata %||% FALSE)[[1L]])) {
    required <- c("data_version_id", "data_hash")
    if (!all(required %in% names(action)) ||
        anyNA(unlist(action[, required, with = FALSE]))) {
      rlang::abort("Metadata reconstruction lacks an exact clean receipt.",
                   class = "pipdata_metadata_action_invalid")
    }
    clean <- pipload::load_pip_data(
      pip_id, version = action$data_version_id[[1L]],
      alias = "pip", verbose = FALSE
    )
    if (!identical(stamp::st_hash_obj(clean), action$data_hash[[1L]])) {
      rlang::abort("Pinned cleaned artifact failed hash verification.",
                   class = "pipdata_metadata_base_invalid")
    }
    metadata <- pd_aux_attr(
      stats::setNames(list(clean), pip_id), snapshot$aux$objects
    )[[pip_id]]
  } else {
    required <- c("metadata_version_id", "metadata_hash")
    if (!all(required %in% names(action)) ||
        anyNA(unlist(action[, required, with = FALSE]))) {
      rlang::abort("Metadata action lacks an exact base receipt.",
                   class = "pipdata_metadata_action_invalid")
    }
    metadata <- pipload::load_pip_data(
      pip_id, version = action$metadata_version_id[[1L]],
      alias = "pip_meta", verbose = FALSE
    )
    if (!identical(stamp::st_hash_obj(metadata), action$metadata_hash[[1L]])) {
      rlang::abort("Pinned metadata failed hash verification.",
                   class = "pipdata_metadata_base_invalid")
    }
  }
  projection <- action$aux_projection[[1L]] %||% list()
  refreshed <- pd_metadata_refresh(metadata, projection, pip_id)
  pd_assert_execution_fence(execution)
  receipt <- pd_save_receipt(refreshed, pip_id, "pip_meta", verbose,
                             execution$lease)
  actual_data_version <- if (!is.null(clean_result)) {
    clean_result$receipts[pip_id == ..pip_id, version_id][1L]
  } else action$data_version_id[[1L]]
  actual_data_hash <- if (!is.null(clean_result)) {
    clean_result$receipts[pip_id == ..pip_id, content_hash][1L]
  } else action$data_hash[[1L]]
  input_hash <- if (!is.null(clean_result)) {
    pd_hash_object(list(actual_data_version, actual_data_hash, projection))
  } else action$input_hash[[1L]]
  c(list(stage = "metadata", pip_id = pip_id,
         input_hash = input_hash,
         code_hash = action$code_hash[[1L]],
         data_version_id = actual_data_version,
         data_hash = actual_data_hash), receipt)
}
