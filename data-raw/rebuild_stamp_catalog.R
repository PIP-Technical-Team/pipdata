# rebuild_stamp_catalog.R
# ---------------------------------------------------------------------------
# One-off repair: regenerate a corrupt / truncated stamp `catalog.qs2` by
# scanning the on-disk artifacts and their per-version sidecars.
#
# WHY THIS EXISTS
# ---------------
# An interrupted qs2 write (e.g. an OOM session abort during pd_process_data)
# can truncate `<alias>/.stamp/catalog.qs2`. After that, pip_read() ->
# st_versions() -> .st_catalog_read() -> qs2::qs_read() fails with a
# "Decompression error", which breaks reads for EVERY artifact under that
# alias (the catalog is a single shared index file).
#
# The catalog is pure *derived* metadata: each artifact still keeps its real
# data file plus a full version history on disk:
#
#   <root>/<ID>.qs2/                         <- artifact "directory"
#       <ID>.qs2                             <- current data file (read on load)
#       stmeta/<ID>.qs2.stmeta.json          <- current sidecar
#       versions/<version_id>/artifact       <- per-version data snapshot
#       versions/<version_id>/sidecar.json   <- per-version metadata
#       versions/<version_id>/parents.json   <- per-version lineage (optional)
#
# So the catalog can be rebuilt without recomputing a single survey.
#
# This script reuses stamp's OWN path/hash helpers (via `:::`) so the
# `artifact_id` it writes is guaranteed to match what st_versions() computes
# at read time. It does NOT recompute version ids — the version directory name
# *is* the version_id.
#
# USAGE (interactive R, after the working release is set up)
# ---------------------------------------------------------
#   pipfun::setup_working_release("<release>")   # registers the "pip_meta" alias
#   source("data-raw/rebuild_stamp_catalog.R")
#
#   rebuild_stamp_catalog(alias = "pip_meta")                 # dry run (default)
#   rebuild_stamp_catalog(alias = "pip_meta", dry_run = FALSE)# commit the rebuild
#
#   # verify:
#   pipload::pip_read(id = "BOL_2022_EH_INC_ALL", alias = "pip_meta")
#
# NOTE: run this only when no pipeline process is writing to the same alias.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}

# Retry a file op a few times to ride out transient SMB close latency.
# NOTE: this does NOT defeat a genuine lock (another process or a leaked qs_read
# handle in this R session). If it still fails after the retries, the file is
# really held open -- restart R and/or close the holding process.
.retry_fs <- function(expr, tries = 5L, wait = 1.0) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({ force(expr); TRUE },
                   error = function(e) {
                     if (i == tries) stop(e)
                     Sys.sleep(wait)
                     FALSE
                   })
    if (ok) return(invisible(TRUE))
  }
  invisible(TRUE)
}

rebuild_stamp_catalog <- function(alias = "pip_meta", dry_run = TRUE) {
  for (pkg in c("stamp", "data.table", "fs", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }
  dt  <- data.table::data.table
  say <- function(...) cat(sprintf(...), "\n", sep = "")

  # Resolve the .stamp state dir + metadata root from the registered alias.
  # Mirrors exactly what stamp uses, so paths/aliases line up with reads.
  state_dir <- stamp:::.st_state_dir_abs(alias)
  if (!fs::dir_exists(state_dir)) {
    stop(sprintf(
      "State dir not found for alias '%s':\n  %s\nDid you run setup_working_release()?",
      alias, state_dir
    ), call. = FALSE)
  }
  meta_root    <- fs::path_dir(state_dir)
  catalog_path <- stamp:::.st_catalog_path(alias = alias)

  say("Alias        : %s", alias)
  say("Metadata root: %s", meta_root)
  say("Catalog file : %s", catalog_path)

  # Discover artifact directories: <root>/<ID>.qs2/ that contain versions/
  art_dirs <- fs::dir_ls(meta_root, type = "directory", glob = "*.qs2")
  art_dirs <- art_dirs[fs::dir_exists(fs::path(art_dirs, "versions"))]
  say("Artifacts    : %d directories with a versions/ folder", length(art_dirs))
  if (!length(art_dirs)) {
    stop("No artifact directories found; nothing to rebuild.", call. = FALSE)
  }

  cat0           <- stamp:::.st_catalog_empty()
  artifacts_rows <- list()
  versions_rows  <- list()
  parents_rows   <- list()
  skipped        <- character(0)

  for (d in art_dirs) {
    # Pass the relative path, exactly as a caller would pass `id`, then let
    # stamp normalize + hash it -> guarantees the same artifact_id as reads.
    rel  <- fs::path_rel(d, start = meta_root)
    norm <- tryCatch(
      stamp:::.st_normalize_user_path(
        rel, alias = alias, must_exist = FALSE, auto_switch = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(norm)) {
      skipped <- c(skipped, rel)
      next
    }
    lp  <- norm$logical_path
    aid <- stamp:::.st_artifact_id(lp)

    vrows <- list()
    fmt   <- "qs2"
    for (vd in fs::dir_ls(fs::path(d, "versions"), type = "directory")) {
      scf <- fs::path(vd, "sidecar.json")
      if (!fs::file_exists(scf)) next
      sc <- tryCatch(
        jsonlite::read_json(scf, simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (is.null(sc)) next

      created <- as.character(sc$created_at %||% NA_character_)
      # st_versions() drops rows with invalid created_at; skip them here too.
      if (is.na(created) || !nzchar(created)) next

      vid <- fs::path_file(vd)            # version directory name IS the version_id
      fmt <- as.character(sc$format %||% fmt)

      vrows[[length(vrows) + 1L]] <- dt(
        version_id     = vid,
        artifact_id    = aid,
        content_hash   = as.character(sc$content_hash %||% NA_character_),
        code_hash      = as.character(sc$code_hash    %||% NA_character_),
        size_bytes     = as.numeric(sc$size_bytes     %||% NA_real_),
        created_at     = created,
        sidecar_format = "json"
      )

      # Lineage (optional) -> parents_index
      if (fs::file_exists(fs::path(vd, "parents.json"))) {
        pars <- tryCatch(stamp:::.st_version_read_parents(vd),
                         error = function(e) list())
        for (p in pars) {
          parents_rows[[length(parents_rows) + 1L]] <- dt(
            parent_artifact_id = stamp:::.st_artifact_id(p$path),
            parent_version_id  = as.character(p$version_id),
            child_artifact_id  = aid,
            child_version_id   = vid
          )
        }
      }
    }

    if (!length(vrows)) {
      skipped <- c(skipped, rel)
      next
    }
    vdt <- data.table::rbindlist(vrows, use.names = TRUE, fill = TRUE)
    versions_rows[[length(versions_rows) + 1L]] <- vdt

    # latest = newest created_at (ISO8601 sorts lexically), tie-broken by id
    data.table::setorder(vdt, -created_at, -version_id)
    artifacts_rows[[length(artifacts_rows) + 1L]] <- dt(
      artifact_id       = aid,
      path              = lp,
      format            = fmt,
      latest_version_id = vdt$version_id[[1L]],
      n_versions        = nrow(vdt)
    )
  }

  cat_new <- cat0
  if (length(artifacts_rows)) {
    cat_new$artifacts <- data.table::rbindlist(
      c(list(cat0$artifacts), artifacts_rows), use.names = TRUE, fill = TRUE)
  }
  if (length(versions_rows)) {
    cat_new$versions <- data.table::rbindlist(
      c(list(cat0$versions), versions_rows), use.names = TRUE, fill = TRUE)
  }
  if (length(parents_rows)) {
    cat_new$parents_index <- data.table::rbindlist(
      c(list(cat0$parents_index), parents_rows), use.names = TRUE, fill = TRUE)
  }

  say("Rebuilt      : %d artifacts, %d versions, %d parent links",
      nrow(cat_new$artifacts), nrow(cat_new$versions),
      nrow(cat_new$parents_index))
  if (length(skipped)) {
    say("Skipped      : %d artifact(s) with no usable versions", length(skipped))
  }

  if (isTRUE(dry_run)) {
    say("DRY RUN -- nothing written. Re-run with dry_run = FALSE to commit.")
    return(invisible(cat_new))
  }

  # Commit with a verified-temp + atomic-rename strategy.
  #
  # We deliberately do NOT use stamp:::.st_catalog_write() here: on an SMB
  # share its delete-then-move sequence fails with [EBUSY] when the catalog
  # still has a lingering handle. We also avoid fs::file_copy() for the backup
  # (it opens the source for reading, and SMB's lazy close leaves that handle
  # alive long enough to make the next operation fail). Renaming is a pure
  # directory metadata op: it neither opens the file content nor needs a delete.
  tmp <- fs::file_temp(tmp_dir = fs::path_dir(catalog_path),
                       pattern = fs::path_file(catalog_path))
  stamp:::.st_write_qs2(cat_new, tmp)
  # Integrity gate: make sure the temp is actually readable before we swap.
  invisible(stamp:::.st_read_qs2(tmp))

  if (fs::file_exists(catalog_path)) {
    bak <- paste0(catalog_path, ".corrupt-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    # rename: backs up AND clears the path. If this EBUSYs, catalog.qs2 is held
    # open by another process or a leaked handle in this session -- restart R.
    .retry_fs(fs::file_move(catalog_path, bak))
    say("Backed up old catalog -> %s", bak)
  }
  .retry_fs(fs::file_move(tmp, catalog_path))  # atomic rename into place, no delete
  say("Wrote rebuilt catalog -> %s", catalog_path)

  invisible(cat_new)
}
