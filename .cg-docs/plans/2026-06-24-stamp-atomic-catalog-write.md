---
date: 2026-06-24
title: "stamp: crash-safe catalog/data writes + catalog self-heal"
status: draft
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small-medium"
deviation-policy: "ask"
target-repo: "stamp"
tags: [stamp, catalog, qs2, atomicity, durability, corruption, reliability]
note: "DRAFT written in pipdata for convenience — MOVE this file into the stamp repo before implementing. It describes changes to stamp/R only."
---

# Plan: stamp — Crash-Safe Catalog/Data Writes + Catalog Self-Heal

## Objective

Make `stamp`'s on-disk writes survive a process abort (OOM kill, Ctrl-C,
machine crash) without producing a **truncated/corrupt `catalog.qs2`** that
takes down reads for every artifact under an alias. Add a cheap recovery path
so a damaged catalog can be rebuilt from the surviving per-artifact version
data instead of forcing a full pipeline re-run.

## Motivation / Incident

During a large `pd_process_data()` run the R session aborted mid-write. The
result on disk:

```
<alias>/.stamp/catalog.qs2   -> 44 bytes (truncated qs2 stream)
```

Every subsequent read failed:

```
pip_read("BOL_2022_EH_INC_ALL", alias = "pip_meta")
  -> stamp::st_versions() -> .st_catalog_read() -> qs2::qs_read()
  ! Decompression error
```

The catalog is a **single shared index** per alias (`.st_catalog_path()` =
`<state_dir>/catalog.qs2`), so one truncated write broke reads for *all*
artifacts, not just the one being written. The per-artifact data and version
sidecars were intact — only the derived index was lost.

### Observed evidence: delete-then-move also fails under normal SMB locking

While rebuilding the corrupt catalog on the production SMB share
(`//w1wbgencifs01/pip/...`), `.st_catalog_write()` failed outright — not from a
crash, but from `fs::file_delete()` step (2) below hitting a live file lock:

```
Backed up old catalog -> .../surveys/.stamp/catalog.qs2.corrupt-20260625-090423
Error:
! [EBUSY] Failed to remove '.../surveys/.stamp/catalog.qs2': resource busy or locked
 3. └─stamp:::.st_catalog_write(cat_new, alias = alias)
 4.   └─fs::file_delete(p)
```

This is concrete field proof for **R3**: the explicit `delete` in
`.st_catalog_write()` is not merely a crash-window risk, it **actively fails on
SMB/Windows** whenever the catalog still has a lingering handle (SMB closes are
lazy, so a `delete` arriving right after any prior read — even a backup copy —
gets `STATUS_SHARING_VIOLATION` → `EBUSY`). An atomic rename-over needs no
`delete` and sidesteps this entirely. The repair script
(`pipdata/data-raw/rebuild_stamp_catalog.R`) was switched to a verified-temp +
rename-over commit (no `delete`, no `file_copy`) and succeeded — this is exactly
the pattern Step 1 ports into stamp.

## Root Cause

`.st_catalog_write()` (`R/version_store.R`) is documented as atomic but is not
fully crash-safe:

```r
.st_catalog_write <- function(cat, alias = NULL) {
  p   <- .st_catalog_path(alias = alias)
  tmp <- fs::file_temp(tmp_dir = fs::path_dir(p), pattern = fs::path_file(p))
  .st_write_qs2(cat, tmp)         # (1) qs_save to temp
  if (fs::file_exists(p)) {
    fs::file_delete(p)            # (2) delete the good catalog
  }
  fs::file_move(tmp, p)           # (3) move temp into place
}
```

Two distinct failure windows:

1. **No source integrity check.** The atomic *rename* (3) only protects
   against an interrupted move. It does **not** protect against `qs_save` (1)
   producing a truncated/corrupt `tmp` (e.g. an OOM during qs2's compression
   buffer allocation, or a kill mid-flush). A corrupt `tmp` is then faithfully
   promoted into the canonical slot. There is no read-back / size / hash
   verification of `tmp` before the good catalog is deleted.

2. **Delete-then-move gap.** Step (2) deletes `p` *before* step (3) moves
   `tmp` into place. This is bad in two ways: (a) a crash between (2) and (3)
   leaves the catalog **missing** (recoverable as "empty", silently losing all
   version history) rather than intact; and (b) the explicit `delete` itself
   **fails with `EBUSY` on SMB/Windows** whenever the file has a lingering
   handle — confirmed in the field (see Observed evidence above). `fs::file_move`
   on the same volume is itself atomic, so the right ordering is move-over
   (replace) without a prior delete — which also removes the `EBUSY` failure
   mode, since a rename does not open the file content.

The same non-atomic direct-write pattern is used for data files via
`.st_write_qs2()` -> `qs2::qs_save()` (`R/format_registry.R`), and for the
version snapshot `artifact` files — so a crash there can also leave a corrupt
current data file (though the version snapshots provide redundancy).

## Requirements

| ID | Requirement |
|----|-------------|
| R1 | Catalog writes must never leave `catalog.qs2` truncated/corrupt after a process abort: either the previous good catalog or the complete new one survives |
| R2 | The temp catalog must be **verified readable** (round-trip `qs_read`) before it replaces the canonical file |
| R3 | Replace must not pre-`delete()` the destination — use a single atomic replace (rename-over), so there is no window where the catalog is missing |
| R4 | Provide a public `st_rebuild_catalog(alias)` that reconstructs the catalog from on-disk per-artifact version sidecars (no recompute) |
| R5 | `.st_catalog_read()` should degrade gracefully on a corrupt catalog: detect the decompression error, emit an actionable message pointing at `st_rebuild_catalog()`, and (option-gated) auto-attempt rebuild |
| R5a | A failed catalog read **must not leak the file handle**. On a `qs_read` error the open handle must be closed (e.g. `on.exit(close(con), add = TRUE)` / guaranteed RAII), so the corrupt `catalog.qs2` is not left locked for the rest of the session — otherwise recovery is blocked because the rebuild cannot replace a file the failed read still holds open (observed as `EBUSY` on rename/delete on SMB) |
| R6 | Apply the same verified-temp-then-atomic-replace helper to data-file writes (`.st_write_qs2` / snapshot `artifact`) |
| R7 | No new hard dependencies; base R + existing deps (`fs`, `qs2`, `jsonlite`, `data.table`) |
| R8 | Roxygen + NEWS entry in stamp |

## Implementation Steps

### Step 1 — `.st_atomic_write()` helper (verified temp -> atomic replace)

- **Requirements**: R1, R2, R3, R6, R7
- **File**: `R/version_store.R` (or a small `R/atomic_io.R`)
- Introduce one shared helper used by both catalog and data writes:

```r
# Write `x` to `path` durably:
#   1. serialize to a temp file in the same directory
#   2. verify the temp is readable (round-trip) before touching `path`
#   3. atomically replace `path` with the temp (rename-over, no pre-delete)
.st_atomic_write <- function(x, path, writer, reader = NULL) {
  dir <- fs::path_dir(path)
  fs::dir_create(dir, recurse = TRUE)
  tmp <- fs::file_temp(tmp_dir = dir, pattern = fs::path_file(path))
  ok  <- FALSE
  on.exit(if (!ok && fs::file_exists(tmp)) fs::file_delete(tmp), add = TRUE)

  writer(x, tmp)

  # (R2) integrity gate: non-empty + readable
  if (!fs::file_exists(tmp) || fs::file_size(tmp) == 0) {
    cli::cli_abort("Refusing to commit: temp write produced an empty file.")
  }
  if (!is.null(reader)) {
    tryCatch(invisible(reader(tmp)), error = function(e) {
      cli::cli_abort(c(
        "Refusing to commit: temp file failed verification read.",
        "x" = conditionMessage(e)
      ))
    })
  }

  # (R3) atomic replace; existing file stays intact until the rename succeeds
  fs::file_move(tmp, path)   # same-volume rename overwrites atomically
  ok <- TRUE
  invisible(path)
}
```

- Rewrite `.st_catalog_write()` to delegate:

```r
.st_catalog_write <- function(cat, alias = NULL) {
  p <- .st_catalog_path(alias = alias)
  .st_atomic_write(cat, p, writer = .st_write_qs2, reader = .st_read_qs2)
}
```

- **Note on `fs::file_move` overwrite semantics**: confirm it replaces an
  existing destination on Windows/SMB (the incident is on an SMB share). If it
  does not overwrite, fall back to `file.rename()` which maps to
  `MoveFileEx(..., MOVEFILE_REPLACE_EXISTING)` semantics on the same volume;
  add a test on the target filesystem. This is the one real portability risk —
  validate before shipping.

### Step 2 — `st_rebuild_catalog()` (public recovery)

- **Requirements**: R4, R7
- **File**: `R/rebuild.R` (alongside `st_rebuild`/`st_plan_rebuild`)
- Scan `<root>/**/<ID>.<ext>/versions/<version_id>/sidecar.json` and
  reconstruct `artifacts` / `versions` / `parents_index` using the existing
  internal schema (`.st_catalog_empty()`) and id helpers
  (`.st_artifact_id()`), then commit via the Step 1 `.st_catalog_write()`.
- `version_id` = version directory name (no recompute). `artifact_id` =
  `.st_artifact_id(.st_normalize_user_path(rel)$logical_path)` so it matches
  `st_versions()` lookups. `latest_version_id` = newest `created_at`.
- Back up any existing catalog to `catalog.qs2.corrupt-<ts>` first.
- Provide `dry_run = TRUE` default (report counts, write nothing).
- **Reference implementation already exists**: `pipdata/data-raw/rebuild_stamp_catalog.R`
  — port that logic into stamp as the exported function (drop the `:::` once
  it lives inside the package).

### Step 3 — Graceful read + guided recovery (and no leaked handle)

- **Requirements**: R5, R5a
- **File**: `R/version_store.R` — `.st_catalog_read()`; also `.st_read_qs2()`
  in `R/format_registry.R`
- Wrap `.st_read_qs2(p)` in `tryCatch`. On a decompression/read error:
  - emit `cli::cli_abort` (or warn) naming the catalog path and instructing
    the user to run `st_rebuild_catalog(alias = ...)`, instead of surfacing a
    raw `qs2` decompression error from deep in the stack;
  - if `getOption("stamp.auto_rebuild_catalog", FALSE)` is TRUE, attempt
    `st_rebuild_catalog()` automatically and continue.
- **(R5a) Guarantee handle closure on failure.** A `qs_read` that aborts
  mid-decompression can leave the OS file handle open for the life of the R
  session; on Windows/SMB this **locks the corrupt `catalog.qs2`** so the
  rebuild cannot replace it (`EBUSY` on rename/delete — observed in the field).
  Ensure the read path cannot leak: open via an explicit connection guarded by
  `on.exit(close(con), add = TRUE)`, or confirm `qs2::qs_read` releases the
  handle on error and add a regression test that proves the file is
  rename-able immediately after a failed read in the same session. This is what
  makes recovery possible without a session restart.

### Step 4 — Docs + NEWS

- **Requirements**: R8
- Roxygen for `st_rebuild_catalog()`; `@details` note on durability in
  `.st_catalog_write` (internal) and `st_save`.
- `NEWS.md` (stamp): durability fix + new recovery function.

## Testing Strategy

`tests/testthat/` in stamp:

1. **Atomicity / integrity gate**: stub `writer` to produce a truncated temp;
   assert `.st_atomic_write()` aborts and the **pre-existing** catalog is
   untouched (R1/R2).
2. **No missing-file window**: assert the destination always exists after a
   failed commit attempt (R3).
3. **Round-trip**: normal `st_save()` -> read back catalog -> versions match.
4. **Rebuild**: save N artifacts (some multi-version), delete/corrupt
   `catalog.qs2`, run `st_rebuild_catalog()`, assert `st_versions()` /
   `st_latest()` / `st_load()` all match pre-corruption state (R4).
5. **Graceful read**: write a 44-byte `catalog.qs2`, assert `.st_catalog_read()`
   raises the guided message (and auto-rebuilds when the option is set) (R5).
6. **Filesystem replace semantics**: overwrite-via-rename test, ideally
   parametrized to run on an SMB path when available (Step 1 note).

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| `fs::file_move` does not overwrite existing dest on Windows/SMB | Medium | High | Verify on target FS; fall back to `file.rename()`; covered by test 6. **Note**: the rename-over commit is already proven to work on the production SMB share (the repair script uses it); the open question is only fs's overwrite-existing-dest behavior, which test 6 must pin down |
| SMB lingering-handle `EBUSY` on any explicit `delete` of the catalog | Confirmed | High | Eliminate `delete` from the write path entirely (rename-over only) — already validated by the repair script |
| Verification read (R2) adds latency per catalog write | High (small) | Low | Catalog is small; round-trip is cheap relative to data writes; only catalog uses `reader` by default |
| Rebuild mis-derives `artifact_id` (case/separator drift) | Low | High | Reuse stamp's own `.st_normalize_user_path()`/`.st_artifact_id()` — identical to read path; covered by test 4 |
| Concurrent writers during rebuild | Low | Medium | Acquire the existing `catalog.lock` (`.st_with_lock`) around rebuild + document "run offline" |

## Out of Scope

- Catalog journaling / WAL or multi-writer transactions.
- Changing the qs2 format or compression settings.
- Per-artifact data-file checksumming on every load (separate `verify_on_load`
  feature already exists).
- The upstream OOM cause in `pd_process_data` (fixed separately in
  `pipdata`: see `2026-06-24-pd-process-data-oom-crash.md`).

## Completion Contract

### Outcome
A process abort during any stamp write leaves either the prior good file or a
complete new file on disk — never a truncated catalog. A damaged catalog is
recoverable in one call via `st_rebuild_catalog()` without recomputing data.

### Verification Surface
| ID | Evidence Required | Required |
|----|-------------------|----------|
| V1 | Test: truncated temp never replaces a good catalog | yes |
| V2 | Test: catalog file always present after a failed commit | yes |
| V3 | Test: `st_rebuild_catalog()` restores versions/latest/load after corruption | yes |
| V4 | Test/Doc: `fs::file_move` overwrite verified on the target SMB filesystem (high priority — EBUSY on delete already observed there) | yes |
| V6 | The write path contains no explicit `delete` of the catalog (rename-over only), confirmed by code review | yes |
| V7 | Test: after a failed `.st_catalog_read()` on a corrupt catalog, the file can be renamed/replaced **in the same session** (no leaked handle, R5a) | yes |
| V5 | `.st_catalog_read()` emits guided recovery message on corrupt catalog | yes |

### Constraints
| ID | Constraint |
|----|------------|
| C1 | No new hard dependencies |
| C2 | Catalog on-disk schema unchanged (`.st_catalog_empty()` layout) |
| C3 | Public read/write APIs unchanged except the added `st_rebuild_catalog()` |

### Boundaries
- **Allowed**: `stamp/R/version_store.R`, `stamp/R/rebuild.R`,
  `stamp/R/format_registry.R` (or new `stamp/R/atomic_io.R`), stamp tests,
  stamp `NEWS.md`/roxygen.
- **Out of scope**: `pipdata`, `pipload`, `pipfun`, qs2 internals.
