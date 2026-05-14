---
date: 2026-05-04
title: "Master inventory real column names for data and metadata versioning"
category: "data-quality"
language: "R"
tags: [master-inventory, pipload, column-names, versioning, content-hash, metadata]
root-cause: "Assumed column names version_id_data and version_id_metadata do not exist in the real pip master inventory; the actual versioning columns use content hashes and ISO timestamps"
severity: "P2"
---

# Master inventory real column names for data and metadata versioning

## Problem

`R/pd_deflation.R` referenced three non-existent columns in the PIP master
inventory when filtering and sorting rows:

| Code used (wrong) | Real column |
|---|---|
| `row$version_id_data` | `row$content_hash_data` |
| `row$version_id_metadata` | `row$content_hash_metadata` |
| `order(row$version_id_data, …)` | `order(row$created_at_metadata, …)` |

These column names were invented during implementation without cross-checking
against the live inventory object. The code contained a guard that detected
the missing `version_id_metadata` column and aborted with an error, but the
guard itself used the wrong column name so it never fired — the error would
have surfaced only at the `row$version_id_metadata[[1L]]` assignment.

Matching mock fixtures in `tests/testthat/test-pd-deflation.R` used the same
wrong names, so tests passed despite the column mismatch.

## Root Cause

The master inventory schema was not documented in project context. Developers
writing new code that queries the inventory have to guess column names or
inspect a live object — a gap that caused this mismatch to slip through review.

## Solution

Use the verified real column names (from `names(pipload::load_pip_master_inventory())`
checked against live `old_pip_inv`):

```r
# Version identifier for the data artifact
row$content_hash_data       # SHA-like hex string, e.g. "6e0d6b340d834c6a"

# Version identifier for the metadata artifact
row$content_hash_metadata   # SHA-like hex string, e.g. "1153dc4b3beb72ff"

# Timestamp to sort "most recent" when version = NULL
order(row$created_at_metadata, decreasing = TRUE)   # ISO-8601 string
```

Fixed code in `.load_deflation_aux()`:

```r
if (!is.null(version)) {
  row <- row[row$content_hash_data == version, ]
} else {
  row <- row[order(row$created_at_metadata, decreasing = TRUE), ]
  row <- utils::head(row, 1L)
}

if (!"content_hash_metadata" %in% names(row)) {
  cli::cli_abort("Master inventory missing 'content_hash_metadata' column.")
}
meta_version <- row$content_hash_metadata[[1L]]
```

Update test fixtures to match:

```r
fake_inv <- data.table::data.table(
  pip_id                = "ABC_2015_TST_INC_D1",
  content_hash_data     = "abc123",
  content_hash_metadata = "meta_abc123",
  created_at_metadata   = "2026-01-01T00:00:00Z"
)
```

## Canonical Master Inventory Schema (key columns)

Verified from live `old_pip_inv` (release 20260401):

| Column | Type | Description |
|--------|------|-------------|
| `survey_id` | chr | Full DLW survey id (e.g. `AGO_2000_HBS_V01_M_V02_A_GMD_GPWG`) |
| `pip_id` | chr | PIP survey identifier (key for lookup) |
| `path_data` / `path_metadata` | chr | UNC paths to `.qs2` artifacts |
| `content_hash_data` | chr | SHA content hash of the data artifact (use as data version) |
| `content_hash_metadata` | chr | SHA content hash of the metadata artifact (use as metadata version) |
| `created_at_data` / `created_at_metadata` | chr | ISO-8601 creation timestamps |
| `size_bytes_data` / `size_bytes_metadata` | fs_bytes | File sizes |
| `country_code` | chr | 3-letter ISO code |
| `surveyid_year` | dbl | Survey year |
| `survey_acronym` | chr | Survey program acronym |
| `status` | chr | `"valid"` / other statuses |
| `welfare_type` | chr | `"CON"` / `"INC"` |

**No `version_id_*` columns exist.** Versioning is by content hash, not by
a separate version identifier field.

## Prevention

- **Before writing any code that queries the master inventory, inspect the
  live column names**: `names(pipload::load_pip_master_inventory())`.
- Add the canonical schema table above to `compound-gpid.context.md` so it
  is always available to AI assistants and new developers.
- When writing mock `fake_inv` fixtures in tests, derive them from the real
  schema (at minimum include the columns actually used in the function under test).

## Related

- `.cg-docs/solutions/testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md`
  — pattern for mocking `pipload` functions in tests
- `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md`
  — sister finding: `content_hash_metadata` is stamp's `content_hash`, not `version_id`; use `pip_read(..., version="available")` to resolve
