---
date: 2026-05-27
title: "Catalog-wide validation before scope filter generates spurious warnings"
category: "data-quality"
language: "R"
tags: [stamp, catalog, validation, filter-first, pip_id, warning]
root-cause: "pip_id format validation ran on the full stamp catalog (all historical artifacts) before filtering to the current-run's target_ids, causing false-positive warnings for unrelated historical entries."
severity: "P2"
---

# Catalog-Wide Validation Before Filter Generates Spurious Warnings

## Problem

`build_pip_inventory()` emitted `build_pip_inventory_bad_pip_id_format`
warnings on every run — even when the current run's surveys were perfectly
valid. The warnings listed artifact paths from historical stamp catalog
entries (test artifacts, admin snapshots) completely unrelated to the
current run.

## Root Cause

The code validated pip_id format on the full catalog returned by
`st_catalog_query()` **before** filtering to the current run's `target_ids`:

```r
# WRONG order
bad_data <- cat_data[!grepl(pip_id_pattern, pip_id), path]  # full catalog
...
cat_data <- cat_data[pip_id %in% target_ids]  # filter happens after
```

The full catalog contains every artifact ever saved under the alias,
including old test runs, admin entries, and any non-survey artifacts.
Validating the full set means any historical non-standard entry triggers
a warning on every subsequent run forever.

## Solution

Apply the **filter-first principle**: scope `cat_data` and `cat_meta` to
`target_ids` first, then validate format only on the current-run set:

```r
# Correct order: filter first, then validate
cat_data <- cat_data[pip_id %in% target_ids]
cat_meta <- cat_meta[pip_id %in% target_ids]

# Validation now only covers current-run artifacts
bad_data <- cat_data[!grepl(pip_id_pattern, pip_id), path]
bad_meta <- cat_meta[!grepl(pip_id_pattern, pip_id), path]
```

This means warnings are actionable — if one fires, it is guaranteed to
be about a survey that was actually processed in this run.

## Prevention

**Filter-first is the default pattern for any catalog-based assembler**:
- Query the full catalog (one cheap call)
- Immediately scope to the current run's target set
- All validation, deduplication, and join logic runs on the scoped set only

This avoids: (1) spurious warnings from historical entries; (2) performance
cost of validating thousands of irrelevant rows; (3) false confidence from
"no warnings" when the target set is actually problematic.

The comment in `build_pip_inventory.R` captures the intent:
```r
# Scope to current run first (filter-first principle: validate only the
# current-run set so unrelated historical artifacts in the alias never
# produce spurious warnings).
cat_data <- cat_data[pip_id %in% target_ids]
```

## Related

- `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`
