---
date: 2026-05-05
title: "pipmd/pipgd S3 class stripped when loading survey from stamp"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, stamp, pip-class, pipmd, pipgd, s3, round-trip, pd-deflation]
root-cause: "stamp serialises and deserialises data.tables as plain data.table objects; the pipmd/pipgd S3 class prefix is not preserved across a pip_write()/pip_read() round-trip."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# pipmd/pipgd S3 class stripped when loading survey from stamp

## Symptom

```
Error in UseMethod("deflation"):
no applicable method for 'deflation' applied to an object
of class "c('data.table', 'data.frame')"
```

`pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")` loads the survey from stamp
but the `deflation()` S3 dispatch then fails because the object has no
`pipmd` or `pipgd` class.

## Root Cause

`stamp::st_save()` / `st_load()` (via `pip_write()` / `pip_read()`) serialises
a `data.table` and restores it as a plain `data.table` — custom S3 class
prefixes like `pipmd` and `pipgd` are not preserved in the round-trip.

The class is assigned during the pipeline by `data_to_dt()` →
`pipload::as_pip()`, but it is never re-applied on load.

## Fix

After `pip_read()` in `pd_deflation()` Mode B, restore the class using the
`module` column (if present) via `assign_pipclass()`, with a fallback to
inferring from the `pip_id` last segment when `module` was dropped on save
(which `data_to_dt()` does with `dt[, module := NULL]`).

```r
# R/pd_deflation.R — pd_deflation(), Mode B load block
dt <- pipload::pip_read(id = pip_id, alias = "pip", version = version)
# stamp round-trips strip the pip S3 class prefix — restore it.
if ("module" %in% names(dt)) {
  dt <- pipload::assign_pipclass(dt)
} else {
  pip_module <- utils::tail(strsplit(pip_id, "_", fixed = TRUE)[[1L]], 1L)
  dt <- if (grepl("GROUP", pip_module, ignore.case = TRUE)) {
    pipload::as_pipgd(dt)
  } else {
    pipload::as_pipmd(dt)
  }
}
```

## Lessons Learned

- **Never assume S3 class prefixes survive stamp serialisation.** Any code that
  loads a `pip_write()`-saved object and dispatches on its S3 class must
  explicitly restore the class after loading.
- `assign_pipclass()` is the authoritative way to assign `pipmd`/`pipgd` — it
  reads the `module` column. The `pip_id` last-segment fallback is a secondary
  heuristic for when `module` was dropped before saving.
- The long-term fix is to store the class name in stamp metadata (e.g. as a
  field in the `metadata` list passed to `pip_write()`), so re-assembly on load
  is automatic and doesn't require inference.

## Related

- `.cg-docs/solutions/bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md` — same root cause: `ppp_data_level`/`cpi_data_level` columns also stripped; fixed with `restore_data_level_cols()`
- `.cg-docs/solutions/bugs/2026-05-05-stale-content-hash-in-load-deflation-aux.md`
- Roadmap: `store-version-id-in-inventory` — same plan should also capture pip
  class in stamp metadata to avoid the inference fallback entirely.
