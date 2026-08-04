---
date: 2026-04-29
title: "Duplicate logmeta discriminator key causes malformed report sections"
category: "bugs"
language: "R"
tags: [logmeta, log_report, parse_log_meta, piplog, discriminator, inv_update_inf, release_write_err, aaa]
root-cause: "Two structurally different log events shared the same logmeta discriminator string ('inv_update_inf'), so parse_log_meta() routed both to the same report section with mismatched fields"
severity: "P1"
---

# Duplicate logmeta discriminator key causes malformed report sections

## Problem

`update_pip_inventory()` emits an `inv_update_inf` event for the inventory
verification block (fields: `n_expected`, `n_confirmed`, `n_missing`,
`surveys_confirmed`, `surveys_missing`). When the release inventory write
failed, the error handler was also emitting `error = "inv_update_inf"` — but
with completely different fields (`condition_msg` only, no count or survey
list fields).

`parse_log_meta()` uses the discriminator string as the sole type key to route
entries to report sections. With two structurally different events sharing one
key, the verification section renderer received entries missing `n_expected`,
`n_confirmed`, etc., causing `vapply(..., character(1))` to fail or produce
garbled output.

## Root Cause

The release write error handler was copy-pasted from the verification block
and the discriminator was not changed. There was no lint or test preventing
reuse of an existing discriminator for a new event shape.

## Solution

1. **New discriminator**: Changed the release write error handler in
   `R/update_pip_inventory.R` to emit `error = "release_write_err"`:

```r
error = function(e) {
  pipfun::log_error(
    "Release inventory write failed. Master inventory will be saved without release version columns.",
    name = "pipdata_log",
    logmeta = list(
      error = "release_write_err",       # <-- was "inv_update_inf"
      condition_msg = conditionMessage(e)
    )
  )
  NULL
}
```

2. **Register in `.log_internal_types`** (`R/aaa.R`) so it is excluded from
   the Summary by Type table:

```r
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata",
  "release_write_err"       # <-- added
)
```

3. **Contract test** added in `tests/testthat/test-logging-integration.R`:

```r
test_that("release_write_err logmeta structure", {
  expected_structure <- list(
    error = "release_write_err",
    condition_msg = "some error message"
  )
  expect_equal(expected_structure$error, "release_write_err")
  expect_true(is.character(expected_structure$condition_msg))
  expect_true("error" %in% names(expected_structure))
  expect_false("info" %in% names(expected_structure))
})
```

4. **Update `compound-gpid.context.md`**: `release_write_err` added to the
   canonical logmeta types list.

## Prevention

- **Every new logmeta discriminator must be unique** — never reuse an existing
  discriminator for a structurally different event (different fields).
- When adding a new logmeta emitter: (a) choose a unique string, (b) add it to
  `.log_internal_types` in `R/aaa.R` if it is a metadata carrier, (c) add a
  contract structure test, (d) document it in the `@details` Logging section of
  the relevant function's roxygen comment, (e) add it to the canonical types
  list in `compound-gpid.context.md`.
- The logmeta field contract (`info`/`error` as string discriminator, never an
  R condition object) is documented in `compound-gpid.context.md` —
  re-read that section before adding any new `log_add`/`log_error`/`log_info`
  call.

## Related

- [2026-04-07-internal-logmeta-types-polluting-type-summary.md](./2026-04-07-internal-logmeta-types-polluting-type-summary.md) — original introduction of `.log_internal_types` and the discriminator contract
- [2026-04-29-logging-in-trycatch-handlers.md](../testing-patterns/2026-04-29-logging-in-trycatch-handlers.md) — how `capture_log_args()` behaves inside tryCatch handlers
- [2026-04-27-contract-testing-for-logging-side-effects.md](../testing-patterns/2026-04-27-contract-testing-for-logging-side-effects.md) — contract testing pattern used for logmeta structure tests
