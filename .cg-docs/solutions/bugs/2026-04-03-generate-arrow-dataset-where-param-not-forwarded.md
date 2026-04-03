---
date: 2026-04-03
title: "generate_arrow_dataset does not forward 'where' to load_pip_data calls"
category: "bugs"
type: "bug"
language: "R"
tags: [pipdata, arrow-generation, load_pip_data, where, master, release, generate_arrow_dataset]
root-cause: "The 'where' parameter was commented out on the raw data load and hardcoded to 'release' on the metadata load, so surveys only present in the master inventory silently failed."
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# generate_arrow_dataset does not forward 'where' to load_pip_data calls

## Symptom

`generate_arrow_dataset(inv, overwrite = TRUE)` succeeded for one survey (BOL)
but errored on another (ARG) with:

```
✖ Wrong number of data to load.
ℹ It should be only 1. You attempt to load 0:
```

The function appeared to work because one survey happened to exist in both
the master and release inventories, masking the bug for that survey.

## Root Cause

Inside the `for` loop of `generate_arrow_dataset()`, two `load_pip_data()`
calls are made per survey — one for raw data and one for metadata. Both had
broken `where` handling:

1. **Raw data load** — `where = where` was commented out, so the call fell
   back to `load_pip_data()`'s default `where = c("release", "master")`,
   which `match.arg()` resolves to `"release"`.
2. **Metadata load** — `where` was hardcoded to `"release"` instead of
   using the `where` parameter received by `generate_arrow_dataset()`.

The function signature correctly defaults `where = "master"`, but neither
call ever used that value. Surveys whose metadata only existed in the master
inventory (e.g. ARG in a TEST stamp) received 0 results from the release
inventory lookup and failed. Surveys that happened to exist in both
inventories (BOL) succeeded, hiding the bug.

## Reproduction Test

Added to `tests/testthat/test-arrow-generation.R`:

```r
test_that("generate_arrow_dataset passes 'where' to both raw and meta load_pip_data calls", {
  calls_captured <- list()

  local_mocked_bindings(
    load_pip_data = function(..., where = c("release", "master"), metadata = FALSE) {
      calls_captured[[length(calls_captured) + 1L]] <<- list(
        metadata = metadata,
        where    = match.arg(where)
      )
      stop("stub - not testing further")
    },
    .package = "pipload"
  )

  inv <- data.table::data.table(
    survey_id      = "ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL",
    pip_id         = "ARG_2003_EPHC-S2_INC_ALL",
    country_code   = "ARG",
    surveyid_year  = 2003L,
    survey_acronym = "EPHC-S2",
    vermast        = "v01",
    veralt         = "v09",
    collection     = "GMD",
    module         = "ALL"
  )

  suppressWarnings(
    tryCatch(
      generate_arrow_dataset(inv, arrow_repo_path = tempdir(), where = "master"),
      error = function(e) NULL
    )
  )

  raw_call <- Filter(function(x) !x$metadata, calls_captured)
  expect_length(raw_call, 1L)
  expect_equal(raw_call[[1L]]$where, "master",
               info = "raw data load must use the 'where' argument, not the default 'release'")
})
```

The test failed on the old code (`actual: "release"`, `expected: "master"`)
and passes after the fix.

## Fix

In `R/arrow_generation.R`, inside `generate_arrow_dataset()`, both
`load_pip_data()` calls were updated to pass `where = where` and
`version = version`:

```r
# Before (raw load — where commented out, version commented out)
raw <- pipload::load_pip_data(
  ...
  #where        = where,
  #version      = version,
  metadata       = FALSE,
  verbose        = FALSE
)

# Before (meta load — where hardcoded, version commented out)
meta <- pipload::load_pip_data(
  ...
  where          = "release",
  #version       = version,
  metadata       = TRUE
)

# After (both calls)
raw <- pipload::load_pip_data(
  ...
  where          = where,
  version        = version,
  metadata       = FALSE,
  verbose        = FALSE
)

meta <- pipload::load_pip_data(
  ...
  where          = where,
  version        = version,
  metadata       = TRUE
)
```

The `@param where` roxygen documentation was also updated to clarify that
`where` is forwarded to **both** raw and metadata loads, and that `"master"`
is the correct default for processing surveys not yet published to the
release inventory.

## Lessons Learned

- **Commented-out parameters are bugs waiting to happen.** When a parameter
  is wired through a function signature but commented out in internal calls,
  the function silently ignores user intent. Prefer always-active defaults
  over commented-out code in production paths.
- **Partial success masks bugs.** When a batch function processes N items and
  only some fail, the failure pattern may be coincidental (e.g. one survey
  exists in both inventories, another only in one). Always test with surveys
  that exist *only* in the target repository.
- **Use `local_mocked_bindings` to assert call arguments.** For functions
  that delegate to external packages, mock the dependency and capture the
  actual arguments passed rather than only testing observable side effects.
  This catches forwarding bugs that integration tests may miss.

## Related

None.
