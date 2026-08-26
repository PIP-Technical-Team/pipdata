---
date: 2026-08-21
title: "Verify R package dependencies and stage checkpoints from built artifacts"
category: "testing-patterns"
language: "R"
tags: [R-CMD-check, package-build, piplog, stamp, checkpoint, testthat]
root-cause: "Source-loaded tests and source-directory checks can miss installed-package dependency resolution, alias registration, and checkpoint persistence failures."
severity: "P1"
related: [".cg-docs/solutions/build-errors/2026-08-21-authors-at-r-description-validation.md", ".cg-docs/solutions/testing-patterns/2026-04-27-contract-testing-for-logging-side-effects.md", ".cg-docs/solutions/testing-patterns/2026-08-25-verify-exported-r-api-retirement.md"]
---

# Verify Built Packages and Stage Checkpoints

## Problem

Unified logging passed source-loaded tests, but review identified gaps that
source-only assertions could not detect:

- `R CMD check` resolved the global `pipfun` 1.0.0 instead of the local 1.0.1
  checkpoint build.
- A checkpoint call could pass a source-order test while using an unregistered
  `stamp` alias.
- A mocked checkpoint did not prove that the persisted object contained the
  correct stage metadata.

## Root Cause

Interactive `pkgload::load_all()` and source-directory checks do not reproduce
the installed-package library and artifact-registration environment. Tests that
inspect function text or mock persistence can therefore remain green while the
installed package fails to resolve a dependency or save/load a checkpoint.

## Solution

Build and check the package tarball, and make the dependency library explicit:

```powershell
R CMD build --no-build-vignettes .
R CMD check --library=<verified-library> --no-manual --no-build-vignettes --as-cran pipdata_0.0.1.tar.gz
```

For a checkpoint, initialize the intended alias, save the log, then inspect the
persisted artifact:

```r
stamp::st_init(root = root, alias = "dlw_meta")
pipfun::log_save_checkpoint(
  name = "pipdata_log",
  stage = "dlw",
  alias = "dlw_meta"
)
info <- stamp::st_info(
  fs::path(root, "pipdata_log_checkpoint_dlw.qs2"),
  alias = "dlw_meta"
)
testthat::expect_equal(info$sidecar$stage, "dlw")
```

At orchestration boundaries, place the checkpoint after the final summary and
inventory/report diagnostics, not merely after the first summary log. For R
package tests, inspect installed function bodies with `deparse()` rather than
reading `R/*.R` paths that do not exist inside an installed tarball.

## Prevention

- Treat `R CMD build` followed by tarball `R CMD check` as the package-level
  validation path.
- Install and verify the exact dependency version required by `DESCRIPTION` in
  the library used by the checker.
- Test both checkpoint aliases used by the application and reload each saved
  artifact with `stamp::st_info()` or `stamp::st_load()`.
- Keep source-order assertions as supplementary checks, never as the sole proof
  of persistence or finalization order.
- Include generated man pages in the staged change; source regeneration alone
  does not track new `.Rd` files.

## Related

- [Validate `Authors@R` through a built R package](../build-errors/2026-08-21-authors-at-r-description-validation.md)
- [Contract testing for logging side effects](2026-04-27-contract-testing-for-logging-side-effects.md)
- [Verify exported R API retirement from built package surfaces](2026-08-25-verify-exported-r-api-retirement.md)
- [Fail-closed durable reconciliation for staged data pipelines](../data-quality/2026-08-26-durable-stage-reconciliation.md)
