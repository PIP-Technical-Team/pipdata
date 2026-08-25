---
date: 2026-08-25
title: "Verify exported R API retirement from built package surfaces"
category: "testing-patterns"
language: "R"
tags: [R-CMD-check, api-retirement, namespace, installed-help, roxygen2]
root-cause: "Source deletion alone does not prove that an exported R API disappeared cleanly because generated namespace/help artifacts can remain stale and source-loaded checks can hide installed-package differences."
severity: "P1"
plan: ".cg-docs/plans/2026-08-24-retire-copy-dlw-metadata.md"
related: [".cg-docs/solutions/build-errors/2026-04-30-r-package-file-archival-checklist.md", ".cg-docs/solutions/testing-patterns/2026-08-21-built-package-and-checkpoint-verification.md"]
---

# Verify Exported R API Retirement from Built Package Surfaces

## Problem

Removing an exported R function requires more than deleting its source. The
generated `NAMESPACE` can change unexpectedly, an orphaned `.Rd` page can keep
the function in installed help, and a passing source-loaded test suite does not
prove what users receive from the package tarball.

For `copy_dlw_metadata()`, the retirement also had to distinguish intentional
compatibility risk for unknown external callers from accidental regressions to
the package's remaining exports, help topics, and check diagnostics.

## Root Cause

An exported R API is represented on several independently generated and
installed surfaces: source, `NAMESPACE`, `.Rd` files, the installed namespace,
and the installed help database. Roxygen regeneration does not reliably delete
orphaned `.Rd` files, while `pkgload::load_all()` does not reproduce the exact
installed-package environment. A final check without a comparable baseline can
also hide pre-existing diagnostics or unrelated generator drift.

## Solution

Use a baseline/final retirement gate against built tarballs:

1. Search active source, tests, vignettes, examples, scripts, and package
   metadata for real callers. Stop if an operational dependency appears.
2. Before deletion, snapshot the complete ordered `NAMESPACE`, help-source
   filenames, R/dependency environment, and normalized package-check messages.
3. Build and check the unchanged package with the exact library, environment,
   and safety flags planned for the final run.
4. Delete the source and its `.Rd` file explicitly, then regenerate once.
5. Require the final ordered `NAMESPACE` to equal the baseline with only the
   intended `export(...)` line removed.
6. Build and check the final tarball in the same environment. Compare complete
   normalized diagnostics, not only status counts.
7. Query the independently installed final package to prove the symbol and help
   topic are absent, and compare export/help sets to prove nothing else changed.

```r
expected_namespace <- baseline_namespace[
  baseline_namespace != "export(copy_dlw_metadata)"
]
stopifnot(identical(final_namespace, expected_namespace))

ns <- loadNamespace("pipdata", lib.loc = final_library)
stopifnot(!"copy_dlw_metadata" %in% getNamespaceExports(ns))

help_db <- tools:::fetchRdDB(
  file.path(final_library, "pipdata", "help", "pipdata")
)
stopifnot(!"copy_dlw_metadata" %in% names(help_db))
```

When examples or package initialization can touch external storage, isolate the
package root and use safe check flags consistently for both runs. Record the
exact commands, environment, outputs, and hashes in a durable work report.

## Prevention

- Treat an exported-function deletion as a public API change and document it in
  release notes, including whether a replacement exists.
- Never rely on roxygen to remove orphaned `.Rd` files; delete them explicitly.
- Compare the entire ordered `NAMESPACE`, not only the target export count.
- Verify installed namespace and help databases from built tarballs rather than
  treating source searches or `load_all()` as sufficient evidence.
- Capture a contemporaneous baseline with the same environment and flags so
  pre-existing diagnostics remain distinguishable from retirement regressions.
- Remove unrelated generator drift before the final gate and rebuild the exact
  reviewed state after any review fix.

## Related

- [R package file archival checklist](../build-errors/2026-04-30-r-package-file-archival-checklist.md)
- [Verify R package dependencies and stage checkpoints from built artifacts](2026-08-21-built-package-and-checkpoint-verification.md)
- [Retire `copy_dlw_metadata()` work report](../../work-reports/2026-08-25-retire-copy-dlw-metadata.md)
