---
date: 2026-11-06
title: "Contained R preflight and certified same-store parents"
category: "environment-issues"
language: "R"
tags: [namespace-hooks, containment, exact-pins, stamp, windows]
root-cause: "Lazy-closure decoding resolved live namespaces, while alias spelling and legacy snapshot assumptions did not satisfy the downstream consumer contracts."
severity: "P1"
plan: ".cg-docs/plans/2026-11-05-m2-deflate-one-survey-approved-contract.md"
---

# Contained R Preflight and Certified Same-Store Parents

## Problem
Preparation appeared read-only, but decoding installed lazy closures loaded
third-party namespaces and ran startup hooks. A credentials hook generated a
Git helper configuration. Later runner failures involved Windows path casing,
multiple aliases at one root, duplicate parent paths, and legacy snapshots
without persisted byte checksums. None was a reason to bypass the engine guard.

## Root Cause
`lazyLoadDBfetch()` can resolve namespaces while unserializing closures.
Intercepting only `getNamespace()` was insufficient: direct `loadNamespace()`
calls still occurred. A successful preparation exit was not proof of no loads.

Separate integration assumptions also failed: lexical paths were compared as
though Windows were case-sensitive; linked pipload expected one alias per
configured root spelling; stamp rejects duplicate parent paths even at distinct
versions; and exact legacy readback does not establish certified byte checksums.

## Solution
Use the inspected three-script recipe under `.tmp/m2-phase3-deflate/` as the
version-specific implementation, not as a portable sandbox or production recipe.
Those runtime scripts are intentionally Git-ignored: their links describe the
current workspace, not files guaranteed in a clean checkout. The linked durable
reports and the explanation below retain the reusable evidence and technique.

### Inspect Without Loading
In a fresh disposable R process, `prepare.R` temporarily intercepts both
namespace lookups during hook extraction, restores both bindings on exit, and
requires an empty `loadedNamespaces()` delta. It inspects only R source files as
text; binary files remain in source identity manifests. Dependency copies are
hash-compared without installation. Do not execute package code with the
extraction stubs installed, or repeat this inspection in an ordinary session.

`run.ps1` starts R with `--vanilla` and a constructed child environment. Resolved
temp/home/cache/log/library/source/output paths stay on E; standard executables
and libraries on C are read locations only. Profiles, authentication caches,
telemetry and restart children need the same containment. In-memory Git helper
configuration prevents the inspected credentials hook from writing a default
global helper; do not change the user's Git configuration to repair a test.

The failed preflight's generated external E temp config was removed only after
explicit approval. Failed-run evidence was retained. This history is not a
whole-system no-C-write audit or evidence that arbitrary hooks are safe.

### Prove One Physical Store
For this Windows fixture, distinct case-preserved alias spellings satisfy the
linked loader's lexical lookup while canonical resolved paths are checked to
identify one physical store. Do not generalize the spelling workaround to
case-sensitive systems. Removing the default alias during legacy fixture tests
restores stamp's documented absolute-path auto-detection; recipe reads keep
explicit aliases. Clean and metadata parents use distinct artifact names, not
two pins at the same path.

### Certify Actual Upstream Payloads
Execute the actual accepted producers/validation/clean route first. Preserve
its original receipts. Legacy snapshots without persisted checksums cannot be
used as certified execution parents merely because an exact read succeeds.

The runner uses the public modern write API with a nonempty typed source-pin
field, creates new consumer pins and asserts exact payload readback. This
excerpt assumes the contained runner has already bound the value, alias and
original source pin; it is not stand-alone environment setup:

```r
written <- pipload::pip_write(
  value, id = id, alias = alias, verbose = FALSE,
  metadata = list(source_version = source_pin),
  semantic_schema = list(source_version = list(
    type = "character", shape = "scalar", required = TRUE,
    nullable = FALSE, allow_na = FALSE
  ))
)
exact <- pipload::pip_read(
  id, version = written$version_id, alias = alias, verbose = FALSE
)
stopifnot(identical(exact, value))
```

The tested stamp source rejects an empty typed schema. Re-read the immutable PFW
pin before certification rather than using an in-memory table whose transient
index cache may have changed. Preserve both original producer and consumer pins
in final typed provenance; never edit immutable sidecars to add a checksum.
Parent preparation is not final calculation success: deflation still executes
through request-aware selection and exact receipt/payload/semantic readback.

## Prevention
- Check the namespace delta and captured warnings, not a preflight success
  banner. Preserve failed evidence instead of silently retrying past budgets.
- Compare resolved Windows paths with appropriate case handling; prove actual
  containment and identity rather than shortening or relaxing path checks.
- Test checksum-bearing consumer contracts before treating producer pins as
  execution-ready. A publication receipt alone is not a selected calculation.
- Record loaded source paths, file/body identities and child inventories; Git
  HEAD alone does not identify a dirty source-loaded implementation.
- Execute every selected file/block, including restart and junction checks.
  This bounded source-loaded proof does not replace built-package validation.

## Verified Evidence
The corrected preparation copied/hash-checked 118 dependency snapshots with no
namespace delta or warnings. Actual upstream smoke, exact parent preparation and
the selected 125-block / 831-expectation run passed, followed by final verify.
These are recorded implementation results, not new executions for this capture.

## Related
- [Completed execution report and incident history](../../work-reports/2026-11-05-m2-deflate-one-survey-approved-contract.md)
- [Final verification](../../reviews/2026-11-05-m2-deflate-one-survey-approved-contract-verify-review.md)
- [Actual contained launcher](../../../.tmp/m2-phase3-deflate/run.ps1)
- [No-load preparation](../../../.tmp/m2-phase3-deflate/prepare.R)
- [Actual upstream and parent preparation](../../../.tmp/m2-phase3-deflate/runtime.R)
- [Strict numerical admission](../data-quality/2026-11-06-strict-admission-for-frozen-numerical-calculations.md)
- [Result-bound provenance checkpoints](../data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md)
- [Built-package and checkpoint verification](../testing-patterns/2026-08-21-built-package-and-checkpoint-verification.md)
