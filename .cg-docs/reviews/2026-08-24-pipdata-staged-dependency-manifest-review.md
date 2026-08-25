---
date: 2026-08-25
depth: full
type: standard
plan: .cg-docs/plans/2026-08-24-pipdata-staged-dependency-manifest.md
findings:
  P0.1: fixed
  P0.2: fixed
  P0.3: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
---

# Review Report

## P0 - Blocking

- **[P0.1]** Snapshot planning omitted dependency facts for existing entities.
  Fixed by deriving semantic input, output, and code-change facts.
- **[P0.2]** Checkpoints permitted missing input/code provenance.
  Fixed by requiring and persisting exact records, inputs, and fingerprints.
- **[P0.3]** Fingerprints omitted transitive value-affecting dependencies.
  Fixed with curated closures and a codetools audit.

## P1 - Critical

- **[P1.1]** Force selection did not materialize current entities.
  Fixed with case-insensitive survey and pip identifier resolution.
- **[P1.2]** Reconciliation could lose fields or accept unmatched keys.
  Fixed with canonical row preservation, uniqueness, and match validation.
- **[P1.3]** Pipeline tests did not exercise authoritative top-level behavior.
  Fixed with routing, durable-boundary, restart, and bounded-I/O tests.
- **[P1.4]** Offline lease recovery could admit a live owner.
  Fixed by requiring confirmation and demonstrated same-host process death.

## Verification

- `devtools::test()`: 894 passed, 0 failed, 2 skipped.
- `devtools::check()`: 0 errors, 0 warnings, 4 pre-existing/environmental notes.
