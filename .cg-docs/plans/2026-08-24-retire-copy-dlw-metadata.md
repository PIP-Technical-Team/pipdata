---
date: 2026-08-24
title: "A3.1: Retire copy_dlw_metadata()"
status: completed
completed-date: 2026-08-25
scope: "Lightweight"
brainstorm: ".cg-docs/brainstorms/2026-08-24-pipeline-alignment-a3-cleanup.md"
language: "R"
estimated-effort: "small"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [pipeline-alignment, api-cleanup, dlw, documentation]
execution-report: ".cg-docs/work-reports/2026-08-25-retire-copy-dlw-metadata.md"
---

# Plan: A3.1 Retire `copy_dlw_metadata()`

## Objective

Remove the unsupported `copy_dlw_metadata()` administrative utility from the
installed package, including its export and stale help page, while documenting
the intentional public API break, preserving every unrelated package API, and
preparing documentation sources so the next pkgdown build drops the page.

## Context

The decided brainstorm found no active or historical repository caller, no
test, and no known operational use for `copy_dlw_metadata()`. The implementation
in `R/pipdata_copy_dlw_meta.R` predates the unified DLW logging checkpoint,
does not establish safe stamp alias rebinding, ignores write results, permits
partial copies, and leaves global working-release state changed.

The removal is intentionally direct rather than deprecated. The package is at
version 0.0.1, but the function is exported and has been published through
pkgdown, so release notes must identify the compatibility break. If new evidence
of real use appears, execution stops and the utility is not preserved or
redesigned under this cleanup plan.

Relevant prior patterns:

- Explicitly delete stale Rd files before roxygen regeneration because
  `devtools::document()` does not reliably remove orphaned pages:
  `.cg-docs/solutions/build-errors/2026-04-30-r-package-file-archival-checklist.md`.
- Verify the built tarball against a controlled dependency library rather than
  relying only on `pkgload::load_all()`:
  `.cg-docs/solutions/testing-patterns/2026-08-21-built-package-and-checkpoint-verification.md`.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Confirm there is no executable repository caller before removal. | Brainstorm Decision; retirement safety |
| R2 | Delete the utility source rather than archive or internalize it. | Brainstorm Decision |
| R3 | Remove only `export(copy_dlw_metadata)` from the generated namespace. | Public API inventory |
| R4 | Explicitly delete the stale `copy_dlw_metadata.Rd` page before regenerating documentation. | Archival checklist |
| R5 | Add a breaking-change note stating that no replacement copier is introduced. | Compatibility decision |
| R6 | Preserve all unrelated exports, help topics, and package behavior. | Scope boundary |
| R7 | Verify removal without network access, release mutation, or persistent storage writes. | Safety acceptance criteria |
| R8 | Link execution and completion to roadmap feature `audit-copy-dlw-meta`. | Existing roadmap item |
| R9 | Capture a contemporaneous pre-deletion tarball-check baseline using the exact final environment and flags. | Plan review P2.1 |
| R10 | Require the complete NAMESPACE to differ by exactly one removed directive. | Plan review P2.2 |
| R11 | Update the roadmap description to record retirement as the audited disposition. | Plan review P3.1 |

## Affected Files

| Path | Action |
|------|--------|
| `R/pipdata_copy_dlw_meta.R` | Delete the complete source and roxygen block. |
| `man/copy_dlw_metadata.Rd` | Delete explicitly before documentation regeneration. |
| `NAMESPACE` | Regenerate; expected semantic change is removal of one export. |
| `NEWS.md` | Add one bullet under `## Breaking changes`. |
| `roadmap.json` | Update plan/status and audited disposition only through `@cg-roadmap`; never edit directly. |

No `_pkgdown.yml` edit is expected because its catch-all reference section
discovers exported functions automatically.

## Implementation Steps

### 1. Reconfirm Preconditions and Capture the Verification Baseline

- **Requirements**: R1, R6, R7, R9, R10
- **Files**: read-only preflight over source, generated documentation,
  NAMESPACE, DESCRIPTION, and temporary checker output
- **Details**:
  1. Search active `R/`, `tests/`, `vignettes/`, scripts, and package metadata
     for executable references to `copy_dlw_metadata()`.
  2. Treat the decided brainstorm response, "No known use," as the recorded
     operator-use decision. Stop if repository or user evidence contradicts it.
  3. Snapshot the complete NAMESPACE with `readLines()`, preserving the ordered
     generated header, blank lines, S3, export, and import directives while
     normalizing platform line endings. The required final vector is this
     snapshot with only the exact line `export(copy_dlw_metadata)` removed.
  4. Snapshot generated help-topic names and the installed dependency versions,
     `.libPaths()`, R version, and temporary `PIP_ROOT_DIR` shape.
  5. Before any deletion, build and check the unchanged tarball using the same
     isolated library/environment and flags planned for the final gate. Include
     `--no-examples` so the unrelated storage-backed `unq_obs_dt()` example
     cannot call working-release/storage code.
  6. Run the UTF-8-aware Rd harness over the baseline pages and record
     diagnostics by path. Use
     `tools::checkRd(tools::parse_Rd(path, encoding = "UTF-8"), def_enc = TRUE)`;
     do not require unrelated pre-existing diagnostics to be zero.
  7. Persist the exact baseline/final commands, exit statuses, environment
     manifest, complete normalized error/warning and Rd diagnostics, and their
     comparison in the durable work report. Raw logs may remain temporary only
     when their hashes and all completion-relevant contents are captured in the
     report; a temporary path alone is not evidence. Compare messages, not only
     counts.
- **Test Scenarios**:
  - Happy path: no caller exists and baseline evidence is complete.
  - Edge case: baseline has pre-existing notes/errors; record exact messages and
    continue only if the check itself executed successfully.
  - Error path: operational use appears, or a reproducible baseline cannot be
    obtained with safe flags; stop before deletion.
- **Tests**: targeted repository search; ordered `readLines()` NAMESPACE/help
  snapshots; UTF-8 baseline Rd diagnostics; pre-change tarball build/check with
  `--no-examples`.
- **Acceptance criteria**: V1 passes and a reusable baseline exists for V4.

### 2. Remove the Public Surface, Regenerate, and Verify

- **Requirements**: R2, R3, R4, R5, R6, R7, R8, R10, R11
- **Files**: `R/pipdata_copy_dlw_meta.R`,
  `man/copy_dlw_metadata.Rd`, `NEWS.md`, `NAMESPACE`, generated package
  metadata, and `roadmap.json` through `@cg-roadmap`
- **Details**:
  1. Delete `R/pipdata_copy_dlw_meta.R`; do not move it to `old_files/`.
  2. Delete `man/copy_dlw_metadata.Rd` explicitly. Do not rely on roxygen to
     remove it.
  3. Add a `NEWS.md` bullet under `## Breaking changes` stating that
     `copy_dlw_metadata()` was removed because its cross-release artifact and
     state-safety contract was obsolete, and that no supported replacement is
     introduced.
  4. Run roxygen once after explicit source/Rd deletion. Do not manually edit
     `NAMESPACE`.
  5. Read the complete final NAMESPACE with `readLines()` and require
     `identical(final_lines, expected_lines)`, where `expected_lines` is the
     baseline ordered vector with exactly `export(copy_dlw_metadata)` removed.
     This is line-for-line after platform line-ending normalization, not raw-byte
     equality. Supplement it with installed `getNamespaceExports("pipdata")`
     inspection.
  6. Run the same UTF-8-aware Rd harness as baseline. Require no new normalized
     diagnostic on any surviving page; the removed page and its diagnostics are
     excluded from the expected final map.
  7. Parse `_pkgdown.yml` locally; do not add pkgdown to `DESCRIPTION` or require
     a local site build when Pandoc is unavailable. Treat public-site removal as
     pending the next successful pkgdown build/deployment, not as a local
     completion claim.
  8. Build the final package tarball and run `R CMD check --no-examples`
     against the same isolated dependency library and `PIP_ROOT_DIR` used for
     the baseline. Require no new normalized error/warning message.
  9. Verify the installed namespace and help index do not expose the removed
     symbol.
  10. Record executed evidence in the work report. Through `@cg-roadmap`, update
      `audit-copy-dlw-meta` to record the audited disposition: retired because
      it had no callers and stale/unsafe contracts; no replacement introduced.
      Move the feature to done only after required evidence passes.
- **Test Scenarios**:
  - Happy path: package loads, target symbol/help are absent, and all other
    exports remain.
  - Edge case: pre-existing package-check messages remain unchanged and are
    recorded separately; historical documentation references remain history.
  - Error path: generated namespace removes another export, stale help remains,
    or package checks gain a new error/warning; restore plan status to blocked.
- **Tests**:
  - `devtools::document(roclets = c("rd", "namespace"))`
  - `tools::checkRd(tools::parse_Rd(path, encoding = "UTF-8"), def_enc = TRUE)`
    over every `man/*.Rd`, compared by path to baseline diagnostics
  - `R CMD build --no-build-vignettes --no-manual <repository>`
  - Tarball `R CMD check --no-examples --no-manual --no-build-vignettes` using
    the exact baseline library/environment; do not use `--run-dontrun`.
- **Acceptance criteria**: V1-V6 pass, no storage/network operation occurs, and
  the roadmap feature is eligible to move to done.

## Testing Strategy

No new behavior test is needed for a deleted function. Verification focuses on
negative API evidence and package integrity:

1. Run a pre-change caller/help/ordered-`readLines()` NAMESPACE inventory.
2. Build and check the unchanged tarball with `--no-examples`; preserve logs,
   versions, `.libPaths()`, and environment evidence.
3. Explicitly delete source and Rd, then regenerate once.
4. Compare complete NAMESPACE and generated help topics.
5. Compare UTF-8 parsed Rd diagnostics by path; permit no new diagnostic.
6. Build and check the final tarball with the identical isolated environment
   and `--no-examples`; compare normalized messages.
7. Inspect the installed package namespace/help, not only source files.
8. Store all completion-relevant baseline/final evidence and comparisons in the
   durable work report; temporary log paths are supplementary only.

Use an isolated temporary `PIP_ROOT_DIR` matching the existing pkgdown CI shape
so package initialization cannot resolve to production storage. Do not execute
the removed function as part of verification.

## Documentation Checklist

- [ ] `R/pipdata_copy_dlw_meta.R` removed.
- [ ] `man/copy_dlw_metadata.Rd` explicitly removed.
- [ ] `NAMESPACE` regenerated with only the target export removed.
- [ ] `NEWS.md` breaking-change bullet added.
- [ ] `_pkgdown.yml` parses and requires no reference-index edit.
- [ ] Next pkgdown build will omit the removed topic; live deployment is not
  claimed by local completion.
- [ ] No active vignette, README, or example references the removed symbol.

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Unknown external script calls the exported symbol | Public API break | User recorded no known use; stop if new evidence appears; document removal prominently in NEWS. |
| Roxygen leaves the old help page | Removed API still appears installed | Delete `man/copy_dlw_metadata.Rd` explicitly before regeneration and inspect installed help. |
| NAMESPACE regeneration removes unrelated directives | Package API or dispatch regression | Compare ordered `readLines()` vectors; require exactly one removed export line. |
| Package check resolves wrong dependency versions | False pass or unrelated failure | Reuse one explicit isolated checker library and record exact versions for baseline/final. |
| Verification initializes production storage | Operational side effect | Set isolated `PIP_ROOT_DIR`; never call the utility or working-release setup. |
| Ordinary package examples call working-release/storage code | Unsafe verification | Use `R CMD check --no-examples`; namespace/help evidence proves removal. |
| Existing package-check messages obscure the result | Ambiguous completion | Preserve contemporaneous baseline logs and compare normalized messages. |
| Hosted pkgdown page remains until deployment | Premature public-doc claim | State that sources and next build are ready; deployment remains a post-merge gate. |

## Out of Scope

- A replacement metadata-copy utility.
- Deprecation shims or internal retention.
- Stamp catalog/history/provenance migration.
- Changes to `pipload`, `pipfun`, or `stamp` APIs.
- Cleanup of any other exported or internal function.
- `\dontrun{}` remediation outside the removed utility example; that is A3.2.
- Full local pkgdown site rendering when Pandoc is unavailable.

## Completion Contract

### Outcome

`copy_dlw_metadata()` is absent from source, namespace, installed help, and
package documentation sources. The next successful pkgdown build will omit its
page; live-site deployment is not claimed by local completion. No replacement
copier or unrelated API change is introduced.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | No executable caller exists before removal | Targeted source/test/vignette search recorded in work report | yes |
| V2 | Source, export, and stale Rd topic are absent | File checks, regenerated `NAMESPACE`, installed namespace/help inspection | yes |
| V3 | Breaking removal is documented | `NEWS.md` under `## Breaking changes` | yes |
| V4 | Package namespace/help, Rd diagnostics, and tarball have no new check message | Durable baseline/final ordered NAMESPACE, UTF-8 Rd diagnostics, build, tarball `R CMD check --no-examples` | yes |
| V5 | Existing pkgdown configuration needs no manual index edit and next build omits topic | `_pkgdown.yml` parse and removed installed topic | yes |
| V6 | Roadmap records retirement as the audited disposition | Targeted `roadmap.json` read after `@cg-roadmap` | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | Do not archive or internalize the utility. | Source and help are absent, not relocated. |
| C2 | Do not design a replacement copier. | Diff contains no new copy API. |
| C3 | Preserve every other NAMESPACE directive and generated help topic. | Ordered normalized NAMESPACE and help comparison. |
| C4 | Do not touch network or persistent storage during verification. | Isolated `PIP_ROOT_DIR`; `R CMD check --no-examples`; no working-release calls. |

### Boundaries

- Allowed: target source/Rd deletion, generated namespace change, NEWS note,
  package verification, work report, and roadmap status through `@cg-roadmap`.
- Out of scope: replacement design, deprecation/internalization, dependency API
  changes, provenance migration, and unrelated cleanup.

### Iteration Policy

1. Stop if a real caller or operational use appears.
2. Capture a safe contemporaneous baseline before deleting anything.
3. Delete the stale Rd page explicitly before roxygen regeneration.
4. Fix only regressions caused by removal.
5. Record pre-existing package-check messages separately; do not hide new ones.
6. Under `deviation-policy: ask`, obtain approval before crossing a boundary.

### Blocked-Stop Conditions

- A real operational dependency or active caller is discovered.
- Another active function requires `copy_dlw_metadata()`.
- A safe pre-deletion build/check baseline cannot be captured.
- Completion-relevant baseline/final evidence cannot be recorded durably in the
  work report.
- Verification would require network or persistent storage mutation.
- Removal causes a behavior/API fix outside this plan.
- Any required evidence fails after bounded recovery attempts.
- The work report cannot be created or updated durably.
