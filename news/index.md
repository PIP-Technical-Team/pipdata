# Changelog

## pipdata 0.0.1

### Breaking changes

- Remove `copy_dlw_metadata()` because its cross-release artifact and
  state-safety contracts were obsolete. No supported replacement
  metadata copier is introduced.

- [`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
  [`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md),
  and
  [`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md)
  no longer accept `log` or `save_log` arguments. Logging is
  unconditional and writes typed entries to `"pipdata_log"`. Checkpoints
  are saved automatically at DLW and pipeline stage boundaries, and
  [`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
  now covers DLW acquisition and validation alongside survey cleaning
  and deflation.

### New features

- Add a staged dependency manifest and read-only
  [`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md)
  covering clean, auxiliary metadata, and exact-input deflation
  provenance. State is release/identity/repository scoped, immutable by
  generation, checksum verified, and protected by a fencing lease.

- Add explicit resumable legacy bootstrap controls (`bootstrap` and
  `bootstrap_entities`) and exact data/metadata version loading for
  pipeline deflation. Missing provenance never triggers an implicit
  rebuild.

- Add
  [`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md):
  a batch orchestrator that iterates over the master inventory, deflates
  each survey via
  [`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
  saves results to the `"pip_deflated"` stamp alias, updates the master
  inventory with deflation provenance columns (`deflated`,
  `content_hash_deflated`, `aux_*_hash_at_deflation`), and logs a
  structured summary. Supports `force = TRUE` to re-deflate already
  processed surveys.

- Add `force_surveys` parameter to
  [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md):
  enables surgical re-processing of specific surveys by `survey_id` or
  `pip_id`, without the global stamp versioning side effect of
  `force = TRUE`. Mutually exclusive with `force`.

### Refactoring

- Introduce
  [`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md):
  a single data-driven validation engine that replaces the 7
  near-identical per-module DLW validation functions
  (`dlw_validation_gpwg`, `dlw_validation_group`, `dlw_validation_bin`,
  `dlw_validation_hist`, `dlw_validation_all`, `dlw_validation_aspire`,
  `dlw_validation_l`) and `dlw_validation_skip`. Engine behavior is
  driven by `inst/extdata/validation_spec.yml` (corrected selection
  semantics, per-check severity, hhid/pid gating, skip `error_stop`).
  The legacy functions remain as deprecated wrappers calling the engine.
  Report output matches the legacy functions on the deterministic subset
  (golden fixture-tested via `tests/testthat/fixtures/`);
  `assertion.id`/`error_df` remain non-deterministic per data.validator.

### Maintenance

- Remove `lubridate` dependency.

- Fix pkgdown CI configuration and article references.

## pipdata 0.0.0.9015

- Introduce new functions to modify `pipdata`
