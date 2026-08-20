# Changelog

## pipdata 0.0.1

### New features

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
