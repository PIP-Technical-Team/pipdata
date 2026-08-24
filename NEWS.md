# pipdata 0.0.1

## Breaking changes

* `pipdata_dlw_process()`, `pipdata_get_gmd()`, and
  `pipdata_validate_gmd()` no longer accept `log` or `save_log` arguments.
  Logging is unconditional and writes typed entries to `"pipdata_log"`.
  Checkpoints are saved automatically at DLW and pipeline stage boundaries,
  and `log_report()` now covers DLW acquisition and validation alongside
  survey cleaning and deflation.

## New features

* Add `pd_deflate_pipeline()`: a batch orchestrator that iterates over the
  master inventory, deflates each survey via `pd_deflation()`, saves results
  to the `"pip_deflated"` stamp alias, updates the master inventory with
  deflation provenance columns (`deflated`, `content_hash_deflated`,
  `aux_*_hash_at_deflation`), and logs a structured summary. Supports
  `force = TRUE` to re-deflate already processed surveys.

* Add `force_surveys` parameter to `pd_process_data()`: enables surgical
  re-processing of specific surveys by `survey_id` or `pip_id`, without
  the global stamp versioning side effect of `force = TRUE`. Mutually
  exclusive with `force`.

## Refactoring

* Introduce `dlw_validation_engine()`: a single data-driven validation engine
  that replaces the 7 near-identical per-module DLW validation functions
  (`dlw_validation_gpwg`, `dlw_validation_group`, `dlw_validation_bin`,
  `dlw_validation_hist`, `dlw_validation_all`, `dlw_validation_aspire`,
  `dlw_validation_l`) and `dlw_validation_skip`. Engine behavior is driven by
  `inst/extdata/validation_spec.yml` (corrected selection semantics, per-check
  severity, hhid/pid gating, skip `error_stop`). The legacy functions remain
  as deprecated wrappers calling the engine. Report output matches the legacy
  functions on the deterministic subset (golden fixture-tested via
  `tests/testthat/fixtures/`); `assertion.id`/`error_df` remain
  non-deterministic per data.validator.

## Maintenance

* Remove `lubridate` dependency.

* Fix pkgdown CI configuration and article references.

# pipdata 0.0.0.9015

* Introduce new functions to modify `pipdata`
