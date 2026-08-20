# pipdata (development version)

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

* Remove `lubridate` dependency 

# pipdata 0.0.0.9015

* Introduce new functions to modify `pipdata`
