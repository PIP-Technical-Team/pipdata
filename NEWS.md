# pipdata 0.0.1

## Breaking changes

* Remove `copy_dlw_metadata()` because its cross-release artifact and
  state-safety contracts were obsolete. No supported replacement metadata copier
  is introduced.

* `pipdata_dlw_process()`, `pipdata_get_gmd()`, and
  `pipdata_validate_gmd()` no longer accept `log` or `save_log` arguments.
  Logging is unconditional and writes typed entries to `"pipdata_log"`.
  Checkpoints are saved automatically at DLW and pipeline stage boundaries,
  and `log_report()` now covers DLW acquisition and validation alongside
  survey cleaning and deflation.

* `pipdata_get_gmd()` and `pipdata_validate_gmd()` now invisibly return plain
  six-field stage results (`stage`, `outcome`, `inventory`, `summary`,
  `failures`, and `artifacts`). `pipdata_dlw_process()` now invisibly returns a
  plain aggregate with its acquisition and validation results, compact wrapper
  failures, and checkpoint facts. Outcomes are `success`, `partial`, `failed`,
  or `no_work`; nested disabled or dependency-blocked stages use `not_run`.
  Code that assigns these calls should inspect `outcome` rather than expecting
  `NULL`.

* After argument and working-release preconditions succeed, DLW runtime
  failures now return inspectable failed or partial results instead of
  necessarily aborting the caller. Invalid arguments, missing setup
  preconditions, interactive cancellation, and interrupts still escape. This
  means an unassigned noninteractive script can continue after a failed DLW
  result and should use the returned outcome when continuation is conditional.

* `gmd_valid_inv` now stores completed validation state only: available rows
  classified `valid` or `invalid`. Execution failures are absent and retry by
  absence. `pd_process_data()`, internal dependency execution, and
  `pd_change_report()` filter recognized legacy blank/unavailable control rows
  before planning; malformed completed rows fail validation instead of entering
  cleaning.

## New features

* Add a staged dependency manifest and read-only `pd_change_report()` covering
  clean, auxiliary metadata, and exact-input deflation provenance. State is
  release/identity/repository scoped, immutable by generation, checksum
  verified, and protected by a fencing lease.

* Add explicit resumable legacy bootstrap controls (`bootstrap` and
  `bootstrap_entities`) and exact data/metadata version loading for pipeline
  deflation. Missing provenance never triggers an implicit rebuild.

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

* Acquisition now actively downloads five modules (`ALL`, `GROUP`, `HIST`,
  `GPWG`, and `BIN`), while catalog and validation handling recognizes seven
  modules by also mapping `ASPIRE` and `L`. Each selected download forces
  replacement of the exact catalog filename. The acquisition inventory is
  reconciled to the authoritative current catalog, including no-worker runs,
  and unresolved current five-module rows retry when `check_missing = TRUE`.

* DLW persistence now treats thrown, null-version, and malformed write returns
  as uncertain. Acquisition inventory, validation report/inventory, direct
  inventory utility, and checkpoint paths reload durable state and compare
  canonical prior and intended content rather than assuming rollback.

* Validation now calls the data-driven `dlw_validation_engine()` for one survey
  at a time using all seven module mappings. Invalid classification counts as a
  completed validation, separately from execution failure. The next completed
  `pipeline_version` comes from the maximum persisted history for that survey,
  including superseded checksums; failed attempts consume no version.

* Validation inventory and report are reconciled to current available
  acquisition keys on every run. The report exactly covers completed inventory
  IDs, exact normalized rows are deduplicated, and report content is verified
  before the completed inventory commit. Unreadable or ambiguous history is not
  overwritten. Every catalog-listed history version must now be readable and
  schema-valid, report ordering uses all persisted columns, and optional-column
  compatibility includes coercion-relevant attributes.

* `pipdata_dlw_process()` remains the supported DLW entry point and routes a
  custom `inv_gmd_list` through acquisition, bootstrap, and validation.
  Validate-only execution never opens a menu; missing inventories under
  automation return failed stage results. Validation can continue after an
  acquisition failure when a trustworthy durable inventory remains. Aggregate
  outcome and summary logging come from stage facts, while checkpoint failure
  remains separate from the business outcome. A broader `run_pipeline()` API is
  future direction only.

* `log_report()` now segments acquisition and validation independently from
  each latest attempt boundary, prefers exact completion entries, and confines
  legacy fallback to that segment. Dedicated DLW sections own all DLW
  discriminators; generic type and country sections exclude them to prevent
  stale-history leakage and double counting.

## Maintenance

* Remove `lubridate` dependency.

* Fix pkgdown CI configuration and article references.

# pipdata 0.0.0.9015

* Introduce new functions to modify `pipdata`
