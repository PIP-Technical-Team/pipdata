# Pipdata Package

## Objective

This project aims to run the initial steps of the new pipeline of PIP.
That is, the validation of DLW data and cleaning so it can be
transformed in pip data format. The pip data will be later used for
estimations of regional and global poverty and inequality indicators.

## Key Deliverables

- R package improvement

## Constraints

- R packages good standards

## Current Focus

Pipeline Alignment Audit: aligning the pipdata codebase with the
canonical three-wrapper architecture. The subnational deflation fix
(`ppp_data_level = "area"` resolution) and the integration of
`pd_deflation.R` into the active pipeline are both **done** (see
`subnational-deflation-fast-fix` and `integrate-deflation` in
`roadmap.json`), as is the inventory architecture refactor
([`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
replacing `update_pip_inventory()`/`format_vrs()`) and the
pipeline-wrapper documentation vignettes. Remaining open items in this
milestone:
[`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md)
batch orchestrator (deflate-pipeline-wrapper), making `*_data_level`
attribute semantics explicit (explicit-data-level-semantics), auditing
[`copy_dlw_metadata()`](https://pip-technical-team.github.io/pipdata/reference/copy_dlw_metadata.md)
as a standalone admin utility (audit-copy-dlw-meta), auditing
`\dontrun{}` examples (audit-dontrun-examples), a run-manifest logging
evaluation (run-manifest-audit), and a content-hash-based re-clean
trigger (content-hash-reclean-trigger).
