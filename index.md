# pipdata

[![Ask
DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/PIP-Technical-Team/pipdata)

`pipdata` prepares and processes survey data for the PIP (Poverty and
Inequality Platform) pipeline. It handles the end-to-end workflow from
raw DLW (DataLibWeb) data to cleaned, deflated, and validated micro-data
ready for poverty and inequality calculations.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("PIP-Technical-Team/pipdata")
```

## Key functions

| Function | Purpose |
|----|----|
| [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md) | Main entry point: iterate over the DLW inventory, merge auxiliary data, clean variables, validate, and save |
| [`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md) | Batch-deflate every survey in the master inventory |
| [`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md) | Data-driven validation engine (replaces per-module validators) |
| [`pd_cpfw_merge()`](https://pip-technical-team.github.io/pipdata/reference/pd_cpfw_merge.md) | Merge country PFW metadata with DLW data |
| [`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md) | Clean main welfare and demographic variables |
| [`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md) | Deflate welfare values using CPI/PPP data |
| [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md) | Attach auxiliary metadata (CPI, PPP, population, GDP, PCE) as attributes |
| [`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md) | Read-only staged dependency report; never loads household artifacts |

## Staged dependency provenance

The dependency layer separates `clean` (`survey_id`), `metadata`
(`pip_id`), and `deflate` (`pip_id`) work. Stamp remains authoritative
for immutable artifact versions; pipdata stores a rebuildable,
release-scoped provenance manifest. Legacy state is never rebuilt
implicitly: inspect
[`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md),
then use explicit `bootstrap = TRUE` and a restrictive
`bootstrap_entities` canary before resuming larger batches.

Production must set `pipdata.dependency_manifest_path` to durable shared
storage and complete the signed Windows/SMB fencing and unique-rename
smoke test documented in `inst/doc/staged-dependency-manifest.md`. Local
cache defaults are development-only and do not indicate production
activation.

## Documentation

Browse the package documentation at
<https://pip-technical-team.github.io/pipdata/>.

## License

MIT
