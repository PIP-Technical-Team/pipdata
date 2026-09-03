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
| [`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md) | Incrementally run the durable clean, metadata, and deflate stages |
| [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md) | Compatible clean/metadata stage wrapper; returns the master inventory |
| [`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md) | Batch-deflate every survey in the master inventory |
| [`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md) | Data-driven validation engine (replaces per-module validators) |
| [`pd_cpfw_merge()`](https://pip-technical-team.github.io/pipdata/reference/pd_cpfw_merge.md) | Merge country PFW metadata with DLW data |
| [`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md) | Clean main welfare and demographic variables |
| [`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md) | Deflate welfare values using CPI/PPP data |
| [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md) | Attach auxiliary metadata (CPI, PPP, population, GDP, PCE) as attributes |
| [`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md) | Read-only staged dependency report; never loads household artifacts |

## Incremental staged pipeline

After auxiliary refresh and DLW acquisition/validation, run:

``` r

result <- pd_run_pipeline(verbose = FALSE)
```

The executor has exactly three durable nodes: `clean:<survey_id>`,
`metadata:<pip_id>`, and `deflate:<pip_id>`. Internal load, PFW merge,
recode, auxiliary attachment, and save functions are fingerprint
components, not independent cached artifacts. Selected nodes are
reported as current or stale/forced, then cached, runnable, successful,
failed, skipped, or blocked. An unchanged rerun is fully cached and does
not load cached household data.

Stamp is authoritative for immutable artifact versions and exact
receipts. The release-scoped C2 dependency manifest is the only
pipdata-owned currentness and provenance index. A worker is successful
only after its exact receipt, inventories, and manifest checkpoint are
finalized.
[`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md)
uses the same metadata-only facts for read-only inspection.

Auxiliary invalidation uses exact keys. A Colombia 2018 CPI change runs
only matching Colombia 2018 metadata and deflate nodes; another Colombia
year and an unrelated country/year remain cached. `force = TRUE`
rebuilds the full selected graph. `force_surveys` adds only selected
survey or PIP chains while preserving independently stale work. Legacy
state is never adopted implicitly: use `bootstrap = TRUE` and a
restrictive `bootstrap_entities` canary before a complete baseline
rebuild.

Recoverable entity failures block only descendants. Integrity failures
stop later writes. Restart creates a new authoritative plan from exact
Stamp facts and the last valid manifest. There is no persisted run
cursor and no exactly-once guarantee. Existing
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
and
[`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md)
signatures, aliases, positional behavior, and master inventory returns
remain compatible;
[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md)
intentionally returns a compact `pipdata_pipeline_result`.

Production must set `pipdata.dependency_manifest_path` to durable shared
storage. Production activation is explicitly blocked until signed target
Windows/SMB fencing and immutable unique-rename evidence are complete.
Local temporary-directory tests do not satisfy this requirement.

## Documentation

Browse the package documentation at
<https://pip-technical-team.github.io/pipdata/>.

## License

MIT
