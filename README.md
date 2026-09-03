# pipdata

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/PIP-Technical-Team/pipdata)

`pipdata` prepares and processes survey data for the PIP (Poverty and Inequality
Platform) pipeline. It handles the end-to-end workflow from raw DLW (DataLibWeb)
data to cleaned, deflated, and validated micro-data ready for poverty and
inequality calculations.

## Version 1.0.0

Version 1.0.0 adds `pd_run_pipeline()` as the top-level incremental executor
for clean, metadata, and deflate stages. It uses exact Stamp receipts and a
release-scoped dependency manifest to run only stale or explicitly forced
nodes.

This release changes the public DLW contracts. Assign the result from
`pipdata_dlw_process()`, `pipdata_get_gmd()`, or `pipdata_validate_gmd()` and
inspect its `outcome` before conditional continuation. These functions no
longer accept `log` or `save_log`; logging is unconditional.
`copy_dlw_metadata()` was removed without a replacement. See the
[changelog](https://pip-technical-team.github.io/pipdata/news/index.html) for
the full migration notes.

## Installation

```r
# install.packages("remotes")
remotes::install_github("PIP-Technical-Team/pipdata")
```

## Key functions

| Function | Purpose |
|---|---|
| `pipdata_dlw_process()` | Acquire and validate DLW surveys and return an inspectable aggregate outcome |
| `pd_run_pipeline()` | Incrementally run the durable clean, metadata, and deflate stages |
| `pd_process_data()` | Compatible clean/metadata stage wrapper; returns the master inventory |
| `pd_deflate_pipeline()` | Batch-deflate every survey in the master inventory |
| `dlw_validation_engine()` | Data-driven validation engine (replaces per-module validators) |
| `pd_cpfw_merge()` | Merge country PFW metadata with DLW data |
| `pd_dlw_clean()` | Clean main welfare and demographic variables |
| `pd_deflation()` | Deflate welfare values using CPI/PPP data |
| `pd_aux_attr()` | Attach auxiliary metadata (CPI, PPP, population, GDP, PCE) as attributes |
| `pd_change_report()` | Read-only staged dependency report; never loads household artifacts |

## Incremental staged pipeline

After auxiliary refresh and DLW acquisition/validation, run:

```r
result <- pd_run_pipeline(verbose = FALSE)
```

The executor has exactly three durable nodes: `clean:<survey_id>`,
`metadata:<pip_id>`, and `deflate:<pip_id>`. Internal load, PFW merge, recode,
auxiliary attachment, and save functions are fingerprint components, not
independent cached artifacts. Selected nodes are reported as current or
stale/forced, then cached, runnable, successful, failed, skipped, or blocked.
An unchanged rerun is fully cached and does not load cached household data.

Stamp is authoritative for immutable artifact versions and exact receipts. The
release-scoped C2 dependency manifest is the only pipdata-owned currentness and
provenance index. A worker is successful only after its exact receipt,
inventories, and manifest checkpoint are finalized. `pd_change_report()` uses
the same metadata-only facts for read-only inspection.

Auxiliary invalidation uses exact keys. A Colombia 2018 CPI change runs only
matching Colombia 2018 metadata and deflate nodes; another Colombia year and an
unrelated country/year remain cached. `force = TRUE` rebuilds the full selected
graph. `force_surveys` adds only selected survey or PIP chains while preserving
independently stale work. Legacy state is never adopted implicitly: use
`bootstrap = TRUE` and a restrictive `bootstrap_entities` canary before a
complete baseline rebuild.

Recoverable entity failures block only descendants. Integrity failures stop
later writes. Restart creates a new authoritative plan from exact Stamp facts
and the last valid manifest. There is no persisted run cursor and no
exactly-once guarantee. Existing `pd_process_data()` and
`pd_deflate_pipeline()` signatures, aliases, positional behavior, and master
inventory returns remain compatible; `pd_run_pipeline()` intentionally returns
a compact `pipdata_pipeline_result`.

Production must set `pipdata.dependency_manifest_path` to durable shared
storage. Production activation is explicitly blocked until signed target
Windows/SMB fencing and immutable unique-rename evidence are complete. Local
temporary-directory tests do not satisfy this requirement.

## Documentation

Browse the package documentation at
<https://pip-technical-team.github.io/pipdata/>.

## License

MIT
