# Run the staged PIP data pipeline incrementally

Runs the durable `clean`, `metadata`, and `deflate` stages in
topological waves under one dependency-manifest writer lease. Current
nodes are returned as cached units. Downstream waves are accepted only
after committed upstream receipts have been reloaded into the
authoritative dependency facts.

## Usage

``` r
pd_run_pipeline(
  inv = NULL,
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  force_surveys = NULL,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  checkpoint_size = 25L,
  checkpoint_seconds = Inf
)
```

## Arguments

- inv:

  The complete completed-validation inventory. If `NULL`, the current
  durable validation inventory is loaded with
  [`pipload::load_gmd_valid_inv()`](https://pip-technical-team.github.io/pipload/reference/load_dlw_data.html).

- force:

  Logical scalar. Rebuild all selected nodes and temporarily use Stamp
  timestamp versioning. Mutually exclusive with `force_surveys`.

- verbose:

  Logical scalar passed to pipeline storage operations.

- force_surveys:

  Optional character vector of exact `survey_id` or `pip_id` selectors.
  Selected chains are added to ordinary invalidation.

- bootstrap:

  Logical scalar. Explicitly permit unknown C2 provenance.

- bootstrap_entities:

  Optional character vector of bootstrap survey or PIP selectors. A PIP
  selector includes its owning clean survey and complete atomic output
  chain. Requires `bootstrap = TRUE`.

- checkpoint_size:

  Positive whole-number metadata and deflate checkpoint batch size.

- checkpoint_seconds:

  `Inf` or a positive numeric checkpoint interval in seconds.

## Value

A visible `pipdata_pipeline_result`. This differs intentionally from the
legacy stage wrappers, which continue to return master inventories.

## Details

The C2 dependency manifest and exact Stamp receipts are the only
currentness authority. The function does not persist a run cursor. A
restart creates a new run and replans from the latest valid manifest.
Recoverable entity errors block only their descendants. Unknown storage,
lease, fence, receipt, and checkpoint failures stop later writes and are
captured only after a complete stage context exists. Interrupts and
explicit cancellation conditions always propagate.

The only durable nodes are `clean:<survey_id>`, `metadata:<pip_id>`, and
`deflate:<pip_id>`. Load, PFW merge, recode, auxiliary attachment, and
save helpers are code-fingerprint components, not separately cached
nodes. Each selected node is reported as current or stale/forced and
then as cached, runnable, successful, failed, skipped, or blocked.
Cached clean nodes do not load household artifacts.

`force = TRUE` rebuilds the complete selected graph. `force_surveys`
adds the selected survey or PIP chain to ordinary invalidation without
suppressing unrelated stale work. An absent manifest or unknown pre-C2
provenance requires `bootstrap = TRUE`; use `bootstrap_entities` for a
restrictive canary before a complete baseline rebuild.

Auxiliary invalidation is keyed. For example, a CPI change for Colombia
2018 refreshes only matching Colombia 2018 metadata and deflate nodes;
another Colombia year and unrelated country/year nodes stay cached.
Worker completion is not success until exact receipts, inventories, and
a manifest checkpoint are finalized. Recoverable entity failures block
only their descendants. A later call resumes by authoritative replan
from Stamp and the last valid manifest, without a persisted run cursor
or an exactly-once guarantee.

The top-level API always uses the canonical auxiliary measures `pfw`,
`cpi`, `ppp`, `pop`, `gdp`, and `pce`. Production activation remains
blocked until signed target Windows/SMB fencing and immutable
unique-rename evidence are complete.

## See also

Other pd_process_data pipeline:
[`add_attr()`](https://pip-technical-team.github.io/pipdata/reference/add_attr.md),
[`aux_hash_candidates()`](https://pip-technical-team.github.io/pipdata/reference/aux_hash_candidates.md),
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md),
[`create_attr()`](https://pip-technical-team.github.io/pipdata/reference/create_attr.md),
[`data_to_dt()`](https://pip-technical-team.github.io/pipdata/reference/data_to_dt.md),
[`filter_aux_data()`](https://pip-technical-team.github.io/pipdata/reference/filter_aux_data.md),
[`filter_aux_inv()`](https://pip-technical-team.github.io/pipdata/reference/filter_aux_inv.md),
[`fix_year_var()`](https://pip-technical-team.github.io/pipdata/reference/fix_year_var.md),
[`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md),
[`inv_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/inv_dlw_load.md),
[`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md),
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md),
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md),
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
[`resolve_force_surveys()`](https://pip-technical-team.github.io/pipdata/reference/resolve_force_surveys.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260831", "TEST")
result <- pd_run_pipeline(verbose = FALSE)
print(result)
} # }
```
