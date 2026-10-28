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
  checkpoint_seconds = Inf,
  recipes = NULL
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

- recipes:

  Optional opt-in M2 envelope with exactly `context` and `nodes`.
  `context` supplies nonempty `release`, `identity = "TEST"`, and an
  existing absolute E-drive `root` already initialized in stamp. `nodes`
  is an unnamed list, including an empty list. Each node requires
  `path`, `parents`, `calculation`, `helpers`, `constants`,
  `semantic_schema`, `semantic`, and lowercase `format` (`rds`, `qs2`,
  or `fst`); only `audit` and `review` are optional. See the M2 section
  below. `NULL` preserves the legacy route.

## Value

A visible `pipdata_pipeline_result`. This differs intentionally from the
legacy stage wrappers, which continue to return master inventories. With
`recipes`, a data.table of stamp feedback instead: `release_id`, `path`,
`preview_decision`, `status`, `reason`, `version_id`, `request_id`,
`receipt_id`, `outcome_id`, `failure_class`, `governing_failure_id`,
`message`, and actual `attempted`. Unavailable proof IDs are
`NA_character_`.

## Details

The following staged-pipeline description applies when `recipes = NULL`.
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

The legacy route always uses the canonical auxiliary measures `pfw`,
`cpi`, `ppp`, `pop`, `gdp`, and `pce`. Production activation remains
blocked until signed target Windows/SMB fencing and immutable
unique-rename evidence are complete.

With `recipes`, pipdata supplies a complete request set to public
[`stamp::st_plan_rebuild()`](https://randrescastaneda.github.io/stamp/reference/st_plan_rebuild.html)
and
[`stamp::st_rebuild()`](https://randrescastaneda.github.io/stamp/reference/st_rebuild.html).
Stamp alone selects and revalidates run/skip/blocked work; legacy
manifests and selectors are not used. Nondefault legacy controls are
rejected. All selected outputs are targets, including multiple roots and
intermediate nodes. Parents are unnamed lists of exactly `path` and
`version_id`; pins must be explicit nonempty character IDs, never
numeric or mutable selectors. `NULL` is allowed only for a selected
upstream recipe and is resolved by stamp during this call. Cycles are
rejected.

Calculations have exactly `(path, parents)` formals and return exactly
one named `x` (including `NULL` when supported), optionally `metadata`,
`format`, `code`, and `code_label`. Format must agree with the request.
Declared semantic values are passed as metadata; supplied semantic
metadata must match exactly. Ordinary R helper closures and complete
immutable constants are explicitly declared in disjoint named lists.
Deep copies run in a locked environment whose parent is the base
environment, not their original capturing environment. Fingerprints are
SHA-256 over serialized canonical bodies/formals and values; neither
caller digests nor package-version substitutes are accepted. Changes to
helpers/constants invalidate the request; audit-only edits do not.

Only reviewed pure base operations, declared helper calls, exact-version
public pipload/stamp reads and explicit stamp-classified failures are
supported. Dynamic lookup/dispatch, primitives as components, mutable
run-state captures, hidden global dependencies, package-internal calls
and direct side effects are rejected. This is a trusted declarative
callback contract, not a security sandbox, arbitrary legacy closure
support, or automatic dependency discovery. Read callbacks must consume
the supplied exact parents, not mutable selectors. A guarded public-read
boundary enforces the effective parent path/pin pair and explicit
physical format before reading. Ambient locale/timezone conversions and
unsupported S3 generics are rejected. Reviewed base operations reject
classed operands; explicitly unclass reviewed input values before
operations. The read/base/closure policy code is also SHA-256 bound into
request proof. Recursive guarded operations also reject classed elements
nested in lists, so registered methods cannot silently supply undeclared
value dependencies. The route checks consumed public producer exports
and explicit arguments before recipe preparation, lease acquisition or
rebuild; version labels alone do not establish those capabilities.
Nonempty recipes require the suggested package `codetools` for proof
generation. If unavailable, an actionable
`pipdata_engine_dependency_error` requests installation in an approved
library; no proof bypass, automatic installation or write-side work is
performed.

A separate lease is scoped to release, TEST identity, and canonical
root; changing a calculation never creates a different writer scope.
Frozen proof, root/options and ownership are checked before and after
callbacks, including exceptions. A latched guard prevents later callback
attempts. Required success proof is read through returned exact receipt
IDs and same-pin metadata, never a catalog/latest substitute. Integrity
failures raise `pipdata_engine_integrity_error` carrying actual
`feedback` and `causes`; cancellation remains cancellation even if
cleanup also fails.

Deterministic and unknown node failures block descendants but not
independent work. External failures retry only on a later explicit call.
`rlang::abort(..., class = "stamp_deterministic_failure")` and
`rlang::abort(..., class = "stamp_external_failure")` signal the
corresponding classified calculation failures; unclassified errors are
unknown failures. Review is explicit
`list(outcome_id = governing_failure_id)` plus a real parent/component
change; review alone is not force. Governing IDs are recovered from
exact committed outcome history across repeated blocks and process
restarts. Retained-output recalculation can return `unchanged` with a
new receipt but the same output pin; descendants can then remain
skipped. No in-call retry or run cursor is created. This bounded TEST
route does not activate production stages or certify SMB, concurrency
beyond the local lease, full-suite coverage, or umbrella closure.

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
