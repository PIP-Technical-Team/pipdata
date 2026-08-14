# Deflation of welfare using auxiliary data

Deflates a single cleaned survey `data.table`. Two input modes:

## Usage

``` r
pd_deflation(
  dt = NULL,
  cpi = NULL,
  ppp = NULL,
  pop = NULL,
  pip_id = NULL,
  version = NULL,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- dt:

  A single cleaned survey `data.table` (class `pipmd` or `pipgd`), or
  `NULL` when `pip_id` is given instead.

- cpi:

  Named numeric vector of CPI values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("cpi")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- ppp:

  Named numeric vector of PPP values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("ppp")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- pop:

  Named numeric vector of population values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("pop")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- pip_id:

  Character scalar. Survey identifier for Mode B (load from stamp).
  Ignored when `dt` is provided.

- version:

  Character scalar or `NULL`. Stamp version used when loading the survey
  (Mode B) or resolving the metadata version from the master inventory.

- verbose:

  Logical. When `TRUE` (the default), informational messages from
  downstream `pipload`/`stamp` I/O calls are shown. Set to `FALSE` to
  suppress them. Defaults to
  `getOption("pipdata.verbose", default = TRUE)`.

## Value

The input survey `data.table` augmented with `welfare_lcu` and
`welfare_ppp_*` columns, and three attributes:

- `welfare_vars`: character vector of all `welfare_*` column names

- `adj_pop`: logical; `TRUE` if population weights were adjusted

- `ppp_sort`: integer base year used for row sorting (e.g. `2017L`), or
  `NULL` when deflation produced no `welfare_ppp_*` columns Returns `NA`
  when deflation fails (error logged via
  [`log_failure()`](https://pip-technical-team.github.io/pipdata/reference/log_failure.md)).

## Details

- **Mode A** (`dt`): pass the cleaned survey directly. When
  `cpi`/`ppp`/`pop` are `NULL`, auxiliary metadata is loaded
  automatically from stamp via the master inventory.

- **Mode B** (`pip_id`): pass a survey identifier and optional stamp
  version. The survey and metadata are both loaded automatically.

To deflate many surveys in a batch, use the future
`pd_deflate_pipeline()` wrapper (tracked in the roadmap as
`deflate-pipeline-wrapper`), which calls `pd_deflation()` for each
survey in an inventory.

## Note

`pd_deflation()` is a single-survey deflation helper. When
`cpi`/`ppp`/`pop` are `NULL` (the default), it resolves the matching
metadata version from the master inventory and loads CPI/PPP/pop
automatically. All package-level environment access uses the unified
`.pipdataenv` via accessor helpers (`pd_env_set()`, `pd_env_get()`,
`pd_env_rm()`).

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
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Mode A: pass survey directly, aux loaded automatically from master inventory
release <- "20250203"
pipfun::setup_working_release(release)
pfw <- pipload::pip_load_aux("pfw")
gd  <- pipload::pip_load_dlw("CHN", 2015)
ls  <- pd_cpfw_merge(gd, pfw)
x   <- pd_dlw_clean(gd)[["CHN_2015_CHIP_INC_D1"]]
pd_deflation(x)

# Legacy Mode A: explicit aux tables
ppp <- pipload::pip_load_aux("ppp")
cpi <- pipload::pip_load_aux("cpi")
pop <- pipload::pip_load_aux("pop")
pd_deflation(x, cpi = cpi, ppp = ppp, pop = pop)

# Mode B: load by survey id
pd_deflation(pip_id = "CHN_2015_CHIP_INC_D1")
} # }
```
