# Clean data from datalibweb structure (High level)

Clean data from datalibweb structure (High level)

## Usage

``` r
pd_dlw_clean(
  ls,
  verbose = getOption("pipdata.verbose", TRUE),
  recode_spec = NULL
)
```

## Arguments

- ls:

  List of data frames or single dataframe.

- verbose:

  Logical. Print progress messages. Default:
  `getOption("pipdata.verbose", TRUE)`.

- recode_spec:

  Optional pre-resolved recode spec (as returned by
  [`sync_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/sync_recode_spec.md))
  threaded down to
  [`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md)
  so the spec is read once upstream instead of once per survey. Default
  `NULL` (each survey reads the spec from stamp).

## Value

list with data.tables

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

pfw  <- pipload::pip_load_aux("pfw")

gd    <- pipload::pip_load_dlw("CHN", 2015)
gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
ls    <- pd_cpfw_merge(gd, pfw)
lf    <- pd_dlw_clean(ls)
names(lf)

md    <- pipload::pip_load_dlw(country = "PHL", 2012)
md  <- survey_id_to_attr(md, unique(md$survey_id))
ls    <- pd_cpfw_merge(md, pfw)
lf    <- pd_dlw_clean(ls)
names(lf)
} # }
```
