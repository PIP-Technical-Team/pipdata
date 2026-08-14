# Process datalibweb data: merge PFW data and clean variables

Process datalibweb data: merge PFW data and clean variables

## Usage

``` r
process_data(inv, aux_list, recode_spec = NULL, verbose = TRUE, ...)
```

## Arguments

- inv:

  inventory with survey_id and pins folder

- aux_list:

  Named list of auxiliary data frames; expected keys: `"pfw"`, `"cpi"`,
  `"ppp"`, `"pop"`, `"gdp"`, `"pce"`.

- recode_spec:

  Optional pre-resolved recode spec (as returned by
  [`sync_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/sync_recode_spec.md))
  threaded to
  [`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md)/[`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md)
  so the spec is read once upstream rather than once per survey. Default
  `NULL`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

- ...:

  other parameters

## Value

data.table

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

pfw <- pipload::load_aux_data("pfw")

gd  <- pipload::load_aux_data("CHN", 2015)
gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
process_data(gd, pfw)

md   <- pipload::load_aux_data(country = "PRY", 2012)
md  <- survey_id_to_attr(md, unique(md$survey_id))
process_data(md, pfw)
} # }
```
