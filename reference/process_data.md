# Process datalibweb data: merge PFW data and clean variables

Process datalibweb data: merge PFW data and clean variables

## Usage

``` r
process_data(inv, aux_list, recode_spec = NULL, verbose = TRUE, ...)
```

## Arguments

- inv:

  A one-row `data.frame` or `data.table` from the completed DLW
  validation inventory containing `survey_id`, `status == "valid"`, and
  `data_available == "Yes"`. Rows missing validation fields or having
  `status != "valid"` or `data_available != "Yes"` are rejected before
  loading.

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

Invisibly, a list with element `pip_names` (character vector of saved
clean `pip_id` names) when cleaning, post-format validation, and saving
succeed, or `NULL` when an error or validation failure is logged and
caught.

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
