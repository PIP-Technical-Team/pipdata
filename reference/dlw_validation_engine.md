# DLW Validation Engine

A single data-driven engine that replaces the 7 per-module validation
functions. Reads `inst/extdata/validation_spec.yml` and dispatches
validation checks accordingly.

## Usage

``` r
dlw_validation_engine(dlw_data, svy_id, module)
```

## Arguments

- dlw_data:

  A DLW dataset (data.table).

- svy_id:

  Survey identifier string.

- module:

  Module id (one of: gpwg, group, bin, hist, all, aspire, l, skip).

## Value

A data.table with columns `table_name`, `message`, `type` (invisibly).
Also appends the full validation record to
`pd_env_get("validation_report")`.
