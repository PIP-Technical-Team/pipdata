# Create cache ID for country PFW

Constructs a `cache_id` string of the form
`{country_code}_{surveyid_year}_{survey_acronym}_{INC|CON}_{module}` and
adds it as a column to `cpfw`. The table is then split by `cache_id` and
returned as a named list — one element per welfare type.

## Usage

``` r
cache_id(att, cpfw)
```

## Arguments

- att:

  Named list of survey attributes (from `attributes(dt)`). Must contain:
  `country_code`, `surveyid_year`, `survey_acronym`, `module`.

- cpfw:

  data.table with country Price Framework, containing at minimum
  `welfare_type` and `reporting_level` columns.

## Value

Named list of data.tables, one element per unique `cache_id`.
