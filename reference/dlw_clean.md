# Clean data from datalibweb structure (lower level, S3 methods)

Clean data from datalibweb structure (lower level, S3 methods)

## Usage

``` r
dlw_clean(
  df,
  verbose = getOption("pipdata.verbose", TRUE),
  recode_spec = NULL,
  ...
)
```

## Arguments

- df:

  data.table

- verbose:

  Logical. Print progress messages. Default:
  `getOption("pipdata.verbose", TRUE)`.

- recode_spec:

  Optional pre-resolved recode spec (see
  [`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md)).

- ...:

  other parameters

## Value

data.table
