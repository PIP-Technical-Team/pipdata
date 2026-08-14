# Clean micro data from Datalibweb original file

Clean micro data from Datalibweb original file

## Usage

``` r
# S3 method for class 'pipmd'
dlw_clean(
  df,
  verbose = getOption("pipdata.verbose", TRUE),
  recode_spec = NULL,
  ...
)
```

## Arguments

- df:

  data frame with micro data,

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
