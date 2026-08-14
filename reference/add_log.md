# Add errors to the package environment

Add errors to the package environment

## Usage

``` r
add_log(line, error = NULL, class = "piperr")
```

## Arguments

- line:

  line to be added to the log

- error:

  name of error or warning list

- class:

  PIP error or warning class. Values are stored in `.pipdataenv` under
  the key `paste0("log_", class)`. Currently used values: `"piperr"`
  (stored as `"log_piperr"`) and `"unk_err"` (stored as
  `"log_unk_err"`). Retrieve with `pd_env_get(paste0("log_", class))`.

## Value

Updated error list stored in `.pipdataenv` under
`paste0("log_", class)`.
