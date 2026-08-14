# Make vars as attributes

Make vars as attributes

## Usage

``` r
vars_to_attr(df, vars)
```

## Arguments

- df:

  A data.frame

- vars:

  variables to changed to attributes

## Value

A data.frame with vars variables as attributes

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- data.table(a = c(1, 2), b = 1:10, c = 5)
out <- vars_to_attr(dt, "a")
} # }
```
