# Create a named vector of attributes

Create a named vector of attributes

## Usage

``` r
num_vars_to_attr(df, num_var, name_var)
```

## Arguments

- df:

  A data.frame

- num_var:

  Column name with numerical values

- name_var:

  Column name with name values

## Value

Data.table with named attributes

## Examples

``` r
if (FALSE) { # \dontrun{
 dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
 out <- num_vars_to_attr(dt, "a", "c")
} # }
```
