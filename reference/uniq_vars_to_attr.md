# convert variables with unique values along the data set to attributes and then remove those unique variables

convert variables with unique values along the data set to attributes
and then remove those unique variables

## Usage

``` r
uniq_vars_to_attr(x, exclude_vars = NULL)
```

## Arguments

- x:

  a data.frame

- exclude_vars:

  variables to be excluded from turning to attributes (default NULL)

## Value

data.frame with multiple-value variables only and single-value variables
as attributes

## Examples

``` r
dt <- data.table::data.table(a = 1, b = 1:10, c = 5)
out <- uniq_vars_to_attr(dt)
out[]
#>         b
#>     <int>
#>  1:     1
#>  2:     2
#>  3:     3
#>  4:     4
#>  5:     5
#>  6:     6
#>  7:     7
#>  8:     8
#>  9:     9
#> 10:    10
attr(out, "a")
#> [1] 1
attr(out, "c")
#> [1] 5

# Exclude `a` from being added as attribute
out <- uniq_vars_to_attr(dt, "a")
out[]
#>         a     b
#>     <num> <int>
#>  1:     1     1
#>  2:     1     2
#>  3:     1     3
#>  4:     1     4
#>  5:     1     5
#>  6:     1     6
#>  7:     1     7
#>  8:     1     8
#>  9:     1     9
#> 10:     1    10

# var `a` is not included as part of the attributes
attr(out, "a")
#> [1] 1

# Var `c` is
attr(out, "c")
#> [1] 5
```
