# Get path to pipdata original files

pipdata comes bundled with a number of internal datasets originally
created in CSV format and then converted to proper R format. They are
placed in its `inst/extdata` directory. This function make them easy to
access. This function is based (mainly copied) from `readr_example` in
the `readr` package

## Usage

``` r
pipdata_int(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the internal files will be listed.

## Examples

``` r
pipdata_int()
#> [1] "dlw_pip_var_type.csv" "pip_pc_var_type.csv"  "recode_spec.yml"     
#> [4] "validation_spec.yml" 
pipdata_int("pip_pc_var_type.csv")
#> [1] "/home/runner/work/_temp/Library/pipdata/extdata/pip_pc_var_type.csv"
```
