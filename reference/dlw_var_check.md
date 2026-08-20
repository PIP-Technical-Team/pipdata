# Validating Specific Conditions of a Variable (Generic Documentation)

This interface serves as a generic check for variables in DLW datasets
across various scenarios. It includes specific functions designed to
assess different conditions, such as determining if a variable is of
character or numeric type, checking the number of reporting levels for
urban/rural variables, verifying if values are greater than zero, and
confirming the availability of a variable within the dataset.

## Usage

``` r
dlw_var_check(val, col_name)

is_character(val, col_name)

is_numeric(val, col_name)

check_urban(val, col_name)

check_gender(val, col_name)

is_greaterthanzero(val, col_name)

is_greaterequale0(val, col_name)

is_var_avail(val, col_name)

is_var_startwith_avail(val, col_name)

is_valuebtwn0and110(val, col_name)
```

## Arguments

- val:

  variable name

- col_name:

  data

## Value

a validation report as text

## Functions

- `is_character()`: Check a variable is character

- `is_numeric()`: Check a variable is numeric

- `check_urban()`: Check residential variable (urban/rural) has more
  than one reporting level in group data

- `check_gender()`: Check gender (male - variable) has more than two
  categories in ALL data

- `is_greaterthanzero()`: Check a numeric variable is greater than 0

- `is_greaterequale0()`: Check a numeric variable is greater than or
  equal to 0

- `is_var_avail()`: Check a variable is available in a dataset with
  specified variable name

- `is_var_startwith_avail()`: Check a variable is available in a dataset
  with variable name starting with a specified text

- `is_valuebtwn0and110()`: Check age is available in a dataset with
  value between 0 and 110

## Examples

``` r
if (FALSE) { # \dontrun{
is_character(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_numeric(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
check_urban(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
check_gender(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_greaterthanzero(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_greaterequale0(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_var_avail(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_var_startwith_avail(
  val = data,
  col_name = variable_name,
)
} # }
if (FALSE) { # \dontrun{
is_valuebtwn0and110(
  val = data,
  col_name = variable_name,
)
} # }
```
