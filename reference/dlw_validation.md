# Validate DLW data (Generic Documentation)

This is a generic validation interface for DLW datasets across different
module types. Specific functions handle validation logic for GPWG,
GROUP, BIN, HIST, ALL, ASPIRE, and L module types.

## Usage

``` r
dlw_validation(dlw_data, svy_id)

dlw_validation_gpwg(dlw_data, svy_id)

dlw_validation_group(dlw_data, svy_id)

dlw_validation_bin(dlw_data, svy_id)

dlw_validation_hist(dlw_data, svy_id)

dlw_validation_all(dlw_data, svy_id)

dlw_validation_aspire(dlw_data, svy_id)

dlw_validation_l(dlw_data, svy_id)

dlw_validation_skip(dlw_data, svy_id)
```

## Arguments

- dlw_data:

  A DLW dataset in `qs` format.

- svy_id:

  A survey identifier extracted from the dataset.

## Value

A data.frame containing validation results.

An empty data.frame with minimal checks applied.

## Functions

- `dlw_validation_gpwg()`: Validate GPWG data

  Performs variable and structural checks on GPWG data, such as
  availability of core variables, non-missingness, valid value ranges,
  and duplication checks.

- `dlw_validation_group()`: Validate GROUP data

  Checks for missing values, type mismatches, and invalid entries in
  GROUP datasets.

- `dlw_validation_bin()`: Validate BIN data

  Performs structural and value-based validation for BIN datasets,
  checking numeric, character, and key variable consistency.

- `dlw_validation_hist()`: Validate HIST data

  Conducts data validation for HIST datasets, including checks for key
  variables like `urban`, `weight`, and `welfare`, as well as common
  structural validations.

- `dlw_validation_all()`: Validate ALL data

  Validates general ALL module type data containing core variables such
  as `welfare`, `weight`, and optionally `urban`. Ensures basic
  structure and NA thresholds.

- `dlw_validation_aspire()`: Validate ASPIRE data

  Handles validation for ASPIRE DLW datasets by checking structure and
  numeric variable consistency. Special attention is paid to `hhweight`,
  `urban`, and household size.

- `dlw_validation_l()`: Validate Labor (L) DLW data

  Validates DLW datasets containing labor-specific data, such as
  employment status (`lstatus`, `empstat`), person-level identifiers
  (`hhid`, `pid`), and working hours (`whours`).

- `dlw_validation_skip()`: Skip Validation

  Used for DLW modules that require no validation. Ensures only that the
  dataset is not blank.

## Examples

``` r
if (FALSE) { # \dontrun{
dlw_validation_gpwg(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_group(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_bin(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_hist(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_all(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_aspire(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_l(
  dlw_data = "data/dlw_qs",
  svy_id = "survey_id",
)
} # }
if (FALSE) { # \dontrun{
dlw_validation_skip(
  dlw_data = "data/dlw_qs",
  svy_id = survey_id
)
} # }
```
