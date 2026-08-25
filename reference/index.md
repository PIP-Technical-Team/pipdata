# Package index

## Pipeline wrappers

The three top-level wrapper functions that make up the pipeline (see the
“Pipeline” articles for the narrative walkthrough).

- [`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
  : Process DLW Data
- [`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md)
  : Retrieve and Save GMD Catalog Datasets to a Local Directory
- [`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md)
  : Validate GMD data and generate inventory report data
- [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
  : Process DLW inventory and create cleaned pip data

## Deflation

Deflates a cleaned survey’s welfare values; run as a separate, post-hoc
step after
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).

- [`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md)
  : Deflation of welfare using auxiliary data
- [`deflation()`](https://pip-technical-team.github.io/pipdata/reference/deflation.md)
  : Deflation of welfare using auxiliary data (lower level)

## Logging

Summarizes the unified `"pipdata_log"` across DLW acquisition,
validation, survey cleaning, and deflation with stage-aware warnings.

- [`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
  : Generate a markdown report from a pipeline log.

## Other functions

- [`add_area()`](https://pip-technical-team.github.io/pipdata/reference/add_area.md)
  : Recode urban to area (lower level, S3 methods)

- [`add_area(`*`<pipgd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/add_area.pipgd.md)
  : Recode urban to area for group data

- [`add_area(`*`<pipmd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/add_area.pipmd.md)
  : Recode urban to area for micro data

- [`add_dist_type()`](https://pip-technical-team.github.io/pipdata/reference/add_dist_type.md)
  : Add distribution type (lower level, S3 methods)

- [`add_dist_type(`*`<pipgd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/add_dist_type.pipgd.md)
  : Add distribution type group

- [`add_dist_type(`*`<pipmd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/add_dist_type.pipmd.md)
  : Add distribution type micro

- [`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md)
  : Apply recode specification to a data.table

- [`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
  : Build the PIP master and release inventories from stamp catalogs

- [`check_directory()`](https://pip-technical-team.github.io/pipdata/reference/check_directory.md)
  : Check whether the working folder exists and abort if it does not

- [`cln_changes()`](https://pip-technical-team.github.io/pipdata/reference/cln_changes.md)
  : Clean output from compare_aux_releases and compare_aux_vintages

- [`copy_dlw_metadata()`](https://pip-technical-team.github.io/pipdata/reference/copy_dlw_metadata.md)
  : Copy DLW Metadata Between Release Folders

- [`cpfw_merge()`](https://pip-technical-team.github.io/pipdata/reference/cpfw_merge.md)
  : Merge country/survey PFW with dataliweb survey data

- [`deflation(`*`<pipgd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/deflation.pipgd.md)
  : Deflation of welfare for group data

- [`deflation(`*`<pipmd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/deflation.pipmd.md)
  : Deflation of welfare for micro data

- [`diff_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/diff_recode_spec.md)
  : Compare two recode_spec versions (or one version vs. package YAML)

- [`dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/dlw_clean.md)
  : Clean data from datalibweb structure (lower level, S3 methods)

- [`dlw_clean(`*`<pipgd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/dlw_clean.pipgd.md)
  : Clean group data from Datalibweb original file

- [`dlw_clean(`*`<pipmd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/dlw_clean.pipmd.md)
  : Clean micro data from Datalibweb original file

- [`dlw_gmd_list()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_list.md)
  : Retrieve a List of GMD datasets from the Server and save it in the
  local dlw inventory folder.

- [`dlw_gmd_match()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_match.md)
  : Get the list of current GMD datasets that match the local inventory

- [`dlw_gmd_new()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_new.md)
  : Compare the local GMD dataset list with the server version to
  identify new entries.

- [`dlw_gmd_unvalidated()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_unvalidated.md)
  : Get un-validated datasets list

- [`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md)
  : DLW Validation Engine

- [`export_recode_spec_yaml()`](https://pip-technical-team.github.io/pipdata/reference/export_recode_spec_yaml.md)
  : Export recode spec from stamp to YAML

- [`get_country_pfw()`](https://pip-technical-team.github.io/pipdata/reference/get_country_pfw.md)
  : Get Country Price framework data based on PFW and DLW data info

- [`get_data_status()`](https://pip-technical-team.github.io/pipdata/reference/get_data_status.md)
  : Get a simple frequency that shows number of valid and invalid
  datasets

- [`get_validation_ctry()`](https://pip-technical-team.github.io/pipdata/reference/get_validation_ctry.md)
  : List of validation result by country and module type

- [`get_validation_list()`](https://pip-technical-team.github.io/pipdata/reference/get_validation_list.md)
  : List of validation result by survey ID and module type

- [`get_validation_report()`](https://pip-technical-team.github.io/pipdata/reference/get_validation_report.md)
  : Get the validation report data

- [`gmd_to_validate()`](https://pip-technical-team.github.io/pipdata/reference/gmd_to_validate.md)
  : Get datasets list that needs to be validated

- [`gmd_validated()`](https://pip-technical-team.github.io/pipdata/reference/gmd_validated.md)
  : Return Validated GMD Records

- [`inv_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/inv_dlw_load.md)
  : Load DLW survey data and prepare it for the PIP pipeline

- [`list_recode_spec_versions()`](https://pip-technical-team.github.io/pipdata/reference/list_recode_spec_versions.md)
  : List recode_spec versions from stamp catalog

- [`num_vars_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/num_vars_to_attr.md)
  : Create a named vector of attributes

- [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)
  : Build auxiliary metadata attributes for cleaned survey data

- [`pd_change_report()`](https://pip-technical-team.github.io/pipdata/reference/pd_change_report.md)
  : Report staged dependency changes without writing artifacts

- [`pd_cpfw_merge()`](https://pip-technical-team.github.io/pipdata/reference/pd_cpfw_merge.md)
  : Merge country/survey PFW info with dataliweb survey data

- [`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md)
  : Batch-deflate every survey in the PIP master inventory

- [`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md)
  : Clean data from datalibweb structure (High level)

- [`pd_split_alt_welfare()`](https://pip-technical-team.github.io/pipdata/reference/pd_split_alt_welfare.md)
  : Split data based on alternative welfare

- [`pd_wbpip_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_wbpip_clean.md)
  : Clean data for wbpip compatibility (high level)

- [`pipdata_int()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_int.md)
  : Get path to pipdata original files

- [`ppp_to_wide()`](https://pip-technical-team.github.io/pipdata/reference/ppp_to_wide.md)
  :

  Convert PPP data from `pipload` to wide format

- [`process_data()`](https://pip-technical-team.github.io/pipdata/reference/process_data.md)
  : Process datalibweb data: merge PFW data and clean variables

- [`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md)
  : Save cleaned PIP data or metadata to versioned storage

- [`uniq_vars_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/uniq_vars_to_attr.md)
  : convert variables with unique values along the data set to
  attributes and then remove those unique variables

- [`unq_obs_dt()`](https://pip-technical-team.github.io/pipdata/reference/unq_obs_dt.md)
  : Find unique values in PFW according to some key variables

- [`valid_aux_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_aux_load.md)
  : Retrieve the inventory of aux files that changed from previous
  release or vintage

- [`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
  : Determine which DLW surveys need processing

- [`vars_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/vars_to_attr.md)
  : Make vars as attributes

- [`vars_to_list()`](https://pip-technical-team.github.io/pipdata/reference/vars_to_list.md)
  : Return a named list with unique values of variables

- [`wbpip_clean()`](https://pip-technical-team.github.io/pipdata/reference/wbpip_clean.md)
  : Clean data to meet wbpip requirements and formats

- [`wbpip_clean(`*`<pipgd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/wbpip_clean.pipgd.md)
  : wbpip_clean method for pipgd class (group data)

- [`wbpip_clean(`*`<pipmd>`*`)`](https://pip-technical-team.github.io/pipdata/reference/wbpip_clean.pipmd.md)
  : wbpip_clean method for pipmd class (microdata)
