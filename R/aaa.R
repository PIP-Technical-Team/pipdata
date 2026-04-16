
.pipdata <-  new.env(parent = emptyenv())

.logenv <-  new.env(parent = emptyenv())

.pipdataenv <-  new.env(parent = emptyenv())

# Internal logmeta type markers -- excluded from the summary-by-type table
# in log_report() so it only shows genuine pipeline errors/warnings.
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata"
)

# Suppress R CMD check notes for unquoted data.table column names and other
# symbols used in non-standard evaluation throughout the package.
utils::globalVariables(c(
  # data.table NSE column names
  "..key", "..selected_vars", ".data", ".joyn",
  "Checksum", "Ext", "FileName", "Module", "N",
  "age", "count_valid", "country", "data_available", "data_status",
  "date_validated", "description", "dlw_meta", "educat7", "educy",
  "error_type", "event", "ext", "hhid", "logmeta",
  "maxalt", "maxmast", "maxpip", "module_type", "pid", "pin_version",
  "pip_id", "rf_year", "status", "status_count", "survey",
  "table_name", "tool", "type", "version_dlw"
))
