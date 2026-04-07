
.pipdata <-  new.env(parent = emptyenv())

.logenv <-  new.env(parent = emptyenv())

.pipdataenv <-  new.env(parent = emptyenv())

# Internal logmeta type markers — excluded from the summary-by-type table
# in log_report() so it only shows genuine pipeline errors/warnings.
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata"
)
