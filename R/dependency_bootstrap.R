pd_dependency_completeness <- function(plan) {
  stale <- plan$actions[action != "none"]
  unknown <- plan$reasons[reason %in% c("unknown_provenance", "output_drift")]
  list(complete = nrow(stale) == 0L && nrow(unknown) == 0L,
       stale = data.table::copy(stale), unknown = data.table::copy(unknown))
}
