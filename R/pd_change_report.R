#' Report staged dependency changes without writing artifacts
#'
#' @param inv Completed DLW validation inventory metadata. Before dependency
#'   planning, only `data_available = "Yes"` rows with status `"valid"` or
#'   `"invalid"` are retained. Recognized legacy blank/`"No"` retry rows are
#'   excluded, while malformed completed rows abort.
#' @param master PIP master inventory metadata.
#' @param manifest Optional dependency manifest.
#' @param context Optional resolved dependency context.
#' @return A `pip_dependency_plan`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' plan <- pd_change_report(
#'   inv = pipload::load_gmd_valid_inv(),
#'   master = pipload::load_pip_master_inventory()
#' )
#' }
pd_change_report <- function(inv = pipload::load_gmd_valid_inv(verbose = FALSE),
                             master = pipload::load_pip_master_inventory(verbose = FALSE),
                             manifest = NULL,
                             context = pd_dependency_context()) {
  prepared <- pd_prepare_dependency_facts(
    inv = inv,
    master = master,
    context = context,
    manifest = manifest
  )
  snapshot <- prepared$snapshot
  plan <- pd_dependency_plan(
    snapshot$inventory,
    snapshot$master,
    prepared$manifest,
    prepared$context,
    snapshot$fingerprints,
    snapshot = snapshot
  )
  print(plan)
  invisible(plan)
}

#' @export
print.pip_dependency_plan <- function(x, ...) {
  nodes <- data.table::copy(x$actions)
  nodes[, `:=`(
    state = data.table::fifelse(action == "none", "current", "stale"),
    scheduling_state = data.table::fifelse(
      action == "none", "cached", "runnable"
    )
  )]
  forced <- unique(x$reasons[
    reason == "forced", .(stage, entity_id)
  ])
  if (nrow(forced)) {
    nodes[forced, on = c("stage", "entity_id"), state := "forced"]
  }
  totals <- nodes[, .N, by = .(stage, action)]
  dispositions <- nodes[, .N, by = .(stage, state, scheduling_state)]
  reasons <- x$reasons[, .N, by = .(stage, reason)]
  cat("PIP dependency plan\n")
  if (!nrow(totals)) cat("  no changes\n") else print(totals)
  cat("Disposition summary\n")
  if (!nrow(dispositions)) cat("  no selected nodes\n") else {
    print(dispositions)
  }
  cat("Reason summary\n")
  if (!nrow(reasons)) cat("  no invalidation reasons\n") else print(reasons)
  invisible(x)
}
