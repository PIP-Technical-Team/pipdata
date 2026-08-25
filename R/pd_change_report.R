#' Report staged dependency changes without writing artifacts
#'
#' @param inv DLW inventory metadata.
#' @param master PIP master inventory metadata.
#' @param manifest Optional dependency manifest.
#' @param context Optional resolved dependency context.
#' @return A `pip_dependency_plan`, invisibly.
#' @export
pd_change_report <- function(inv = pipload::load_gmd_valid_inv(verbose = FALSE),
                             master = pipload::load_pip_master_inventory(verbose = FALSE),
                             manifest = NULL,
                             context = pd_dependency_context()) {
  if (is.null(manifest)) manifest <- pd_manifest_read(context, allow_absent = TRUE)
  plan <- pd_dependency_plan(inv, master, manifest, context)
  print(plan)
  invisible(plan)
}

#' @export
print.pip_dependency_plan <- function(x, ...) {
  totals <- x$actions[, .N, by = .(stage, action)]
  cat("PIP dependency plan\n")
  if (!nrow(totals)) cat("  no changes\n") else print(totals)
  invisible(x)
}
