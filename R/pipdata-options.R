#' pipdata package options
#'
#' @description
#' `pipdata` is controlled by a small set of [options()]. Each option has a
#' package-level default set in `.onLoad()` (see `zzz.R`) and can be
#' overridden per-session or per-call.
#'
#' @section Options:
#'
#' **`pipdata.verbose`** (`logical`, default `TRUE`)
#'
#' Controls whether informational I/O messages from downstream packages
#' (`pipload`, `stamp`, `pipaux`) are printed during pipeline runs.
#'
#' - `TRUE` (default): `pip_read()`, `pip_write()`,
#'   `load_pip_master_inventory()`, `load_aux_data()`, and related I/O
#'   calls emit their normal progress messages. Useful for interactive
#'   exploration.
#' - `FALSE`: all downstream I/O messages are suppressed. Recommended for
#'   batch/production runs to keep output focused on pipeline-level events.
#'
#' The option is read at the boundary of every exported function via
#' `getOption("pipdata.verbose", default = TRUE)`. You can still override it
#' per-call by passing `verbose = FALSE` (or `TRUE`) explicitly — the
#' per-call argument always takes precedence over the global option.
#'
#' All `pipload`/`stamp`/`pipaux` I/O calls propagate the resolved value
#' without exception. The only calls hardcoded to `verbose = FALSE` are
#' `joyn::` join diagnostics, which are not I/O messages and outside the
#' scope of this option.
#'
#' **Batch run pattern** — set at the top of your orchestration script:
#'
#' ```r
#' options(pipdata.verbose = FALSE)
#' ```
#'
#' **Note**: `pipdata.verbose` controls *user-facing I/O messages* only.  It
#' does **not** affect the structured pipeline logging written via
#' `pipfun::log_add()` / `pipfun::log_info()` into `pipdata_log`. Structured
#' log entries are always emitted regardless of this option.
#'
#' @name pipdata-options
#' @keywords internal
NULL
