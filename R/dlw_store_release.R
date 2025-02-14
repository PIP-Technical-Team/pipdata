#' Store a DLW release record in `_release/` folder, updates master inventory list.
#'
#' By default, this function writes the contents of `pip_raw_inventory_df` into
#' a new `.qs` file in a `_release/` folder (one file per release).
#' If `update_inventory_list = TRUE`, it also loads or creates a master list
#' (pointed to by `pip_raw_releases`), appends the release, and saves it back.
#'
#'
#' @param pip_raw_inventory_df data.frame. A data frame of pipeline rows (survey_id, file paths, etc.)
#' @param release_label Character. e.g. "20250202_INT"
#' @param release_folder Character. Path to the folder in which a `_release/`
#'   subfolder is created if it does not exist.
#' @param update_inventory_list Logical. If \code{TRUE}, also load/save a `.qs` file
#'   storing a named list of releases at \code{pip_raw_releases}.
#' @param pip_raw_releases Character. Path to a `.qs` file that holds a named list
#'   of releases. Only used if \code{update_inventory_list=TRUE}.
#' @param log_err Logical. Whether to log errors via \code{add_log()}.
#' @param skip_err Logical. Whether to \emph{skip} errors (instead of aborting).
#'
#' @return Invisibly returns the updated master list (if \code{update_inventory_list=TRUE}),
#'   otherwise returns \code{NULL}. If an error occurs and \code{skip_err=TRUE}, returns \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Default usage (no skip, log errors, update master list):
#' dlw_store_release(
#'   pip_raw_inventory_df  = pip_raw_inventory_df,
#'   release_label         = "20250202_INT",
#'   release_folder        = "pip_raw",
#'   update_inventory_list = TRUE,
#'   pip_raw_releases      = "pip_raw/pip_raw_inventory_releases.qs",
#'   log_err  = TRUE,
#'   skip_err = TRUE
#' )
#' }
dlw_store_release <- function(
    pip_raw_inventory_df,
    release_label,
    release_folder,
    update_inventory_list = TRUE,
    pip_raw_releases      = NULL,
    log_err               = TRUE,
    skip_err              = TRUE
) {


  tryCatch(
    expr = {

      # 1. Create `_release/` folder ---
      #
      # store_release_err tryCatch 1
      release_dir <- file.path(release_folder, "_release")
      tryCatch(
        expr = {
          if (!dir.exists(release_dir)) {
            dir.create(release_dir, recursive = TRUE)
          }
        },
        error = function(e) {
          cli::cli_abort(
            message = sprintf("Cannot create directory '%s': %s", release_dir, e$message),
            class   = c("store_release_err", "piperr"),
            call    = sys.call(),
            log     = log_err,
            skip    = skip_err
          )
        }
      )

      # Construct the path for the release .qs
      release_filename <- paste0("pip_raw_inventory_", release_label, ".qs")
      release_path     <- file.path(release_dir, release_filename)

      # 2. Save the input data frame as a .qs file for this release ---
      #
      # store_release_err tryCatch 2
      tryCatch(
        expr = {
          qs::qsave(pip_raw_inventory_df, release_path)
          cli::cli_alert_success(
            "Saved release '{release_label}' in '{release_path}'"
          )
        },
        error = function(e) {
          cli::cli_abort(
            message = sprintf("Could not save release to '%s': %s", release_path, e$message),
            class   = c("store_release_err", "piperr"),
            call    = sys.call(),
            log     = log_err,
            skip    = skip_err
          )
        }
      )

      # 3. Update the inventory list .qs ---
      if (isTRUE(update_inventory_list)) {

        # 3.1 Attempt to load or create the inventory list
        inventory_list <- tryCatch(
          expr = {
            if (!is.null(pip_raw_releases) && file.exists(pip_raw_releases)) {
              qs::qread(pip_raw_releases)
            } else {
              list()
            }
          },
          error = function(e) {
            cli::cli_abort(
              message = sprintf("Could not read master list '%s': %s", pip_raw_releases, e$message),
              class   = c("store_release_err", "piperr"),
              call    = sys.call(),
              log     = log_err,
              skip    = skip_err
            )
          }
        )

        # 3.2 Build the new release entry
        row_lists <- lapply(seq_len(nrow(pip_raw_inventory_df)), function(i) {
          as.list(pip_raw_inventory_df[i, , drop = FALSE])
        })
        new_entry <- list(
          timestamp = as.character(Sys.time()),
          data      = row_lists
        )

        inventory_list[[release_label]] <- new_entry

        # 3.3 Save the updated inventory list
        tryCatch(
          expr = {
            if (is.null(pip_raw_releases) || pip_raw_releases == "") {
              stop("No valid file path specified for pip_raw_releases", call. = FALSE)
            }
            qs::qsave(inventory_list, pip_raw_releases)
            cli::cli_alert_success(
              "Updated inventory list '{pip_raw_releases}' with release '{release_label}'"
            )
          },
          error = function(e) {
            cli::cli_abort(
              message = sprintf("Could not save master list '%s': %s", pip_raw_releases, e$message),
              class   = c("store_release_err", "piperr"),
              call    = sys.call(),
              log     = log_err,
              skip    = skip_err
            )
          }
        )

        # Return the updated inventory list
        return(invisible(inventory_list))
      }

      # If not updating, return NULL
      invisible(NULL)
    },

    # Condition handler for "store_release_err"
    store_release_err = function(cnd) {

      # 1. Log if needed
      if (isTRUE(cnd$log)) {
        add_log(cnd)
      }

      # 2. Skip or abort
      if (!isTRUE(cnd$skip)) {
        cli::cli_abort(
          cnd$message,
          call  = cnd$call,
          class = class(cnd)
        )
      } else {
        cli::cli_alert_warning(sprintf(
          "Skipping store release due to error: %s", cnd$message
        ))
        return(invisible(NULL))
      }
    }
  )
}


# OLD VERSION -----
dlw_store_release_old <- function(
    release_label,
    pip_raw_inventory_df,    # a data frame with your pipeline rows
    pip_raw_releases
) {
  # 1. Load existing JSON or create empty list ----
  if (file.exists(pip_raw_releases)) {
    # use simplifyVector=FALSE so we keep it as a named list-of-lists
    inventory_list <- fromJSON(pip_raw_releases, simplifyVector=FALSE)
  } else {
    inventory_list <- list()  # empty
  }

  # 2. Convert pip_raw_inventory_df to a list-of-rows ----
  ## Note: So that fromJSON(simplifyVector=TRUE) can read as a df

  ## Select only needed variables
  pip_raw_inventory_df <-  pip_raw_inventory_df |>
    select(survey_id, pip_file_path, date_validated,
           country_code, surveyid_year, module,
           vermast, veralt, pipeline_version, file_hash)

  row_lists <- lapply(seq_len(nrow(pip_raw_inventory_df)), function(i) {
    # for each row, build a named list
    row_as_list <- as.list(pip_raw_inventory_df[i, , drop=FALSE])
    # Flatten any factor columns to character, etc.
    row_as_list <- lapply(row_as_list, function(x) {
      if (is.factor(x)) as.character(x) else x
    })
    row_as_list
  })

  # 3. Build the new release entry ----
  new_entry <- list(
    timestamp = as.character(Sys.time()),
    data      = row_lists
  )

  # 4. Insert it under key = release_label ----
  inventory_list[[release_label]] <- new_entry

  # 5. Write JSON back
  ## 'auto_unbox=TRUE' ensures single-value fields remain scalars,
  ## 'pretty=TRUE' for readability
  write_json(inventory_list, pip_raw_releases, pretty=TRUE, auto_unbox=TRUE)
  cli_alert_success("Archived release '{release_label}' with {nrow(pip_raw_inventory_df)} rows into {pip_raw_releases}")
}

