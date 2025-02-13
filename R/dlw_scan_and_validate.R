#' Scan DLW .qs files, validate them, and version them in pip_raw_inventory
#'
#' This function checks a folder of .qs files (dlw_qs_folder), compares them to
#' an existing pip_raw_inventory (pip_raw_inventory_path), and performs
#' validation. It then versions and copies valid files into a pip_raw_folder,
#' updating the inventory with the new file information.
#'
#' @param dlw_qs_folder Character. Path to folder containing .qs files converted from DLW-RAW.
#' @param pip_raw_folder Character. Folder into which validated .qs files
#'   (versioned) will be placed.
#' @param pip_raw_inventory_path Character. Path where the pip_raw_inventory.qs
#'   file is stored (or will be created if missing).
#' @param validation_fn Function or NULL. A user-supplied validation function
#'   that takes a data.frame and returns a list like \code{list(is_valid=TRUE/FALSE, reason="<error message>")}.
#'
#' @return Invisibly returns the updated pip_raw_inventory as a data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_scan_and_validate(
#'   dlw_qs_folder = "data/dlw_qs",
#'   pip_raw_folder = "data/pip_raw",
#'   pip_raw_inventory_path = "data/pip_raw_inventory.qs",
#'   validation_fn = function(df) {
#'     # example: check if df has > 0 rows
#'     if (nrow(df) < 1) {
#'       return(list(is_valid=FALSE, reason="Empty dataset"))
#'     }
#'     list(is_valid=TRUE, reason=NA)
#'   }
#' )
#' }
dlw_scan_and_validate <- function(
    dlw_qs_folder,
    pip_raw_folder,
    pip_raw_inventory_path,
    validation_fn = NULL
) {


  # 1. Load or create pip_raw_inventory.qs (old_inv) ----
  ## If pip_raw_inventory_path exists, read it into 'old_inv'.
  ## Otherwise, create an empty tibble with columns:
  ##   (survey_id, pipeline_version, file_hash, pip_file_path, status, date_validated)

  ##  (!!) GC Note: tryCatch potential 1 - file read error ----

  old_inv <- if (file.exists(pip_raw_inventory_path)) {
    qread(pip_raw_inventory_path)
  } else {
    cli_alert_info("No previous pip_raw_inventory; creating empty.")
    tibble::tibble(
      survey_id        = character(),
      pipeline_version = integer(),
      file_hash        = character(),
      pip_file_path    = character(),
      status           = character(),
      date_validated   = as.POSIXct(character())
    )
  }


  # 2. Ensure pip_raw_folder exists -----
  ## We store validated, versioned files in pip_raw_folder. If it doesn't exist,
  ## we create it.

  if (!dir.exists(pip_raw_folder)) {
    dir.create(pip_raw_folder, recursive = TRUE)
  }

  # 3. Scan .qs in dlw_qs_folder (new_info) ----
  cli_alert_info("Scanning dlw_qs_folder: {dlw_qs_folder}")

  ## List all .qs files. If none exist, no validation is needed -> return early
  qs_files <- list.files(dlw_qs_folder, pattern="\\.qs$", full.names = TRUE)
  if (length(qs_files) == 0) {
    cli_alert_info("No .qs in {dlw_qs_folder}; nothing to validate.")
    return(invisible(NULL))
  }

  ## Build a small tibble with survey_id and a new MD5 hash for each file.
  ## survey_id is the filename (without extension).
  new_info <- lapply(qs_files, function(f) {
    bn <- tools::file_path_sans_ext(basename(f))
    h  <- digest::digest(file = f, algo = "md5")
    tibble::tibble(survey_id = bn, new_hash = h)
  }) |> dplyr::bind_rows()


  # 4. Compare to old_inv by survey_id (comp_df) ----
  ## We join 'old_inv' (hash from prior runs) with 'new_info' (new hash).
  ## The 'status_new' field is set to: new, missing, changed, same, etc.
  comp_df <- dplyr::full_join(
    old_inv |> dplyr::select(survey_id, file_hash, pipeline_version),
    new_info,
    by = "survey_id",
    suffix = c("_old", "_new")
  ) |>
    dplyr::mutate(
      status_new = dplyr::case_when(
        is.na(file_hash) & !is.na(new_hash) ~ "new",
        !is.na(file_hash) & is.na(new_hash) ~ "missing",
        !is.na(file_hash) & !is.na(new_hash) & file_hash != new_hash ~ "changed",
        !is.na(file_hash) & !is.na(new_hash) & file_hash == new_hash ~ "same",
        TRUE ~ NA_character_
      )
    )


  # 5. Validating and versioning ----

  ## We get all survey_ids and prepare new_inv structure
  all_names <- unique(comp_df$survey_id)
  new_inv   <- vector("list", length(all_names))

  cli_progress_bar("Validating & Versioning", total = length(all_names))

  # We'll iterate over each unique 'survey_id' in comp_df:
  for (i in seq_along(all_names)) {
    nm <- all_names[i]
    row_i <- comp_df |> dplyr::filter(survey_id == nm)
    st <- row_i$status_new[1]

    ## 5.1 If "same" or "missing": we don't re-validate ---
    if (st %in% c("same","missing")) {
      # We'll either carry forward the old record or mark it as missing
      oldr <- row_i |> dplyr::filter(!is.na(file_hash))

      if (nrow(oldr) > 0 && st == "same") {
        # carry forward old record
        new_inv[[i]] <- old_inv |>
          dplyr::filter(survey_id==nm, pipeline_version==oldr$pipeline_version) |>
          dplyr::mutate(status=st)
      } else if (nrow(oldr) > 0 && st=="missing") {
        # file used to exist, now missing
        new_inv[[i]] <- old_inv |>
          dplyr::filter(survey_id==nm, pipeline_version==oldr$pipeline_version) |>
          dplyr::mutate(status="missing", pip_file_path=NA)
      }
      cli_progress_update()
      next
    }

    ## 5.2 If "new" or "changed": we DO re-validate ----
    if (st == "new" || st == "changed") {
      # read the .qs file
      qs_path <- file.path(dlw_qs_folder, paste0(nm, ".qs"))
      df <- tryCatch(
        qread(qs_path),
        error = function(e) NULL  # If read fails, 'df' is NULL
      )

      # If we can't read the file, we skip it or mark it invalid
      if (is.null(df)) {
        cli_alert_warning("Cannot read {qs_path}")
        new_inv[[i]] <- NULL
        cli_progress_update()
        next
      }

      ### (!!) Validation ----
      ### Validate with optional user-supplied function (HERE IS WHERE VALIDATION_FN ENTERS)
      is_valid <- TRUE
      fail_reason <- NA
      if (!is.null(validation_fn)) {
        check <- validation_fn(df)
        if (!check$is_valid) {
          is_valid    = FALSE
          fail_reason = check$reason
        }
      }
      if (!is_valid) {
        ### If validation fails, we do not add it to new_inv
        cli_alert_danger("Validation failed for {nm}: {fail_reason}")
        new_inv[[i]] <- NULL
        cli_progress_update()
        next
      }

      ## 5.3 Versioning: ----
      ## Each new or changed file gets version=1 or version=(max+1)
      if (st == "new") {
        new_ver <- 1
      } else {
        ex_vers <- row_i %>% dplyr::filter(!is.na(pipeline_version)) %>% dplyr::pull(pipeline_version)
        new_ver <- ifelse(length(ex_vers) == 0, 1, max(ex_vers) + 1)
      }
      vers_str  <- sprintf("v%02d", new_ver)
      finalname <- paste0(nm, "_", vers_str, ".qs")
      finalpath <- file.path(pip_raw_folder, finalname)

      ## Copy validated file to pip_raw_folder
      qsave(df, finalpath)

      ## Add new row to new_inv
      new_inv[[i]] <- tibble::tibble(
        survey_id        = nm,
        pipeline_version = new_ver,
        file_hash        = row_i$new_hash[1],
        pip_file_path    = finalpath,
        status           = st,
        date_validated   = Sys.time()
      )
      cli_progress_update()
    }
  }

  cli_progress_done()

  ## 5.4 merge new_inv rows into final_inv ----
  ## Combine all new_inv items into a single data frame.
  final_inv <- dplyr::bind_rows(new_inv) |>
    pipload::survey_id_to_vars()

  # 6. Save new_inv as the new pip_raw_inventory ----
  qsave(final_inv, pip_raw_inventory_path)
  cli_alert_success("Created new pip_raw_inventory at: {pip_raw_inventory_path}")

  return(invisible(final_inv))
}
