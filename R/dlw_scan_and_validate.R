dlw_scan_and_validate <- function(
    dlw_qs_folder,
    pip_raw_folder,
    pip_raw_inventory_path,
    validation_fn = NULL
) {
  # 1. Load or create pip_raw_inventory.qs (old_inv) -----
  old_inv <- if (file.exists(pip_raw_inventory_path)) {
    qread(pip_raw_inventory_path)
  } else {
    cli_alert_info("No previous pip_raw_inventory; creating empty.")
    tibble::tibble(
      survey_id         = character(),
      pipeline_version = integer(),
      file_hash        = character(),
      pip_file_path    = character(),
      status           = character(),
      date_validated   = as.POSIXct(character())
    )
  }

  # 2. Ensure pip_raw_folder exists -----
  ## Note: This is where validated files will be stored.
  if (!dir.exists(pip_raw_folder)) dir.create(pip_raw_folder, recursive=TRUE)

  # 3. Scan .qs in dlw_qs_folder (new_info) ----
  cli_alert_info("Scanning dlw_qs_folder: {dlw_qs_folder}")

  ## List files
  qs_files <- list.files(dlw_qs_folder, pattern="\\.qs$", full.names=TRUE)
  if (length(qs_files)==0) {
    cli_alert_info("No .qs in {dlw_qs_folder}; nothing to validate.")
    return(invisible(NULL))
  }

  ## Build new_info: (survey_id, new_hash)
  new_info <- lapply(qs_files, function(f){
    bn <- tools::file_path_sans_ext(basename(f))
    h  <- digest::digest(file=f, algo="md5")
    tibble::tibble(survey_id=bn, new_hash=h)
  }) |> bind_rows()

  # 4. Compare to old_inv by survey_id (comp_df) ----
  comp_df <- full_join(
    old_inv |> select(survey_id, file_hash, pipeline_version),
    new_info,
    by="survey_id",
    suffix=c("_old","_new")
  ) |>
    mutate(
      status_new = case_when(
        is.na(file_hash) & !is.na(new_hash) ~ "new",
        !is.na(file_hash) & is.na(new_hash) ~ "missing",
        !is.na(file_hash) & !is.na(new_hash) & file_hash != new_hash ~ "changed",
        !is.na(file_hash) & !is.na(new_hash) & file_hash == new_hash ~ "same",
        TRUE ~ NA_character_
      )
    )

  # 5. Validating and versioning ----
  all_names <- unique(comp_df$survey_id)
  new_inv <- vector("list", length(all_names))

  cli_progress_bar("Validating & Versioning", total=length(all_names))

  for (i in seq_along(all_names)) {
    nm <- all_names[i]
    row_i <- comp_df %>% filter(survey_id==nm)
    st <- row_i$status_new[1]

    ## 5.1 If "same" or "missing": ----
    ## We do not need validation.
    if (st %in% c("same","missing")) {
      # carry forward old info or mark missing
      oldr <- row_i %>% filter(!is.na(file_hash))
      if (nrow(oldr)>0 && st=="same") {
        # carry forward
        new_inv[[i]] <- old_inv %>%
          filter(survey_id==nm, pipeline_version==oldr$pipeline_version) %>%
          mutate(status=st)
      } else if (nrow(oldr)>0 && st=="missing") {
        # mark missing
        new_inv[[i]] <- old_inv %>%
          filter(survey_id==nm, pipeline_version==oldr$pipeline_version) %>%
          mutate(status="missing", pip_file_path=NA)
      }
      cli_progress_update()
      next
    }

    ## 5.2 If "new" or "changed": ----
    ## We do need validation.
    if (st=="new" || st=="changed") {
      # read the .qs, validate
      qs_path <- file.path(dlw_qs_folder, paste0(nm,".qs"))
      df <- tryCatch(qread(qs_path), error=function(e)NULL)
      if (is.null(df)) {
        cli_alert_warning("Cannot read {qs_path}")
        new_inv[[i]] <- NULL
        cli_progress_update()
        next
      }
      # validate
      is_valid <- TRUE
      fail_reason <- NA
      if (!is.null(validation_fn)) {
        check <- validation_fn(df)
        if (!check$is_valid) {
          is_valid <- FALSE
          fail_reason <- check$reason
        }
      }
      if (!is_valid) {
        cli_alert_danger("Validation failed for {nm}: {fail_reason}")
        new_inv[[i]] <- NULL
        cli_progress_update()
        next
      }

      ## 5.3 Versioning for new or changed: ----
      ## figure out pipeline_version
      if (st=="new") {
        new_ver <- 1
      } else {
        ex_vers <- row_i %>% filter(!is.na(pipeline_version)) |> pull(pipeline_version)
        new_ver <- ifelse(length(ex_vers)==0, 1, max(ex_vers)+1)
      }
      vers_str <- sprintf("v%02d", new_ver)
      finalname <- paste0(nm,"_",vers_str,".qs")
      finalpath <- file.path(pip_raw_folder, finalname)
      qsave(df, finalpath)

      new_inv[[i]] <- tibble::tibble(
        survey_id         = nm,
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

  ## 5.4 merge new_inv rows into final_inv -----
  final_inv <- bind_rows(new_inv) |>
    pipload::survey_id_to_vars()


  # 6. Save new_inv as the new pip_raw_inventory ----
  qsave(final_inv, pip_raw_inventory_path)
  cli_alert_success("Created new pip_raw_inventory at: {pip_raw_inventory_path}")

  return(invisible(final_inv))
}
