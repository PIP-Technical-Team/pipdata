prep_dlw_inventory <- function() {
  dlw_dir      <- path(root,"DLW-RAW")
  dlw_inv_path <- path(dlw_dir,"_Inventory",
                       "DLWRAW_all_DTAs", ext = "txt")

  dlw_inv_path <- path(dlw_dir,"_Inventory",
                       "DLWRAW_all_DTAs", ext = "csv")

  dlw_inv <- read_csv(dlw_inv_path) %>%
    lazy_dt() %>%
    mutate(survey_id     = str_extract(FullName, "[^\\\\]+\\.dta$"),
           survey_id     = str_replace_all(survey_id, "\\.dta$", ""),
           CreationTime  = mdy_hms(CreationTime),
           LastWriteTime = mdy_hms(LastWriteTime)
    ) %>%
    as.data.table()

  id_vars <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection",
      "module"
    )


  pip_modules <-
    c("GPWG",
      "ALL",
      "BIN",
      "GROUP",
      "HIST")

  dlw_inv[, (id_vars) := tstrsplit(survey_id, split = c("_"), fixed = TRUE)]

  # dlw_inv %>%
  #   select(!c(M, A)) %>%
  #   mutate(surveyid_year := as.numeric(surveyid_year))

  dlw_inv <- dlw_inv[module %chin% pip_modules]
  dlw_inv[, c("M", "A")   := NULL]

  dlw_inv[,
          `:=`(
            surveyid_year = as.numeric(surveyid_year),
            tool          = fifelse(module == "ALL", "TB", "PC")
          )]

  dlw_inv %>%
    select(survey_id)
}
