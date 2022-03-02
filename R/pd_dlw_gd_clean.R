#' Clean group data from Datalibweb original file
#'
#' @param x data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return data.table
#' @export
#'
#' @examples
#' x   <- pipload::pip_load_dlw("CHN", 2015)
#' pfw <- pipload::pip_load_aux("pfw")
#' pd_dlw_gd_clean(x, pfw)
pd_dlw_gd_clean <- function(x, pfw) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rename variables --------
  gd[, area := fcase(urban == 1, "urban",
                     urban == 0, "rural",
                     is.na(urban), "national",
                     default = "")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## unique variables --------

  # get single-value variables
  uvl <- uniq_vars_to_list(gd)  #list with unique value

  # filter PFW

  cpfw <- # country price framework
    pfw[ country_code     == uvl$country_code
         & surveyid_year  == uvl$surveyid_year
         & survey_acronym == uvl$survey_acronym
    ]

  cpfw <- as.list(cpfw)


  #--- Is this necessary?
  gd[, survey_year := cpfw$survey_year]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format types --------

  string <- c("country_code", "survey_acronym", "area", "welfare_type", "gd_type")
  nume   <- c("surveyid_year", "survey_year", "weight", "welfare")

  gd[, (string) := lapply(.SD, as.character),
     .SDcols = string]

  gd[, (nume) := lapply(.SD, as.numeric),
     .SDcols = nume]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # data level vars   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (cpfw$ppp_domain == 1) {

    gd[, ppp_data_level := "national"]

  } else if (cpfw$ppp_domain == 2) {

    gd[, ppp_data_level := area]

  } else {

    gd[, ppp_data_level := as.character()]

  }


  pref             <- c("ppp", "cpi", "gdp", "pce", "pop")
  data_level_vars  <- glue("{pref}_data_level")
  domain_vars      <- glue("{pref}_domain")

  trows <- nrow(gd)

  gd[,
     (data_level_vars) :=
       lapply(domain_vars, \(x) {

         if (cpfw[[x]] == 1) {

           y <- rep("national", times = trows)

         } else if (cpfw[[x]] == 2) {
           y <-  area
         } else {
           y <- ""
         }
         y

       })
  ]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Distribution type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  gd[,
     distribution_type := {

       if (cpfw$pop_domain == 1) {

         # y <- rep("national", times = trows)
         Y <- "group"

       } else if (cpfw$pop_domain ==  2) {

         larea <- length(unique(area))

         if (larea %in% c(0, 1)) {
           y <- "group"
         } else {
           y <- "aggregate"
         }

       } else {
         y <- ""
       }
       y

     }
  ]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create variables that do not exist   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get from internal data `pip_var_type`
  pip_vars  <- pip_var_type$pip_vars_pc
  pip_type  <- pip_var_type$pip_vars_pc_class

  miss_ind  <- !(pip_vars %in% names(gd))
  miss_vars <- pip_vars[miss_ind]
  miss_type <- pip_type[miss_ind]

  miss_type <- glue("as.{miss_type}")

  gd[,
     (miss_vars) := lapply(miss_type, \(x) get(x)())]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Order and format --------

  # select columns
  gd <- gd[,  .SD, .SDcols = pip_vars]


  # of variable (columns)
  setcolorder(gd, pip_vars)

  # sorting

  varsort <- c("country_code", "surveyid_year", "area", "welfare")
  setorderv(gd, varsort)

  return(gd)
}
