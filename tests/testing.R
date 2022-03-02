library(data.table)
# load_all()
gd <- pipload::pip_load_dlw("CHN", 2015)
pipload::pip_load_all_aux(replace = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial formating   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

gd[,
   domain := cpfw$ppp_domain
][,
  ppp_data_level := fcase(domain == 1, "national",
                          domain == 2, area,
                          default = "")
][, "domain" := NULL
]


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


