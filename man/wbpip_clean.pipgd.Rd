% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pd_wbpip_clean.R
\name{wbpip_clean.pipgd}
\alias{wbpip_clean.pipgd}
\title{wbpip_clean method for pipgd class (group data)}
\usage{
\method{wbpip_clean}{pipgd}(df, ...)
}
\arguments{
\item{df}{dataframe returned by \code{pd_dlw_clean()}}

\item{...}{Other parameters}
}
\value{
data.table
}
\description{
wbpip_clean method for pipgd class (group data)
}
\examples{
pfw  <- pipload::pip_load_aux("pfw")
gd   <- pipload::pip_load_dlw("CHN", 2015)
cpfw <- get_country_pfw(gd, pfw)
gd   <- pd_split_alt_welfare(gd, cpfw)
x    <- pd_dlw_clean(gd, cpfw)
y    <- wbpip_clean(x[[1]])
y[, unique(area)]

gd   <- pipload::pip_load_dlw("ARE", 2019)
cpfw <- get_country_pfw(gd, pfw)
gd   <- pd_split_alt_welfare(gd, cpfw)
x <- pd_dlw_clean(gd, cpfw)
y <- wbpip_clean(x[[1]])
y[, unique(area)]
}
