% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_country_pfw.R
\name{get_country_pfw}
\alias{get_country_pfw}
\title{GEt Country Price framework  data based on PFW and DLW data info}
\usage{
get_country_pfw(df, pfw)
}
\arguments{
\item{df}{data frame with micro data, loaded with \code{pipload::pip_load_dlw()}}

\item{pfw}{data frame with Price framework data, loaded with
\code{pipload::pip_load_aux("pfw")}}
}
\value{
list of data.tables
}
\description{
GEt Country Price framework  data based on PFW and DLW data info
}
\examples{
pfw <- pipload::pip_load_aux("pfw")
gd   <- pipload::pip_load_dlw("CHN", 2015)
cpfw <- get_country_pfw(gd, pfw)
}
