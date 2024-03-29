% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pd_add_pip_vars.R
\name{add_pip_vars}
\alias{add_pip_vars}
\title{Estimate welfare in PPP values (lower level)}
\usage{
add_pip_vars(df, cpfw, cpi, ppp, pop, ...)
}
\arguments{
\item{df}{dataframe from \code{pd_wbpip_clean()}}

\item{cpfw}{data frame with Price framework data for country/survey in \code{df}.
It is loaded with \code{get_country_pfw(df, pfw)}. \code{pfw} is loaded in
\code{pipload::pip_load_aux("pfw")}}

\item{cpi}{dataframe from \code{pipload::pip_load_aux("cpi")}}

\item{ppp}{dataframe from \code{pipload::pip_load_aux("ppp")}}

\item{pop}{dataframe from \code{pipload::pip_load_aux("pop")}}

\item{...}{extra arguments}
}
\value{
data.table with extra PIP variables
}
\description{
Estimate welfare in PPP values (lower level)
}
\examples{
ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")
md   <- pipload::pip_load_dlw(country = "PRY", 2012)
pfw  <- pipload::pip_load_aux("pfw")
cpfw <- get_country_pfw(md, pfw)
md   <- pd_split_alt_welfare(md, cpfw)
x    <- pd_dlw_clean(md, cpfw)
y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]

add_pip_vars(df = y, cpfw = cpfw, cpi = cpi, ppp = ppp, pop = pop)

gd   <- pipload::pip_load_dlw("CHN", 2015)
cpfw <- get_country_pfw(gd, pfw)
gd   <- pd_split_alt_welfare(gd, cpfw)
x    <- pd_dlw_clean(gd, cpfw)
y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]

add_pip_vars(df = y, cpfw = cpfw, cpi = cpi, ppp = ppp, pop = pop)

gd   <- pipload::pip_load_dlw("ARE", 2019)
cpfw <- get_country_pfw(gd, pfw)
gd   <- pd_split_alt_welfare(gd, cpfw)
x    <- pd_dlw_clean(gd, cpfw)
y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]

add_pip_vars(df = y, cpfw = cpfw, cpi = cpi, ppp = ppp, pop = pop)
}
