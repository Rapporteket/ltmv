% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regn_ut.R
\name{regn_n_pas}
\alias{regn_n_pas}
\title{Regn ut antall pasienter gitt en gruppe}
\usage{
regn_n_pas(d, ...)
}
\arguments{
\item{d}{Tibble/dataramme.}

\item{...}{Kolonner det skal grupperes på.}
}
\value{
Tibble med kolonnene gitt til funksjonen i \code{...},
samt kolonnen \code{n} som gir antallet rader i \code{d} med alle kombinasjoner av
verdiene i kolonnene gitt i \code{...}.
}
\description{
Regn ut antall pasienter gitt en gruppe
}
