% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insidens_og_prevalens.R
\name{regn_insprev}
\alias{regn_insprev}
\title{Funksjon for tilgjengelighet av behandling (insidens og prevalens)}
\usage{
regn_insprev(d, var_folk, insidens = TRUE, ...)
}
\arguments{
\item{d}{Datasett som enten er filtrert på rapporteringsaar
(til insidens) eller som inneholder data fra hele registeret /
alle år (for prevalens).}

\item{var_folk}{Variabel i \code{d} som inneholder folketall.}

\item{insidens}{Skal det regnes ut insidens eller prevalens?
Standard er insidens = TRUE.}

\item{...}{Grupperingsvariabler}
}
\value{
Returnerer en tibble med insidens eller prevalens, samt antall
registreringer, per gruppe.
}
\description{
Regner ut insidens og prevalens.
}
\details{
Krever et datasett som enten er filtrert på rapporteringsaar (til insidens)
eller som inneholder data fra hele registeret / alle år (for prevalens).
}
\examples{
# d_pers_folketall_akt er et datasett som inneholder LTMV-pasienter
# som er registrert i rapporteringsåret.
\dontrun{
ins_akt_rhf = regn_insprev(d_pers_folketall_akt,
  folketall_rhf,
  insidens = TRUE,
  rhf_kode, rhf_tekst
)
}
}
