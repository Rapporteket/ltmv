% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lag_antall_skjema_tabell.R
\name{grupper_skjemaoversikt}
\alias{grupper_skjemaoversikt}
\title{Grupper skjemaoversikt}
\usage{
grupper_skjemaoversikt(d_skjemaoversikt)
}
\arguments{
\item{d_skjemaoversikt}{Skjemaet «skjemaoversikt» frå LTMV-databasen, som ei dataramme.}
}
\value{
Dataramma \code{d_skjemaoversikt} med to ekstra kolonner,
skjema_gruppe_nr og skjema_gruppe.
}
\description{
Grupperer skjemaoversikt i kategoriar alt etter om skjemaa er
ferdige eller uferdige,
og slår saman ad hoc oppfylgjing og oppfylgjingskjema frå og med år 5
til kategorien «Videre oppfølging».
}
\details{
Tak inn skjemaet «skjemaoversikt» frå LTMV-databasen,
som kan hentast med \code{hent_skjema("skjemaoversikt")},
og legg til to nye kolonner, \code{skjema_gruppe_nr} og \code{skjema_gruppe}.

\code{skjema_gruppe} er lik \code{skjemanavn} for alle skjema utanom oppfylgjing
frå og med år 5, og ad hoc oppfylgjing.
Desse vert slått saman og får verdien \verb{Videre oppfølging (år 5+ og AdHoc)}
i den nye kolonnen \code{skjema_gruppe}.
I tillegg vil det for uferdige skjema leggjast på "(uferdig)" på slutten
av verdiane i \code{skjema_gruppe} for uferdige skjema.

\code{skjema_gruppe_nr} tek utgangspunkt i kolonnen \code{skjemarekkeflg},
som er eit unikt tal for kvar skjematype,
der verdien svarar til oppfylgjingsår.
Ad hoc oppfylgjing har i \code{skjemarekkeflg} verdiar på 500 og noko,
og avslutningsskjema verdien 999.
\code{skjema_gruppe_nr} nyttar same verdiane som \code{skjemarekkeflg},
utanom oppfylgjing frå og med år 5 og ad hoc oppfylgjing,
som no får verdien 99.
I tillegg skiljer \code{skjema_gruppe_nr} på ferdige og uferdige skjema,
i motsetnad til \code{skjemarekkeflg},
ved å leggja til 0.5 til verdien.
I \code{skjema_gruppe_nr} tek altså ferdige skjema heiltalsverdiar,
medan uferdige skjema har tilsvarande verdiar,
men med .5 lagt til.

Dette er ein intern funksjon som i hovudsak nyttast til å laga
tabelloversikt over ferdige og uferdige skjema.
}
\examples{
\dontrun{
library(dplyr)

d_skjemaoversikt_gruppert = hent_skjema("skjemaoversikt") |>
  ltmv:::grupper_skjemaoversikt()

d_skjemaoversikt_gruppert |>
  head(20) |>
  select(skjemanavn, skjemastatus, skjema_gruppe_nr, skjema_gruppe)
}
}
\keyword{internal}
