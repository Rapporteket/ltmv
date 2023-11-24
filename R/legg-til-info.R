#' Legg til pasientinfo
#'
#' @description
#' Tar inn en dataramme, og legger til ekstra kolonner med info om pasienten.
#'
#' @param d
#' Dataramme. Må inneholde en kolonne med navnet gitt i `pasientid_varnavn`.
#'
#' @param pasientid_varnavn
#' Navn på kolonnen i `d` som inneholder pasient-ID.
#'
#' @return
#' Datarammen `d`, med ekstra kolonner med info om pasienten.
#'
#' @details
#' Funksjonen knytter datarammen `d` til pasientskjema basert på pasient-IDer
#' i kolonnen `pasientid_varnavn`.
#' Fra pasientskjema hentes dato for registrering, fødselsdato, kjønn,
#' info om pasienten er død eller levende, og eventuell dødsdato.
#' Denne infoen legges så til som nye kolonner i den originale datarammen.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") %>%
#'   legg_til_pasientid(mceid) %>%
#'   legg_til_pasientinfo(patient_id)
#' }
legg_til_pasientinfo = function(d, pasientid_varnavn) {
  d_patient = hent_skjema("patient") %>%
    select(id, registered_date, birth_date, gender, deceased, deceased_date)

  left_join(d, d_patient, by = join_by({{ pasientid_varnavn }} == id))
}

#' Legg til pasientid
#'
#' @description
#' Tar inn en dataramme, og legger til en ekstra kolonne med pasient-ID.
#'
#' @param d
#' Dataramme. Må inneholde en kolonne med navnet gitt i `skjemaid_varnavn`.
#' @param skjemaid_varnavn
#' Navn på kolonnen i `d` som inneholder skjema-ID.
#'
#' @return
#' Datarammen `d`, med en ekstra kolonne med med pasient-ID.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") %>%
#'   legg_til_pasientid(mceid)
#' }
legg_til_pasientid = function(d, skjemaid_varnavn) {
  d_mce = hent_skjema("mce") %>%
    select(patient_id, mceid)

  left_join(d, d_mce, by = join_by({{ skjemaid_varnavn }} == mceid))
}

#' Legg til alder og alderskategori
#'
#' @description
#' Tar inn en dataramme,
#' og legger til ekstra kolonner med alder og alderskategori barn/voksen
#' (over/under 18 år).
#'
#' @param d
#' Dataramme.
#' Må inneholde kolonner med navnene gitt i
#' `fodselsdato_varnavn` og `hendelsesdato_varnavn`.
#' @param fodselsdato_varnavn
#' Navn på kolonnen i `d` som inneholder fødselsdato.
#' @param hendelsesdato_varnavn
#' Navn på kolonnen i `d` som inneholder dato
#' som skal brukes til å beregne alder.
#'
#' @return
#' Datarammen `d`, med ekstra kolonner med alder ved datoen i kolonnen
#' `hendelsesdato_varnavn`, og tilhørende alderskategori barn/voksen
#' (over/under 18 år).
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") %>%
#'   legg_til_pasientid(mceid) %>%
#'   legg_til_pasientinfo(patient_id) %>%
#'   legg_til_alder_og_kategori(birth_date, start_date)
#' }
legg_til_alder_og_kategori = function(d, fodselsdato_varnavn, hendelsesdato_varnavn) {
  d %>%
    mutate(
      alder = lubridate::time_length(
        x = lubridate::interval(
          start = {{ fodselsdato_varnavn }},
          end = {{ hendelsesdato_varnavn }}
        ),
        unit = "years"
      ),
      alderkat = factor(alder >= 18,
        levels = c(FALSE, TRUE),
        labels = c("barn", "voksen")
      )
    )
}

#' Legg til info om behandlingsstopp
#'
#' @description
#' Tar inn en dataramme,
#' og legger til ekstra kolonner med info om behandlingsstopp.
#'
#' @param d
#' Dataramme. Må inneholde en kolonne med navnet gitt i `skjemaid_varnavn`.
#' @param skjemaid_varnavn
#' Navn på kolonnen i `d` som inneholder skjema-ID.
#'
#' @return
#' Datarammen `d`, med ekstra kolonner med info om behandlingsstopp.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") %>%
#'   legg_til_stoppinfo(mceid)
#' }
legg_til_stoppinfo = function(d, skjemaid_varnavn) {
  d_mce = hent_skjema("mce") %>%
    mutate(parent_mce = if_else(mcetype == 1, mceid, parent_mce)) %>%
    select(mceid, parent_mce)

  d_stoppinfo = hent_skjema("conclude") %>%
    left_join(d_mce, by = "mceid") %>%
    select(parent_mce, contains("stop"), conclude_status = status)

  d %>%
    left_join(d_mce, by = join_by({{ skjemaid_varnavn }} == mceid)) %>%
    left_join(d_stoppinfo, by = "parent_mce")
}

#' Legg til info aktiv behandling
#'
#' @description
#' Tar inn en dataramme,
#' og legger til en ekstra kolonne `aktiv_behandling` som er
#' `TRUE` eller `FALSE` avhengig av om forløpet er pågående eller avsluttet.
#'
#' @param d
#' Dataramme.
#' Må inneholde kolonnene `deceased`, `stop_date` og `conclude_status`.
#'
#' @return
#' Datarammen `d`, med en ekstra kolonne `aktiv_behandling` som er
#' `TRUE` eller `FALSE` avhengig av om forløpet er pågående eller avsluttet.
#'
#' @details
#' Et forløp regnes som pågående/aktivt hvis det verken finnes en stoppdato,
#' registrerte opplysninger om at pasienten er død,
#' eller et ferdigstilt tilhørende avslutningsskjema.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hent_skjema("ventreg") %>%
#'   legg_til_pasientid(mceid) %>%
#'   legg_til_pasientinfo(patient_id) %>%
#'   legg_til_stoppinfo(mceid) %>%
#'   legg_til_aktiv_behandling()
#' }
legg_til_aktiv_behandling = function(d) {
  d %>%
    mutate(
      aktiv_behandling = (is.na(deceased) | deceased == 0) & is.na(stop_date) &
        (is.na(conclude_status) | conclude_status != 1)
    )
}
