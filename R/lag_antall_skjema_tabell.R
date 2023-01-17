#' @import dplyr
NULL
#' Lag HTML-tabell med oversikt over ferdige og uferdige skjema på sjukehusnivå
#'
#' @description
#' Funksjonen tek inn ein dato `fra` og ein dato `til`,
#' og gjev ut ein HTML-tabell med talet på ferdige og uferdige skjema på
#' sjukehusnivå for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @param fra
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta frå og med denne datoen vert inkludert.
#' @param til
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta til og med denne datoen vert inkludert.
#'
#' @return
#' HTML-tabell med talet på ferdige og uferdige skjema på sjukehusnivå
#' for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#' @export
#'
#' @examples
#' lag_antall_skjema_tabell(Sys.Date() - 365, Sys.Date())
lag_antall_skjema_tabell = function(fra, til) {
  hent_skjema("SkjemaOversikt") %>%
    aggreger_antall_skjema_tabell(fra, til) %>%
    formater_antall_skjema_tabell()
}

#' Lag dataramme med oversikt over ferdige og uferdige skjema på sjukehusnivå
#'
#' @description
#' Funksjonen tek inn ei dataramme `d_skjemaoversikt`,
#' ein dato `fra` og ein dato `til`,
#' og gjev ut ein dataramme med talet på ferdige og uferdige skjema på
#' sjukehusnivå for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @param d_skjemaoversikt
#' Skjemaet «SkjemaOversikt» frå LTMV-databasen, som ei dataramme.
#' Eventuelt filtrert på t.d. sjukehus.
#' @param fra
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta frå og med denne datoen vert inkludert.
#' @param til
#' Dato, eller eventuelt tekst på formatet "yyyy-mm-dd".
#' Skjema oppretta til og med denne datoen vert inkludert.
#'
#' @return
#' Dataramme med talet på ferdige og uferdige skjema på sjukehusnivå
#' for registrerings-, avslutnings- og ulike oppfylgjingsskjema.
#'
#' @keywords internal
#'
#' @examples
#' ltmv:::aggreger_antall_skjema_tabell(Sys.Date() - 365, Sys.Date())
aggreger_antall_skjema_tabell = function(d_skjemaoversikt, fra, til) {
  d_skjemaoversikt %>%
    filter(
      opprettetdato >= !!fra,
      opprettetdato <= !!til
    ) %>%
    grupper_skjemaoversikt() %>%
    count(skjema_gruppe, sykehusnavn) %>%
    tidyr::pivot_wider(
      names_from = skjema_gruppe,
      values_from = n,
      values_fill = 0,
      names_expand = TRUE
    )
}

#' Formater antall skjema-tabell
#'
#' @description
#' Tek inn ei dataramme med tal på ferdige og uferdige skjema,
#' frå [aggreger_antall_skjema_tabell()],
#' og gjev den ut som HTML-tabell klar til å visast i Rapporteket.
#'
#' @param d_antall_skjema
#' Dataramme frå [aggreger_antall_skjema_tabell()].
#'
#' @return
#' `d_antall_skjema` formatert som HTML-tabell,
#' med samlande overskrifter, hover-effekt og så vidare.
#'
#' @keywords internal
#'
#' @examples
#' d_antall_skjema = ltmv:::aggreger_antall_skjema_tabell(
#'   Sys.Date() - 365, Sys.Date()
#' )
#' ltmv:::formater_antall_skjema_tabell(d_antall_skjema)
formater_antall_skjema_tabell = function(d_antall_skjema) {
  d_antall_skjema %>%
    knitr::kable("html", col.names = NULL) %>%
    kableExtra::add_header_above(
      header = c("", rep(c("Ferdig", "Uferdig"), 5)),
      color = "grey",
      font_size = 12,
      align = "r"
    ) %>%
    kableExtra::add_header_above(
      header = c(
        "", "Registrering år 0" = 2,
        "Oppfølging år 1" = 2, "Oppfølging år 3" = 2,
        "Videre oppfølging\n(år 5+ og AdHoc)" = 2, "Avslutning" = 2
      )
    ) %>%
    kableExtra::column_spec(seq(3, 11, 2), color = "red") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
}

#' Grupper skjemaoversikt
#'
#' @description
#' Grupperer skjemaoversikt i kategoriar alt etter om skjemaa er
#' ferdige eller uferdige,
#' og slår saman ad hoc oppfylgjing og oppfylgjingskjema frå og med år 5
#' til kategorien «Videre oppfølging».
#'
#' @param d_skjemaoversikt
#' Skjemaet «SkjemaOversikt» frå LTMV-databasen, som ei dataramme.
#'
#' @details
#' Tak inn skjemaet «SkjemaOversikt» frå LTMV-databasen,
#' som kan hentast med `hent_skjema("SkjemaOversikt")`,
#' og legg til to nye kolonner, `skjema_gruppe_nr` og `skjema_gruppe`.
#'
#' `skjema_gruppe` er lik `skjemanavn` for alle skjema utanom oppfylgjing
#' frå og med år 5, og ad hoc oppfylgjing.
#' Desse vert slått saman og får verdien `Videre oppfølging (år 5+ og AdHoc)`
#' i den nye kolonnen `skjema_gruppe`.
#' I tillegg vil det for uferdige skjema leggjast på "(uferdig)" på slutten
#' av verdiane i `skjema_gruppe` for uferdige skjema.
#'
#' `skjema_gruppe_nr` tek utgangspunkt i kolonnen `skjemarekkeflg`,
#' som er eit unikt tal for kvar skjematype,
#' der verdien svarar til oppfylgjingsår.
#' Ad hoc oppfylgjing har i `skjemarekkeflg` verdiar på 500 og noko,
#' og avslutningsskjema verdien 999.
#' `skjema_gruppe_nr` nyttar same verdiane som `skjemarekkeflg`,
#' utanom oppfylgjing frå og med år 5 og ad hoc oppfylgjing,
#' som no får verdien 99.
#' I tillegg skiljer `skjema_gruppe_nr` på ferdige og uferdige skjema,
#' i motsetnad til `skjemarekkeflg`,
#' ved å leggja til 0.5 til verdien.
#' I `skjema_gruppe_nr` tek altså ferdige skjema heiltalsverdiar,
#' medan uferdige skjema har tilsvarande verdiar,
#' men med .5 lagt til.
#'
#' Dette er ein intern funksjon som i hovudsak nyttast til å laga
#' tabelloversikt over ferdige og uferdige skjema.
#'
#' @return
#' Dataramma `d_skjemaoversikt` med to ekstra kolonner,
#' skjema_gruppe_nr og skjema_gruppe.
#'
#' @keywords internal
#'
#' @examples
#' d_skjemaoversikt_gruppert = hent_skjema("SkjemaOversikt") %>%
#'   ltmv:::grupper_skjemaoversikt()
#'
#' d_skjemaoversikt_gruppert %>%
#'   head(20) %>%
#'   dplyr::select(skjemanavn, skjemastatus, skjema_gruppe_nr, skjema_gruppe)
grupper_skjemaoversikt = function(d_skjemaoversikt) {
  d_skjema_grupper = tibble::tibble(
    skjema_gruppe_nr = c(0, 0.5, 1, 1.5, 3, 3.5, 99, 99.5, 999, 999.5),
    skjema_gruppe = forcats::fct_inorder(c(
      "Registrering år 0", "Registrering år 0 (uferdig)",
      "Oppfølging år 1", "Oppfølging år 1 (uferdig)",
      "Oppfølging år 3", "Oppfølging år 3 (uferdig)",
      "Videre oppfølging (år 5+ og AdHoc)",
      "Videre oppfølging (år 5+ og AdHoc) (uferdig)",
      "Avslutning", "Avslutning (uferdig)"
    ))
  )

  d_skjemaoversikt %>%
    mutate(
      # Sett skjema_gruppe_nr til 99 for "Videre oppfølging (år 5+ og AdHoc)"
      skjema_gruppe_nr = if_else(skjemarekkeflg %in% c(0, 1, 3, 999),
        true = skjemarekkeflg,
        false = 99
      ),
      # Legg til 0.5 til skjema_gruppe_nr for uferdige skjema
      skjema_gruppe_nr = if_else(skjemastatus != 1,
        true = skjema_gruppe_nr + 0.5,
        false = skjema_gruppe_nr
      )
    ) %>%
    left_join(d_skjema_grupper, by = "skjema_gruppe_nr")
}
